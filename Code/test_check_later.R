# Part 1
rm(list = ls())
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)


interval <- 60
csv_path <- paste0("Data/min_orders.csv")
minimums <- read_csv(csv_path)
minimums$PRICE <- NA
setDT(minimums)
minimums <- minimums[!COIN %in% c("ETH2.SUSD", "USDUSD", "USDCUSD",
                                  "USDTUSD", "USTUSD", "PAXGUSD",
                                  "GBPUSD", "EURUSD", "DAIUSD", "AUDUSD",
                                  "SAMOUSD", "ETHWUSD")]

i<-38
res <- list()
for (i in 1:length(minimums$COIN)){
  df <- simple_OHLC(interval = interval, pair = minimums$COIN[i])
  df[, open_low_per := round((low - open)/open,3)*100 ]
  df[, close_low_per:= round((close- low)/low,3)*100]
  df <- df[open_low_per < 0]
  
  xx <- sort(df$open_low_per)[1:25]
  
  res[[i]] <- data.table(PAIR = minimums$COIN[i],
             mean_low_per = mean(df[,open_low_per]),
             mean_close_per = mean(df[,close_low_per]),
             high_25_mean_close_per = mean(xx) )
             
  Sys.sleep(1.1)
  print(i/length(minimums$COIN))
}
res1 <- rbindlist(res)
View(res1)
setorder(res1, mean_low_per, mean_close_per)


url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
