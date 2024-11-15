rm(list = ls())
library(patchwork)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)


# Compared to market
library(stringr)
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]

mah_list  <- list()
for (i in 1:length(all_pairs$PAIR)){
  
  df <- simple_OHLC(interval = 10080, pair = all_pairs$PAIR[i])
  df[, Date_POSIXct := as.Date(Date_POSIXct)]
  # df <- df[Date_POSIXct >= "2022-11-01"]
  df[, diff := c(0, diff(close))]
  df[, cum_diff := cumsum(diff)]
  df[, cum_diff_per := cum_diff/close[1]*100]
  df[, PAIR := all_pairs$PAIR[i]]
  mah_list[[i]]<- df
  Sys.sleep(1)
  print(i)
}

dfdf <- rbindlist(mah_list)
summar <- dfdf[, list(Mean = mean(diff), Sd = sd(diff)), by = PAIR]
summar[, sharpe := Mean/Sd]
setorder(summar, -sharpe)
summar[, sharpe_rank := 1:.N]

View(summar)

dfdf <- dfdf[, .(PAIR, Date_POSIXct,cum_diff_per)]
