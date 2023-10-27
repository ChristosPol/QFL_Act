# Preamble ---------------------------------------------------------------------
rm(list = ls())

portfolio <- xlsx::read.xlsx("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_sessions/Investing/Invested.xlsx",
                             sheetName = "Sheet2")

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"


i <- 1
trades_raw <- list()
while (offset <= 8000) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}

dfs <- list()
namen <- list()
for(i in 1:length(trades_raw)){
  print(i)
  dfs[[i]] <- as.data.frame(do.call(rbind, trades_raw[[i]]$result$closed))
  namen[[i]] <- names(trades_raw[[i]]$result$closed)
}
df <- rbindlist(dfs, fill =T)
df$ids <- unlist(namen)
setDT(df)


df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]

portfolio <- merge(portfolio, df, by.x = "ORDER", by.y = "ids", all.x = T)
setDT(portfolio)
portfolio[, closetm := anytime(as.numeric(as.character(closetm)))]


# Get last price
url <- paste0("https://api.kraken.com/0/public/Ticker")
tb <- jsonlite::fromJSON(url)
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
setnames(price_info, "PRICE", "CURRENT_PRICE")
price_info[, CURRENCY := str_sub(PAIR,start = -3)]
price_info <- price_info[CURRENCY == "USD"]
price_info[, PAIR := sub("USD", "", PAIR)]
price_info[PAIR == "XXBTZ", PAIR := "BTC"]
price_info[PAIR == "XETHZ", PAIR := "ETH"]
price_info[PAIR == "XLTCZ", PAIR := "LTC"]
price_info[PAIR == "XXBTZ", PAIR := "BTC"]
price_info[PAIR == "LUNA", PAIR := "LUNC"]
price_info[PAIR == "XXRPZ", PAIR := "XRP"]

portfolio <- merge(portfolio, price_info, by.x = "COIN", by.y ="PAIR", all.x = T)
portfolio[, CURRENT_COST := CURRENT_PRICE*as.numeric(vol_exec)]
portfolio[, sum(as.numeric(unlist(vol_exec))*as.numeric(unlist(price)))]# invested 479
portfolio[, sum(CURRENT_COST)]# current 302, 314
portfolio[, .(COIN, cost, CURRENT_COST)]
View(portfolio)
