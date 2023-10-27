# Need to change to bid and ask

rm(list=ls())
library(stringi)
library(stringr)
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))




url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(pairs = all_pairs, CUR=str_sub(all_pairs,start = -3))
# all_pairs <- all_pairs[CUR%in%c("EUR", "USD", "CAD", "GBP", "JPY", "CHF")]
all_pairs_XBT <- all_pairs[CUR%in%c( "XBT")]
all_pairs_XBT[, rowpos := .I]
all_pairs_XBT[, COIN := gsub(CUR, "", pairs), by = rowpos]
# all_pairs_XBT <- all_pairs_XBT[1:10,]

all_pairs_USD <- all_pairs[CUR%in%c( "USD")]
all_pairs_USD[, rowpos := .I]
all_pairs_USD[, COIN := gsub(CUR, "", pairs), by = rowpos]


all <- merge(all_pairs_XBT, all_pairs_USD[, .(pairs, COIN)], by ="COIN", all.x =T)
all <- all[!is.na(pairs.y)]

# Get last price
repeat{
  all_temp <- copy(all)
  url <- paste0("https://api.kraken.com/0/public/Ticker")
  tb <- jsonlite::fromJSON(url)
  price_info <- data.table(PAIR = names(tb$result),
                           ASK = as.numeric(lapply(lapply(tb$result, "[[", 1), "[", 1)),
                           BID = as.numeric(lapply(lapply(tb$result, "[[", 2), "[", 1)))
  BTCUSD <-price_info[PAIR == "XXBTZUSD", BID]
  
  # price_info <- price_info[PAIR %in% c("RARIUSD", "RARIXBT")]
  
  all_temp <- merge(all_temp, price_info, by.x = "pairs.x", by.y = "PAIR", all.x =T)
  setnames(all_temp, "BID", "BID_BTC")
  setnames(all_temp, "ASK", "ASK_BTC")
  # all_temp <- all_temp[!is.na(BID_BTC)]
  all_temp <- merge(all_temp, price_info, by.x = "pairs.y", by.y = "PAIR", all.x =T)
  setnames(all_temp, "BID", "BID_USD")
  setnames(all_temp, "ASK", "ASK_USD")
  
  First <- 100/all_temp[, ASK_USD] # 44.64285714
  Second <- all_temp[, BID_BTC]*First #
  Third <- BTCUSD*Second
  tt <- data.table(pair = all_temp$pairs.y, per= Third)
  tt[, per_enter := round((per-100)/100 *100,3)]
  print(tt[per_enter>0])
  Sys.sleep(1.1)
  
}



