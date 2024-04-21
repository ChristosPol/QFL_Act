# ------------------------------------------------------------------------------
rm(list = ls())
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
interval <- 60
budget <- current_avail_funds();budget
n_pairs_avail <- round(budget/40);n_pairs_avail
minimum <- T
use_existing_price_levels <- F
each_usd <- 5
n_orders <- 8 
grid <- c(0.025,0.05,0.075,0.1, 0.125, 0.15, 0.2, 0.25)
csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)
open_orders <- orders[STATUS_SELL == "OPEN"]
possible_enter <- orders[STATUS_BUY == "OPEN"][, .N, by= PAIR]
setorder(possible_enter, N)
pairs <- possible_enter[1:n_pairs_avail, ]

# ------------------------------------------------------------------------------
# Get all pairs
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]

if(any(!all_pairs$PAIR %in% all_pairs_stored)){
  new_pair <- all_pairs$PAIR[!all_pairs$PAIR %in% all_pairs_stored]
  print("Consider adding new pair to potential pair list")
}


out <- c("AUDUSD", "DAIUSD", "ETHPYUSD", "ETHWUSD", "EURTUSD",
        "PAXGUSD", "PYTHUSD", "PYUSDUSD", "TBTCUSD", "TUSDUSD", "USDCUSD", "USDTZUSD",
        "USTUSD", "WBTCUSD", "XBTPYUSD", "ZEURZUSD", "ZGBPZUSD",
        "LUNA2USD", "LUNAUSD", "MSOLUSD","AUDUSD","DAIUSD","MSOLUSD",
        "TBTCUSD","USDCUSD","USDTZUSD","WBTCUSD","WAXLUSD",
        "ZEURZUSD","ZGBPZUSD","EURTUSD","LUNA2USD",
        "PAXGUSD","TRIBE", "ZEURZUSD", "ZGBPZUSD", "ETHPYUSD")
all_pairs <- all_pairs[!PAIR %in% out]


# Get last price
url <- paste0("https://api.kraken.com/0/public/Ticker")
tb <- jsonlite::fromJSON(url)
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
all_pairs <- merge(all_pairs,price_info, by = "PAIR", all.x = T)

# Get minimal order
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
min_deci_info <- data.table(PAIR = names(tb$result),
                         DECIMALS = lapply(tb$result, "[[", "pair_decimals"),
                         MIN = lapply(tb$result, "[[", "ordermin"))
all_pairs <- merge(all_pairs,min_deci_info, by = "PAIR", all.x = T)

minimums_calculated <- copy(all_pairs)
setnames(minimums_calculated, "PAIR", "COIN")
minimums_calculated$DECIMALS <- unlist(minimums_calculated$DECIMALS)
minimums_calculated$MIN <- unlist(minimums_calculated$MIN)

fwrite(minimums_calculated, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/minimums_calculated.csv"))


rand <- sample(unique(all_pairs$PAIR), n_pairs_avail)
rand1 <- c("ACAUSD","ACHUSD","ADXUSD","AGLDUSD","ALCXUSD","ALPHAUSD","ANTUSD",
          "API3USD","ARPAUSD","AUDIOUSD","BLURUSD","BONDUSD","BONKUSD","BRICKUSD",
          "C98USD","CFGUSD","CTSIUSD","DYMUSD","EGLDUSD","ENSUSD","EULUSD",
          "FARMUSD","FETUSD","FIDAUSD","FISUSD","FLRUSD","GALUSD","GARIUSD",
          "GMTUSD","HDXUSD","HNTUSD","ICXUSD","IMXUSD","KAVAUSD","KILTUSD",
          "KINUSD","KP3RUSD","LPTUSD","LSKUSD","MCUSD","MNGOUSD","NMRUSD","NODLUSD","OGNUSD"
          ,"PEPEUSD","POLSUSD","POWRUSD","RADUSD","RBCUSD","RENUSD","REPV2USD"
          ,"RLCUSD","ROOKUSD","SCRTUSD","SDNUSD","SGBUSD","STGUSD"
          ,"STORJUSD","SUIUSD","SUPERUSD","SYNUSD","TUSDUSD")
rand <- pairs$PAIR
# rand1 <- pairs$PAIR
# rand <- rand[!rand %in%rand1]
all_pairs <- all_pairs[PAIR %in% rand]


trading_table <- data.frame(PAIR = rep(all_pairs$PAIR, each = n_orders),
                            VOL = unlist(rep(all_pairs$MIN, each = n_orders)),
                            DECIMAL= unlist(rep(all_pairs$DECIMALS, each = n_orders)))
pairs <- unique(trading_table$PAIR)

i <- 110
supports <- list()
for (i in 1:length(pairs)){
  msg <- tryCatch({
    df <- simple_OHLC(interval = interval, pair = pairs[i])
    df$week <- isoweek(as.Date(df$Date_POSIXct))
    df$weekday <- weekdays(as.Date(df$Date_POSIXct))
    weeks <- as.character(unique(df$week))
    weeks <- weeks[!weeks %in%names(which.min(table(df$week)))]
    
    SP <- c()
    sp_test <- list()
    rs_test <- list()
    RS <- c()
    j <-1
    # for(j in 1:length(unique(splits[, seq]))){
    for(j in 1:length(weeks)){
      # subdf <- df[splits[seq == j, idx], ]
      subdf <- df[week == weeks[j], ]
      SP[j] <- median(head(sort(subdf[, close]), 5))
      
      
      sp_test[[j]] <- c(min(head(sort(subdf[, close]), 5)), max(head(sort(subdf[, close]), 5)))
      rs_test[[j]] <- c(min(tail(sort(subdf[, close]), 5)), max(tail(sort(subdf[, close]), 5)))
      RS[j] <- median(tail(sort(subdf[, close]), 5))
    }
    
  }, error = function(e){
  })
  
  SP <- SP[SP<tail(df$close, 1)]
  needed <- n_orders-length(SP)
  
  if(needed == n_orders){
    grid_set <- tail(df$close, 1) - tail(df$close, 1)*grid
    SP <- grid_set
  } else{
    lowest <- min(SP)
    grid_selected <- grid[1:needed]
    grid_set <- lowest - lowest*grid_selected
    SP <- c(SP, grid_set)  
  }
  
  print(paste0("Pair ", pairs[i], " needed ", needed, " orders" ))
  # print(paste0("Sharpe Ratio for: ",  EUR_pairs[i]," ", round(unique(df$sharpe), 4))) 
  Sys.sleep(1.2)
  supports[[i]] <- sort(SP, decreasing = T)
}

idx_rem <- which(unlist(lapply(supports, length))<8)
pairs_rem <- pairs[which(unlist(lapply(supports, length))<8)]
if(length(idx_rem) > 0) {
  supports <- supports[-idx_rem]   
}

setDT(trading_table)
trading_table <- trading_table[!PAIR %in% pairs_rem]

trading_table[, PRICE_ENTER := unlist(supports)]
trading_table[, ORDER_BUY_ID:=NA]
trading_table[, STATUS_BUY:=NA]
trading_table[, index := 1:.N, by=PAIR]
minimums_calculated[, MIN := as.numeric(MIN) ]

if(minimum){
  trading_table[, VOL := round(each_usd/PRICE_ENTER, DECIMAL)]
  trading_table <- merge(trading_table, minimums_calculated, by.x = "PAIR", by.y = "COIN", all.x = T)
  trading_table[VOL<MIN, VOL := MIN]
}

# Define exit points depending on open orders
open_orders_sel <- open_orders[PAIR %in% trading_table$PAIR]
exit_points <- unique(open_orders_sel[, .(PAIR, PRICE_EXIT)])
exit_points <- exit_points[, list(PRICE_EXIT = max(PRICE_EXIT)), by ="PAIR"]
# For the pairs that already have an open position then simply give the same exit points
# the one is already there
if(use_existing_price_levels){
  trading_table <- merge(trading_table, exit_points, all.x= T, by = "PAIR")  
} else {
  trading_table[, PRICE_EXIT := as.numeric()]
}

# For the ones that enter for the first time, just 1%
trading_table[index == 1 & is.na(PRICE_EXIT), PRICE_EXIT := PRICE_ENTER + PRICE_ENTER*0.01]
trading_table$PRICE_EXIT <- na.locf(trading_table$PRICE_EXIT)

trading_table$index <- NULL
trading_table[,ORDER_SELL_ID := NA]
trading_table[,STATUS_SELL := NA]

trading_table[, PRICE_ENTER := round(PRICE_ENTER, DECIMAL)]
trading_table[, PRICE_EXIT := round(PRICE_EXIT, DECIMAL)]
trading_table[, DECIMAL := NULL]
trading_table[, BET_ENTER := as.numeric(VOL)*PRICE_ENTER]


cumul <- trading_table[, list(SUM_BET = sum(BET_ENTER)), by = PAIR]
setorder(cumul, SUM_BET)# maybe change this
cumul[, CUM_BET := cumsum(SUM_BET)]
cumul <- cumul[CUM_BET < budget]
trading_table <- trading_table[PAIR %in% cumul$PAIR]


trading_table$CUR <- NULL
trading_table$PRICE <- NULL
trading_table$DECIMALS <- NULL
trading_table$MIN <- NULL


fwrite(trading_table, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table_BIND.csv"))


