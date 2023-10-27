# Notes
# Need to adjust how to send trades
# Option 1
# Send grid as it is calculated, dynamic number of trades, add if needed below 5% from the lowest - Bullish
# Option 2
# Bearish - Get lowest point and add manually -5%, -10% -15%, -20%
# Option 3
# Get a very high time frame (240 minutes) and send all orders - Safer for crushes

# Very important! Fix decimals for orders!

# Part 1
rm(list = ls())
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
minimum <- FALSE
# each_usd <- 10
n_orders <- 8 
grid <- c(0.025,0.05,0.075,0.1, 0.125, 0.15, 0.2, 0.25)
csv_path <- paste0("Data/minimums_calculated.csv")
minimums <- read_csv(csv_path)
setDT(minimums)

# csv_path <- paste0("Data/trading_table.csv")
# orders <- read_csv(csv_path)
# setDT(orders)
# orders[STATUS_BUY == "OPEN", unique(PAIR)]

trade <-c("ACAUSD", "ALICEUSD", "AUDIOUSD", "CVCUSD", "FIDAUSD", "KEYUSD", 
          "MNGOUSD", "PONDUSD", "RUNEUSD")


minimums <- minimums[COIN %in% trade]


trading_table <- data.frame(PAIR = rep(minimums$COIN, each = n_orders),
                            VOL = rep(minimums$MIN, each = n_orders),
                            DECIMAL= rep(minimums$DECIMALS, each = n_orders))
interval <- 60

pairs <- unique(trading_table$PAIR)
i <- 5
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
setDT(trading_table)
trading_table[, PRICE_ENTER := unlist(supports)]
trading_table[, ORDER_BUY_ID:=NA]
trading_table[, STATUS_BUY:=NA]
trading_table[, index := 1:.N, by=PAIR]

if(minimum){
  trading_table[, VOL := round(each_usd/PRICE_ENTER, DECIMAL)]
}


trading_table[index == 1, PRICE_EXIT := PRICE_ENTER + PRICE_ENTER*0.01]

trading_table$PRICE_EXIT <- na.locf(trading_table$PRICE_EXIT)
trading_table$index <- NULL
trading_table[,ORDER_SELL_ID := NA]
trading_table[,STATUS_SELL := NA]

trading_table[, PRICE_ENTER := round(PRICE_ENTER, DECIMAL)]
trading_table[, PRICE_EXIT := round(PRICE_EXIT, DECIMAL)]
# trading_table[, per := (PRICE_EXIT-PRICE_ENTER)/PRICE_ENTER]
# trading_table$per <- NULL
trading_table[, DECIMAL := NULL]
trading_table[, BET_ENTER := VOL*PRICE_ENTER]

cumul <- trading_table[, list(SUM_BET = sum(BET_ENTER)), by = PAIR]
setorder(cumul, SUM_BET)
cumul[, CUM_BET := cumsum(SUM_BET)]

budget <- 340
cumul <- cumul[CUM_BET < budget]

trading_table <- trading_table[PAIR %in% cumul$PAIR]

fwrite(trading_table, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Bot/Data/trading_table_BIND.csv"))
