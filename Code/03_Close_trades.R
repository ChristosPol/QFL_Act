### Create another script that check how far are the orders to enter in
### order to cancel them

# Preamble ---------------------------------------------------------------------
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))
library("rjson")

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

load("trades/trades.Rdata")
df_hist <- copy(df)
rm(df)


key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# Decide number of historical trades to pull
ntrades <- nrow(orders[(STATUS_BUY %in%c("CANCELLED","CLOSED"))])+
  nrow(orders[STATUS_SELL %in% c("CLOSED", "CANCELLED")])
ntrades <- ceiling(ntrades/1000)*1000
ntrades <- 1000

print(paste0("number of trades needed: ", ntrades))
i <- 1
trades_raw <- list()

while (offset <= ntrades) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}

myls <- list()
df_list <- list()
for(k in 1:length(trades_raw)){
  
  for (i in 1:length(trades_raw[[k]]$result$closed)){
    
    final_data <- do.call(rbind, trades_raw[[k]]$result$closed[i])
    myls[[i]] <- as.vector(unlist(final_data[, "status"]))
    names(myls[[i]]) <- rownames(final_data)
    
    
  }
  df_list[[k]] <- unlist(myls)
}

closed <- data.table(ids = names(unlist(df_list)), status = unlist(df_list))
closed <- closed[status =="closed"]

df_hist <- df_hist[, .(ids)][, status := "closed"]
closed <- rbind(df_hist, closed[ids %in% closed$ids[!closed$ids %in% df_hist$ids]])


for(i in 1:nrow(orders)){
  if(orders$STATUS_SELL[i] == "OPEN" & orders$ORDER_SELL_ID[i] %in% closed$ids){
    orders$STATUS_SELL[i] <- "CLOSED"
    print(paste0("Position closed for: ", orders$PAIR[i]))
  }
  # Sys.sleep(0.01)
  # print(paste0("No update for: ", orders$PAIR[i]))
}

orders$row_n <- 1:nrow(orders)
csv_path <- paste0("Data/minimums_calculated.csv")
minimums <- read_csv(csv_path)
setDT(minimums)

orders <- merge(orders, minimums[, .(COIN, MIN)], by.x = "PAIR", by.y = "COIN", all.x = T)
orders$MESSAGE <- as.character(orders$MESSAGE)
setorder(orders, row_n)
orders[, row_n := NULL]

# orders[VOL < MIN & STATUS_BUY == "CLOSED" & STATUS_SELL == "OPEN", MESSAGE := "Minimum vol changed, cannot close order"]
# orders[MESSAGE == "Minimum vol changed, cannot close order", STATUS_SELL := "FAILED"]
orders[, MIN := NULL]
# A third script "Close up all trades" that checks the status_sell

fwrite(orders, file = paste0("Data/trading_table.csv"))

