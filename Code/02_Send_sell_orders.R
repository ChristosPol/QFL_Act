# Preamble ---------------------------------------------------------------------
rm(list = ls())

print(paste0("#1 Script initiated at: ",Sys.time()))



# Source functions
path_source <- "/Users/christos.polysopoulos/Repositories/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


csv_path <- paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table.csv")
orders <- read_csv(csv_path,col_types = cols())
print("#2 Order file loaded")


key = API_Key
secret = API_Sign
print("#3 API keys defined")



url = "https://api.kraken.com/0/private/ClosedOrders"

print("#4 Getting order history from kraken")
i <- 1
trades_raw <- list()
norder <- 18000

offset <- 0
print(paste0("#5 Obtaining the last ", norder, " trades"))

while (offset <= norder) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  offset <- offset + 50
  i <- i +1  
  # print(paste0("Progress: ", offset/norder, "%"))
  Sys.sleep(3)
}
print(paste0("length of list pulled: ",length(trades_raw)))
print(paste0("#6 Historic trades obtained"))


myls <- list()
df_list <- list()
k <- 1
i <- 1
for(k in 1:length(trades_raw)){
  
  for (i in 1:length(trades_raw[[k]]$result$closed)){
    
    final_data <- do.call(rbind, trades_raw[[k]]$result$closed[i])
    myls[[i]] <- as.vector(unlist(final_data[, "status"]))
    names(myls[[i]]) <- rownames(final_data)
  }
  df_list[[k]] <- unlist(myls)
}
print(paste0("length of list edited: ",length(df_list)))

closed <- data.table(ids = names(unlist(df_list)), status = unlist(df_list))
closed <- closed[status =="closed"]
print(paste0("#7 Closed order IDs defined"))

print(paste0("#8 Sending sell orders if buy order closed"))
i <- 4845

for(i in 1:nrow(orders)){
  # print(i)
  msg <- tryCatch({
  if(orders$STATUS_BUY[i] == "OPEN" & orders$ORDER_BUY_ID[i] %in% closed$ids){
    orders$STATUS_BUY[i] <- "CLOSED"
    
    sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                        key = API_Key, secret = API_Sign, pair = orders$PAIR[i], type = "sell",
                        ordertype = "limit", volume = orders$VOL[i], price = orders$PRICE_EXIT[i])
    
    if(length(sell_it$error) ==1){
      orders$FAIL[i] <- sell_it$error
    } else {
      orders$ORDER_SELL_ID[i] <- sell_it$result$txid
      orders$STATUS_SELL[i] <- "OPEN"
    }
    
    print(paste0("Limit sell order sent for: ", orders$PAIR[i]))
    Sys.sleep(10)
  } 
  
  }, error = function(e){
  })
  
}
# A third script "Close up all trades" that checks the status_sell
fwrite(orders, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table.csv"))

print(paste0("Process finished at: ",Sys.time()))
