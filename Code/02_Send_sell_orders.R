# Preamble ---------------------------------------------------------------------
rm(list = ls())

print(paste0("#1 Script initiated at: ",Sys.time()))

# Source functions
path_source <- "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


csv_path <- paste0("/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/trading_table.csv")
orders <- read_csv(csv_path,col_types = cols())
print("#2 Order file loaded")




load("trades/all_orders_cache.Rdata")

key = API_Key
secret = API_Sign

print("#3 API keys defined")

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"


# Update cache
last_n_orders <- get_n_hist_orders(n = 2000)
idx <- which(!last_n_orders$order_id%in%all_orders_cache$order_id)
all_orders_cache <- rbind(last_n_orders[idx, ], all_orders_cache, fill = T)
save(all_orders_cache, file = "trades/all_orders_cache.Rdata")


closed <- all_orders_cache[status == "closed"]
key <- c("order_id", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
closed <- closed[, ..key]

print(paste0("#8 Sending sell orders if buy order closed"))
i <- 15221

for(i in 1:nrow(orders)){
  # print(i)
  msg <- tryCatch({
  if(orders$STATUS_BUY[i] == "OPEN" & orders$ORDER_BUY_ID[i] %in% closed$order_id){
    orders$STATUS_BUY[i] <- "CLOSED"
    
    sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                        key = API_Key, secret = API_Sign, pair = orders$PAIR[i], type = "sell",
                        ordertype = "limit", volume = orders$VOL[i], price = orders$PRICE_EXIT[i])
    print(sell_it)
    
    if(length(sell_it$error) ==1){
      orders$FAIL[i] <- sell_it$error
      print(sell_it$error)
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
fwrite(orders, file = paste0("/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/trading_table.csv"))

print(paste0("Process finished at: ",Sys.time()))

