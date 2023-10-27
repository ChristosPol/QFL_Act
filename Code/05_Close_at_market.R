# Preamble ---------------------------------------------------------------------
rm(list = ls())
options(scipen = 999)
# Source functions
path_source <- "/Users/christos.polysopoulos/Repositories/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


csv_path <- paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table.csv")
orders <- read_csv(csv_path,col_types = cols())



i <- 1270
for (i in 1:nrow(orders)){
  msg <- tryCatch({
    if(orders$STATUS_BUY[i] == "CLOSED" & orders$STATUS_SELL[i] == "CANCELLED" & is.na(orders$MESSAGE[i])){
    
    
    sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                       key=API_Key, secret=API_Sign,
                       pair=orders$PAIR[i], type="sell",
                       ordertype="market", volume=orders$VOL[i])
    Sys.sleep(5)
    
    if(length(sell_it$error) ==1){
      orders$FAIL[i] <- sell_it$error
    } else {
      orders$MESSAGE[i] <- paste0(sell_it$result$txid, " - ", sell_it$result$descr)
      
    }
    
    }
  }, error = function(e){
  })
  
  
  
}

fwrite(orders, file = paste0("Data/trading_table.csv"))


