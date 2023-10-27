# Preamble ---------------------------------------------------------------------
rm(list = ls())

# Source functions
path_source <- "/Users/christos.polysopoulos/Repositories/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


csv_path <- paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table.csv")
orders <- read_csv(csv_path,col_types = cols())


key = API_Key
secret = API_Sign

for(i in 1:nrow(orders)){
  # print(i)
  msg <- tryCatch({
    if(orders$STATUS_BUY[i] == "OPEN"){
      Sys.sleep(3)
      
      orders$STATUS_BUY[i] <- "CANCELLED"
      print(orders[i, ])
      
      cancel_it <- cancel_order(url = "https://api.kraken.com/0/private/CancelOrder",
                           key = API_Key, secret = API_Sign, txid = orders$ORDER_BUY_ID[i])
      Sys.sleep(3)
    } 
    
  }, error = function(e){
  })
}
fwrite(orders, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Act/Data/trading_table.csv"))
