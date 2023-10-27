# Preamble ---------------------------------------------------------------------
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

csv_path <- paste0("Data/trading_table.csv")
orders <- read_csv(csv_path)

for(i in 1:nrow(orders)){
  msg <- tryCatch({
    if(is.na(orders$STATUS_BUY[i])){
      buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                          key = API_Key, secret = API_Sign, pair = orders$PAIR[i], type = "buy",
                          ordertype = "limit", volume = orders$VOL[i], price = orders$PRICE_ENTER[i])
      
      if(length(buy_it$error) ==1){
        orders$FAIL[i] <- buy_it$error
      } else {
        orders$ORDER_BUY_ID[i] <- buy_it$result$txid
        orders$STATUS_BUY[i] <- "OPEN"
      }
      print(buy_it)
      
      Sys.sleep(3)  
  }
  }, error = function(e){
  })
  print(i)
}
fwrite(orders, file = paste0("Data/trading_table.csv"))

