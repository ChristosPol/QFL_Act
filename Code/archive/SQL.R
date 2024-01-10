rm(list=ls())
# library("RODBC")
library("DBI")
library("data.table")
# library("RPostgres")

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# url = "https://api.kraken.com/0/private/OpenOrders"
# tt<- get_trade_history(url, key, secret, offset)
# length(tt$result$open)

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


df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")
orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "ids_BUY",all.x =T) 
df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "ids_SELL",all.x =T) 


orders_upd1[, opentm_BUY := anytime(as.numeric(as.character(opentm_BUY)))]
orders_upd1[, closetm_BUY := anytime(as.numeric(as.character(closetm_BUY)))]
orders_upd1[, opentm_SELL := anytime(as.numeric(as.character(opentm_SELL)))]
orders_upd1[, closetm_SELL := anytime(as.numeric(as.character(closetm_SELL)))]


orders_upd1_closed <- orders_upd1[STATUS_BUY == "CLOSED"]
orders_upd1_closed <- orders_upd1_closed[STATUS_SELL != "CLOSED"]


orders_upd1_closed[, cost_BUY := unlist(as.numeric(cost_BUY))]
orders_upd1_closed[, fee_BUY := unlist(as.numeric(fee_BUY))]
orders_upd1_closed[, vol_BUY := unlist(as.numeric(vol_BUY))]
orders_upd1_closed[, vol_exec_BUY := unlist(as.numeric(vol_exec_BUY))]
orders_upd1_closed[, price_BUY := unlist(as.numeric(price_BUY))]
orders_upd1_closed[, vol_SELL := unlist(as.numeric(vol_SELL))]

orders_upd1_closed[, cost_BUY_clean := cost_BUY + fee_BUY]


# Here inject SQL

con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname="postgres",
                 port = 5432, 
                 user= "postgres",
                 password = "poliso89")

dbListTables(con) # See tables

dbCreateTable(con, "trades_test", orders_upd1_closed)

dbAppendTable(con, "trades_test", orders_upd1_closed)


dbDisconnect(con)


db_table <- as.data.table(dbReadTable(con, "accounts"))
str(db_table)
dsd <- data.table()

dsd[ , ':='(user_id = "sdfsd", username = "y2",
              password ="asda" , email = "asdasd",
              created_on="asfda" ,last_login="asdasd")]
rbind(db_table, dsd)
dbDataType(con, db_table)
