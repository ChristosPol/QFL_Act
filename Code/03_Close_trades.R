# Preamble ---------------------------------------------------------------------
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)
load("trades/all_orders_cache.Rdata")

key = API_Key
secret = API_Sign
offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# Update cache
last_n_orders <- get_n_hist_orders(n = 100)
idx <- which(!last_n_orders$order_id%in%all_orders_cache$order_id)
all_orders_cache <- rbind(last_n_orders[idx, ], all_orders_cache, fill = T)
save(all_orders_cache, file = "trades/all_orders_cache.Rdata")


for(i in 1:nrow(orders)){
  if(orders$STATUS_SELL[i] == "OPEN" & orders$ORDER_SELL_ID[i] %in% all_orders_cache[status == "closed", order_id]){
    orders$STATUS_SELL[i] <- "CLOSED"
    print(paste0("Position closed for: ", orders$PAIR[i]))
  }
}

orders$row_n <- 1:nrow(orders)
csv_path <- paste0("Data/minimums_calculated.csv")
minimums <- read_csv(csv_path)
setDT(minimums)

orders <- merge(orders, minimums[, .(COIN, MIN)], by.x = "PAIR", by.y = "COIN", all.x = T)
orders$MESSAGE <- as.character(orders$MESSAGE)
setorder(orders, row_n)
orders[, row_n := NULL]
orders[, MIN := NULL]

fwrite(orders, file = paste0("Data/trading_table.csv"))

