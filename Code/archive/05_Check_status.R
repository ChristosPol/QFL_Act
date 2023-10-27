### Create another script that check how far are the orders to enter in
### order to cancel them

# Preamble ---------------------------------------------------------------------
rm(list = ls())
options(scipen = 999)
interval <- 1
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))
library("rjson")

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)
key = API_Key
secret = API_Sign

orders <- orders[STATUS_BUY == "CLOSED" & STATUS_SELL == "OPEN"]

pairs <- orders[, unique(PAIR)]
prices <- c()
for (i in 1:length(pairs)){
  df <- simple_OHLC(interval = interval, pair = pairs[i])
  prices[i] <- tail(df$close, 1)
  Sys.sleep(1.5)
  print(i)
}
df <- data.table(price = prices, pair = pairs)

orders <- merge(orders, df, by.x = "PAIR", by.y ="pair", all.x = T) 

orders_red <- orders[, .(PAIR, VOL, PRICE_ENTER,price)]
setnames(orders_red, "price", "CURRENT_PRICE")
orders_red[, TOTAL_ENTER := VOL* PRICE_ENTER]
orders_red[, TOTAL_CURRENT := VOL* CURRENT_PRICE]
orders_red[, RES_USD := TOTAL_CURRENT-TOTAL_ENTER]

orders_red_sum <- orders_red[, list(TOTAL_ENTER = sum(TOTAL_ENTER) ,
                                    TOTAL_CURRENT = sum(TOTAL_CURRENT),
                                    TOTAL_VOL= sum(VOL)) , by = PAIR]
orders_red_sum[, RES_USD := TOTAL_CURRENT-TOTAL_ENTER]
orders_red_sum[, RES_PER := RES_USD/TOTAL_ENTER*100]
sum(orders_red_sum$RES_USD)

View(orders_red_sum)
sum(orders_red_sum$TOTAL_ENTER)
sum(orders_red_sum$TOTAL_CURRENT)
