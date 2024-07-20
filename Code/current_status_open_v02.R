# Preamble ---------------------------------------------------------------------
rm(list = ls())

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

load("trades/all_orders_cache.Rdata")

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)
orders <- orders[PAIR != "PLAUSD"]

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# Update cache
last_n_orders <- get_n_hist_orders(n = 200)
idx <- which(!last_n_orders$order_id%in%all_orders_cache$order_id)
all_orders_cache <- rbind(last_n_orders[idx, ], all_orders_cache, fill = T)
save(all_orders_cache, file = "trades/all_orders_cache.Rdata")


df <- all_orders_cache[status == "closed"]
key <- c("order_id", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]

# i <- 1
# trades_raw <- list()
# namen <- list()
# dfs <- list()
# while (offset <= 1000) {
#    
#   trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
#   trades_raw <- lapply(trades_raw[[i]]$result$closed, function(x){x$descr <- NULL;x})
#   namen[[i]] <- names(trades_raw)
#   dfs[[i]] <- rbindlist(trades_raw, fill = T)
#   offset <- offset + 50
#   i <- i +1  
#   Sys.sleep(3)
#   print(offset)
# }

# df <- rbindlist(dfs, fill =T)
# df[, ids := unlist(namen)]
# df <- df[status == "closed"]
# key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
# df <- df[, .(ids, opentm, closetm, vol, vol_exec, cost, fee, price)]
# df <- rbind(df_hist, df[ids %in% df$ids[!df$ids %in% df_hist$ids],])
# save(df, file ="trades/trades.Rdata")


df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")
orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "order_id_BUY",all.x =T) 
df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "order_id_SELL",all.x =T) 
orders_upd1[, ':='(opentm_BUY = anytime(as.numeric(as.character(opentm_BUY))),
                  closetm_BUY = anytime(as.numeric(as.character(closetm_BUY))),
                  opentm_SELL = anytime(as.numeric(as.character(opentm_SELL))),
                  closetm_SELL = anytime(as.numeric(as.character(closetm_SELL))))]
orders_upd1_closed <- orders_upd1[STATUS_BUY == "CLOSED" & STATUS_SELL != "CLOSED"]
orders_upd1_closed[, ':='(cost_BUY = as.numeric(cost_BUY),
                        fee_BUY = as.numeric(fee_BUY))][, cost_BUY_clean := cost_BUY + fee_BUY]


url <- paste0("https://api.kraken.com/0/public/Ticker")
tb <- jsonlite::fromJSON(url)
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
setnames(price_info, "PRICE", "current_price")


orders_upd1_closed <- merge(orders_upd1_closed, price_info, by.x = "PAIR", by.y ="PAIR", all.x = T) 
orders_upd1_closed[, current_cost:= current_price*as.numeric(vol_exec_BUY)]
orders_upd1_closed[, result_per := (current_cost - cost_BUY_clean)/cost_BUY_clean*100]
orders_upd1_closed$vol_exec_BUY <-as.numeric(orders_upd1_closed$vol_exec_BUY)

orders_upd1_closed[, target_sell_clean := (vol_exec_BUY * PRICE_EXIT)-0.0020*(vol_exec_BUY * PRICE_EXIT)]

f <- orders_upd1_closed[, list(quote_res = sum(current_cost - cost_BUY_clean, na.rm =T),
                               avg_per = mean(result_per, na.rm =T),
                               current_cost=sum(current_cost, na.rm =T),
                               cost_BUY_clean = sum(cost_BUY_clean, na.rm =T),
                               target_sell = sum(target_sell_clean, na.rm =T),
                               VOL= sum(vol_exec_BUY, na.rm =T)), by = PAIR]

print(sum(f$quote_res))
print(mean(f$avg_per))

setorder(f, -quote_res)
f$PAIR <- factor(f$PAIR, levels = f$PAIR)
f[quote_res < 0, colour := "negative"]
f[quote_res > 0, colour := "positive"]
pp <- ggplot(data = f, aes(x = PAIR, y = quote_res, fill = colour))+
  geom_col()+theme_light()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values= c("red", "darkgreen"))
f[, target_profit := target_sell - cost_BUY_clean]
f[, current_eval := current_cost - cost_BUY_clean]
f[, amount_to_target := target_profit - current_eval]
View(f)
print(pp)



gg <- orders_upd1_closed[, list(pair = PAIR,
  quote_res = current_cost - cost_BUY_clean,
                               avg_per = result_per,
                               current_cost = current_cost,
                               cost_BUY_clean = cost_BUY_clean,
                               target_sell = target_sell_clean,
                               VOL= vol_exec_BUY)]

gg[, total:=.N, by=pair]
gg[quote_res<0, total_negative:=.N, by=pair]

needs_hedge <- unique(gg[!is.na(total_negative), .(pair, total, total_negative)])
needs_hedge[, negative_percent := total_negative/total*100]
needs_hedge <- merge(needs_hedge, f[, .(PAIR, current_cost)], all.x =T, by.x = "pair", by.y = "PAIR")
needs_hedge <- merge(needs_hedge, orders[STATUS_BUY=="OPEN", .(n_open=.N), by=PAIR], by.x="pair", by.y="PAIR", all.x=T)
needs_hedge[is.na(n_open), n_open:=0]
hedge_pairs <- needs_hedge[negative_percent ==100 & n_open ==0]
setorder(hedge_pairs, -current_cost)


save(hedge_pairs, file="Data/hedge_pairs.RData")
# sum(f$target_sell)-sum(f$current_cost)
# 
# 
# f <- f[PAIR != "KINUSD"]
# library(RColorBrewer)
# pdf(paste0("Alerts", "/open_orders.pdf"), onefile = TRUE)
# pairs <- unique(f$PAIR)
# for(i in 1:length(pairs)){
#   test <- orders_upd1_closed[PAIR == pairs[i]]
#   test[, interval := floor_date(as.POSIXct(closetm_BUY), unit = "2 hours")]
#   cols <- data.table(PRICE_EXIT = unique(test$PRICE_EXIT), colour = brewer.pal(n = 10, name = "Paired")[1:length(unique(test$PRICE_EXIT))])
#   test <- merge(test, cols, by= "PRICE_EXIT", all.x = T)
#   df <- simple_OHLC(interval = 240, pair = pairs[i])
#   setnames(df, "Date_POSIXct", "interval")
#   df$interval <-  as.POSIXct(df$interval)
#   p<- candles(data =df[interval >= min(test$interval)-days(5) & interval <= max(test$interval)+days(5)])+
#     geom_point(data =test, aes(x = interval, y = PRICE_ENTER),colour = test$colour, size = 3)+
#     geom_hline(yintercept = test$PRICE_EXIT, colour = test$colour)
#   print(p)
#   Sys.sleep(1.5)
# }
# dev.off()
