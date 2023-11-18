# Preamble ---------------------------------------------------------------------
rm(list = ls())

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

load("trades/trades.Rdata")
df_hist <- copy(df)
rm(df)
key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

i <- 1
trades_raw <- list()
namen <- list()
dfs <- list()
while (offset <= 1000) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  trades_raw <- lapply(trades_raw[[i]]$result$closed, function(x){x$descr <- NULL;x})
  namen[[i]] <- names(trades_raw)
  dfs[[i]] <- rbindlist(trades_raw, fill = T)
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}

df <- rbindlist(dfs, fill =T)
df[, ids := unlist(namen)]
df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, .(ids, opentm, closetm, vol, vol_exec, cost, fee, price)]


df <- rbind(df_hist, df[ids %in% df$ids[!df$ids %in% df_hist$ids],])

save(df, file ="trades/trades.Rdata")


df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")
orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "ids_BUY",all.x =T) 
df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "ids_SELL",all.x =T) 
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

orders_upd1_closed[, target_sell_clean := (vol_exec_BUY * PRICE_EXIT)-0.0016*(vol_exec_BUY * PRICE_EXIT)]

f <- orders_upd1_closed[, list(quote_res = sum(current_cost - cost_BUY_clean),
                               avg_per = mean(result_per),
                               current_cost=sum(current_cost),
                               cost_BUY_clean = sum(cost_BUY_clean),
                               target_sell = sum(target_sell_clean),
                               VOL= sum(vol_exec_BUY)), by = PAIR]

print(sum(f$quote_res))
print(mean(f$avg_per))

setorder(f, -quote_res)
f$PAIR <- factor(f$PAIR, levels = f$PAIR)
f[quote_res < 0, colour := "negative"]
f[quote_res > 0, colour := "positive"]
pp <- ggplot(data = f, aes(x = PAIR, y = quote_res, fill = colour))+
  geom_col()+theme_light()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values= c("red", "darkgreen"))
View(f)
print(pp)
sum(f$target_sell)-sum(f$current_cost)

f[, percent_from_buy := (current_cost-cost_BUY_clean)/cost_BUY_clean*100]
f[, percent_from_target := (target_sell)/current_cost*100]
