# Preamble ---------------------------------------------------------------------
rm(list = ls())
init <- 1836
options(scipen = 999)

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



df <- all_orders_cache[status == "closed"]
key <- c("order_id", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]


# FIx market trades
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), ORDER_SELL_ID := substr(MESSAGE, 1, 19)]
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), STATUS_SELL := "CLOSED"]

df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")
orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "order_id_BUY",all.x =T) 

df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "order_id_SELL",all.x =T) 


orders_upd1[, opentm_BUY := anytime(as.numeric(as.character(opentm_BUY)))]
orders_upd1[, closetm_BUY := anytime(as.numeric(as.character(closetm_BUY)))]
orders_upd1[, opentm_SELL := anytime(as.numeric(as.character(opentm_SELL)))]
orders_upd1[, closetm_SELL := anytime(as.numeric(as.character(closetm_SELL)))]

orders_upd1_closed <- orders_upd1[STATUS_BUY == "CLOSED" & STATUS_SELL == "CLOSED"]


orders_upd1_closed[, cost_BUY := as.numeric(cost_BUY)]
orders_upd1_closed[, cost_SELL := as.numeric(cost_SELL)]




orders_upd1_closed[, fee_BUY := as.numeric(fee_BUY)]
orders_upd1_closed[, fee_SELL := as.numeric(fee_SELL)]
orders_upd1_closed[, vol_SELL := as.numeric(vol_SELL)]
orders_upd1_closed[, vol_exec_SELL := as.numeric(vol_exec_SELL)]


orders_upd1_closed[ORDER_SELL_ID == "OVUGIC-RSCXD-ZSLL4T", cost_SELL := cost_SELL/5]
orders_upd1_closed[ORDER_SELL_ID == "OVUGIC-RSCXD-ZSLL4T", fee_SELL := fee_SELL/5]
orders_upd1_closed[ORDER_SELL_ID == "OVUGIC-RSCXD-ZSLL4T", vol_SELL := as.numeric(vol_SELL)/5]
orders_upd1_closed[ORDER_SELL_ID == "OVUGIC-RSCXD-ZSLL4T", vol_exec_SELL := as.numeric(vol_exec_SELL)/5]

orders_upd1_closed[ORDER_SELL_ID == "OGVK4K-4IIXZ-46H743", cost_SELL := cost_SELL/3]
orders_upd1_closed[ORDER_SELL_ID == "OGVK4K-4IIXZ-46H743", fee_SELL := fee_SELL/3]
orders_upd1_closed[ORDER_SELL_ID == "OGVK4K-4IIXZ-46H743", vol_SELL := as.numeric(vol_SELL)/3]
orders_upd1_closed[ORDER_SELL_ID == "OGVK4K-4IIXZ-46H743", vol_exec_SELL := as.numeric(vol_exec_SELL)/3]

orders_upd1_closed[ORDER_SELL_ID == "OO5SQH-LTDK5-K5ZH3I", cost_SELL := cost_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "OO5SQH-LTDK5-K5ZH3I", fee_SELL := fee_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "OO5SQH-LTDK5-K5ZH3I", vol_SELL := as.numeric(vol_SELL)/2]
orders_upd1_closed[ORDER_SELL_ID == "OO5SQH-LTDK5-K5ZH3I", vol_exec_SELL := as.numeric(vol_exec_SELL)/2]


orders_upd1_closed[ORDER_SELL_ID == "O73RHC-OZDJP-XLZ5VM", cost_SELL := cost_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "O73RHC-OZDJP-XLZ5VM", fee_SELL := fee_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "O73RHC-OZDJP-XLZ5VM", vol_SELL := as.numeric(vol_SELL)/2]
orders_upd1_closed[ORDER_SELL_ID == "O73RHC-OZDJP-XLZ5VM", vol_exec_SELL := as.numeric(vol_exec_SELL)/2]

orders_upd1_closed[ORDER_SELL_ID == "OHMNGE-DR3K4-IGGHJS", cost_SELL := cost_SELL/8]
orders_upd1_closed[ORDER_SELL_ID == "OHMNGE-DR3K4-IGGHJS", fee_SELL := fee_SELL/8]
orders_upd1_closed[ORDER_SELL_ID == "OHMNGE-DR3K4-IGGHJS", vol_SELL := as.numeric(vol_SELL)/8]
orders_upd1_closed[ORDER_SELL_ID == "OHMNGE-DR3K4-IGGHJS", vol_exec_SELL := as.numeric(vol_exec_SELL)/8]


orders_upd1_closed[ORDER_SELL_ID == "OPQ7TM-O4GSL-UUV5IV", cost_SELL := cost_SELL/5]
orders_upd1_closed[ORDER_SELL_ID == "OPQ7TM-O4GSL-UUV5IV", fee_SELL := fee_SELL/5]
orders_upd1_closed[ORDER_SELL_ID == "OPQ7TM-O4GSL-UUV5IV", vol_SELL := as.numeric(vol_SELL)/5]
orders_upd1_closed[ORDER_SELL_ID == "OPQ7TM-O4GSL-UUV5IV", vol_exec_SELL := as.numeric(vol_exec_SELL)/5]

orders_upd1_closed[ORDER_SELL_ID == "OWQNKB-QOTIP-REQX47", cost_SELL := cost_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "OWQNKB-QOTIP-REQX47", fee_SELL := fee_SELL/2]
orders_upd1_closed[ORDER_SELL_ID == "OWQNKB-QOTIP-REQX47", vol_SELL := as.numeric(vol_SELL)/2]
orders_upd1_closed[ORDER_SELL_ID == "OWQNKB-QOTIP-REQX47", vol_exec_SELL := as.numeric(vol_exec_SELL)/2]


orders_upd1_closed[ORDER_SELL_ID == "OCB4FI-HF2V2-2DZRO3", cost_SELL := cost_SELL/8]
orders_upd1_closed[ORDER_SELL_ID == "OCB4FI-HF2V2-2DZRO3", fee_SELL := fee_SELL/8]
orders_upd1_closed[ORDER_SELL_ID == "OCB4FI-HF2V2-2DZRO3", vol_SELL := as.numeric(vol_SELL)/8]
orders_upd1_closed[ORDER_SELL_ID == "OCB4FI-HF2V2-2DZRO3", vol_exec_SELL := as.numeric(vol_exec_SELL)/8]

orders_upd1_closed[ORDER_SELL_ID == "OMNKM3-BCOYZ-VAOMD5", cost_SELL := cost_SELL/4]
orders_upd1_closed[ORDER_SELL_ID == "OMNKM3-BCOYZ-VAOMD5", fee_SELL := fee_SELL/4]
orders_upd1_closed[ORDER_SELL_ID == "OMNKM3-BCOYZ-VAOMD5", vol_SELL := as.numeric(vol_SELL)/4]
orders_upd1_closed[ORDER_SELL_ID == "OMNKM3-BCOYZ-VAOMD5", vol_exec_SELL := as.numeric(vol_exec_SELL)/4]



orders_upd1_closed[, cost_BUY_clean := cost_BUY + fee_BUY]

orders_upd1_closed[, cost_SELL_clean := cost_SELL - fee_SELL]
orders_upd1_closed[, quote_result_clean := cost_SELL_clean - cost_BUY_clean]
orders_upd1_closed[, percent_result_clean := ((cost_SELL_clean - cost_BUY_clean)/cost_BUY_clean) *100]

closed_trades <- copy(orders_upd1_closed)
save(closed_trades, file=paste0(getwd(), "/code/Investor_report/data/closed_trades.Rdata"))

eq <- copy(orders_upd1_closed)
eq <- eq[!is.na(percent_result_clean)]
eq[, closetm_SELL := substr(closetm_SELL, 1, 10)]
eq[, closetm_SELL := as.Date(closetm_SELL)]

wins_overall <- eq[, list(sum_risked=sum(cost_BUY_clean),sum_earned=sum(quote_result_clean) )]
wins_overall[, percent := sum_earned/sum_risked*100]

# wins_overall
# round(wins_overall$sum_earned/init*100,2)


wins_all <- eq[, list(sum_risked_USD=round(sum(cost_BUY_clean), 3),sum_earned_USD=round(sum(quote_result_clean),3),n_trades=.N ), by = c("closetm_SELL")]
wins_all[, percent_earned := round(sum_earned_USD/sum_risked_USD*100, 2)]
wins_all[, average_win_USD := round(sum_earned_USD/n_trades,2)]
setorder(wins_all,closetm_SELL)
round(sum(wins_all$sum_risked_USD),1)


wins_only_pair <- eq[, list(returns_quote=round(sum(quote_result_clean), 3),
                            bet_quote = round(sum(cost_BUY_clean), 3)), by = c("PAIR")]
wins_only_pair[, returns_per := round(returns_quote/bet_quote*100, 2)]
quote_equity <- eq[,.(PAIR, closetm_SELL,cost_BUY_clean, quote_result_clean)]
quote_equity <- quote_equity[, .(quote_result_clean = sum(quote_result_clean),
                                 quote_per_clean = sum(quote_result_clean)/sum(cost_BUY_clean)
                                 ), by = closetm_SELL]


colnames(quote_equity)[2] <- "quote_result_clean"
setorder(quote_equity,closetm_SELL)
quote_equity[, cumul := cumsum(quote_result_clean)]

p1 <- ggplot(data=quote_equity, aes(x= closetm_SELL, y = cumul))+
  geom_line(colour = "green", size =1)+
  geom_point(colour = "darkgreen", size = 2,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("Cumulative earnings in USD")+xlab("")+
  ggtitle("Cumulative equity in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = round(seq(0, max(quote_equity$cumul),  5)),
                     sec.axis = sec_axis( trans=~./init*100, name="% Gain"))
  
print(p1)

print(wins_overall)
print(round(wins_overall$sum_earned/init*100,2))
print(tail(quote_equity,6))

tmp <- copy(orders_upd1_closed)
tmp[, closetm_SELL := as.Date(substr(closetm_SELL, 1, 10))]
print("some trades did closed properly")
print(tmp[is.na(percent_result_clean), PAIR])
tmp <- tmp[!is.na(percent_result_clean)]

tmp <- tmp[, sum(quote_result_clean), by = c("closetm_SELL", "PAIR")]
setorder(tmp, -closetm_SELL, -V1)
tmp[, daily_sum := sum(V1), by = closetm_SELL]

daily_trading_sums <- copy(tmp)
save(daily_trading_sums, file=paste0(getwd(), "/code/Investor_report/data/daily_trading_sums.Rdata"))

# Not properly closed



dat <- unique(tmp[, .(closetm_SELL, daily_sum)])
setorder(dat, closetm_SELL)
dat$ema_10 <- EMA(dat$daily_sum, 30)
pp<- ggplot(data = dat, aes(x = closetm_SELL, y = daily_sum))+
  geom_point(colour = "green")+
  geom_smooth(span = 0.1, se = F)+
  geom_line(aes(x=closetm_SELL, y = ema_10 ))+
  geom_hline(yintercept = mean(dat$daily_sum), colour = "red")+
  dark_theme_gray()+
  scale_y_continuous(breaks = seq(0, max(dat$daily_sum), 2))
print(pp)
View(tmp)

