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

load("trades/trades.Rdata")
df_hist <- copy(df)
rm(df)

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# url = "https://api.kraken.com/0/private/OpenOrders"
# tt<- get_trade_history(url, key, secret, offset)
# length(tt$result$open)
# Decide number of historical trades to pull
ntrades <- nrow(orders[(STATUS_BUY %in%c("CANCELLED","CLOSED"))])+
  nrow(orders[STATUS_SELL %in% c("CLOSED", "CANCELLED")])
ntrades <- ceiling(ntrades/1000)*1000
ntrades <- 1000
print(paste0("number of trades needed: ", ntrades))

i <- 1
trades_raw <- list()
while (offset <= ntrades) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}

i <-1

dfs <- list()
namen <- list()
for(i in 1:length(trades_raw)){

  for(j in 1:length(trades_raw[[i]]$result$closed)){
    trades_raw[[i]]$result$closed[[j]]$refid <- NULL
    trades_raw[[i]]$result$closed[[j]]$userref <- NULL
    trades_raw[[i]]$result$closed[[j]]$expiretm <- NULL
    trades_raw[[i]]$result$closed[[j]]$descr <- NULL
    trades_raw[[i]]$result$closed[[j]]$misc <- NULL
    trades_raw[[i]]$result$closed[[j]]$oflags <- NULL
    trades_raw[[i]]$result$closed[[j]]$reason <- NULL
    trades_raw[[i]]$result$closed[[j]]$trigger <- NULL
    trades_raw[[i]]$result$closed[[j]]$reduce_only <- NULL  
  }  
  

  dfs[[i]] <- as.data.frame(do.call(rbind, trades_raw[[i]]$result$closed))
  namen[[i]] <- names(trades_raw[[i]]$result$closed)
}
df <- rbindlist(dfs, fill =T)
df$ids <- unlist(namen)
setDT(df)


df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]

df <- rbind(df_hist, df[ids %in% df$ids[!df$ids %in% df_hist$ids],])

save(df, file ="trades/trades.Rdata")

df$opentm <- unlist(df$opentm)
df$closetm <- unlist(df$closetm)
df$vol <- unlist(df$vol)
df$vol_exec <- unlist(df$vol_exec)
df$cost <- unlist(df$cost)
df$fee <- unlist(df$fee)
df$price <- unlist(df$price)


# FIx market trades
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), ORDER_SELL_ID := substr(MESSAGE, 1, 19)]
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), STATUS_SELL := "CLOSED"]


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


# Percent win or total risked

# orders_upd1_closed[, sum(cost_BUY_clean), by = PAIR]


# View(orders_upd1_closed[, sum(quote_result_clean), by = PAIR])


eq <- copy(orders_upd1_closed)
eq <- eq[!is.na(percent_result_clean)]
eq[, closetm_SELL := substr(closetm_SELL, 1, 10)]
eq[, closetm_SELL := as.Date(closetm_SELL)]

# table(eq$quote_result_clean > 0)


wins_overall <- eq[, list(sum_risked=sum(cost_BUY_clean),sum_earned=sum(quote_result_clean) )]
wins_overall[, percent := sum_earned/sum_risked*100]

# wins_overall
# round(wins_overall$sum_earned/init*100,2)


wins_all <- eq[, list(sum_risked_USD=round(sum(cost_BUY_clean), 3),sum_earned_USD=round(sum(quote_result_clean),3),n_trades=.N ), by = c("closetm_SELL")]
wins_all[, percent_earned := round(sum_earned_USD/sum_risked_USD*100, 2)]
wins_all[, average_win_USD := round(sum_earned_USD/n_trades,2)]
setorder(wins_all,closetm_SELL)
round(sum(wins_all$sum_risked_USD),1)

p1 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = sum_risked_USD))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Amount risked in USD")+xlab("")+
  ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())
p1
# 
p2 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = percent_earned))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Percent earned from risked amount")+xlab("")+
  ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())
p2
# 
p3 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = sum_risked_USD))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Amount risked in USD(green)\n Percent earned (white)")+xlab("")+
  ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())+
  geom_line(aes(x= closetm_SELL, y = percent_earned))+
  scale_y_continuous(breaks = round(seq(0, max(wins_all$sum_risked_USD), by = 5)))
p3
# 
risked <- wins_all[, .(closetm_SELL, sum_risked_USD)]
risked[, flag:="risked"]
colnames(risked)[2] <- "sum"
earned <- wins_all[, .(closetm_SELL, sum_earned_USD)]
earned[, flag:="earned"]
colnames(earned)[2] <- "sum"
risked_earned <- rbind(risked, earned)
risked_earned[, Per:=round(sum[flag=="earned"]/sum[flag=="risked"]*100,2), by = closetm_SELL]
risked_earned[flag == "risked", Per := 100-Per]
risked_earned[,closetm_SELL := as.Date(closetm_SELL)]

p4 <- ggplot(risked_earned, aes(x = closetm_SELL, y = sum, fill = flag)) +
  geom_col()+dark_theme_gray()+ylab("")+
  ggtitle("Risked vs Earned in USD")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks = round(seq(0, 100, by = 10)))+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")
p4
# 
p5 <- ggplot(risked_earned, aes(x = closetm_SELL, y = Per, fill = flag)) +
  geom_col()+dark_theme_gray()+ylab("")+
  ggtitle("Risked vs Earned in %")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks = round(seq(0, 100, by = 10)))+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")
p5
# 
# 
# 

wins_only_pair <- eq[, list(returns_quote=round(sum(quote_result_clean), 3),
                            bet_quote = round(sum(cost_BUY_clean), 3)), by = c("PAIR")]
wins_only_pair[, returns_per := round(returns_quote/bet_quote*100, 2)]
# wins_only_pair[, average_win_USD := round(sum_earned_USD/n_trades,2)]
# setorder(wins_only_pair,-percent_earned)

p6 <- ggplot(wins_only_pair, aes(x = reorder(PAIR, -returns_per), y = returns_per)) +
  geom_col(colour = "green")+dark_theme_gray()+ylab("")+
  ggtitle("Risked vs Earned in %")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #theme(panel.grid.minor = element_blank())+
  theme(legend.position = "bottom")
p6
p111<- ggplot(data = wins_only_pair, aes(x= returns_quote,
                                  y=returns_per))+
  geom_label(aes(label = PAIR), colour = 'green')+
  # geom_point(colour = "green")+
  dark_theme_gray()+
  geom_hline(yintercept = max(wins_only_pair$returns_per)/2)+
  geom_vline(xintercept = max(wins_only_pair$returns_quote)/2)

print(p111)
quote_equity <- eq[,.(PAIR, closetm_SELL,cost_BUY_clean, quote_result_clean)]
quote_equity <- quote_equity[, .(quote_result_clean = sum(quote_result_clean),
                                 quote_per_clean = sum(quote_result_clean)/sum(cost_BUY_clean)
                                 ), by = closetm_SELL]
# > DT[,.(V4.Sum=sum(V4)), by=.(V1,V2)] 
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
# eq_per <- copy(orders_upd1_closed)
# eq_per[, closetm_SELL := substr(closetm_SELL, 1, 10)]
# eq_per[, closetm_SELL := as.Date(closetm_SELL)]
# eq_per <- eq_per[,.(PAIR, closetm_SELL, cost_BUY_clean, quote_result_clean)]
# eq_per <- eq_per[, list(quote = sum(quote_result_clean), bet = sum(cost_BUY_clean)), by = closetm_SELL]
# setorder(eq_per,closetm_SELL)
# eq_per[, cumul_result := cumsum(quote)]
# eq_per[, cumul_bet := cumsum(bet)]
# eq_per[, per_res :=  (cumul_result/cumul_bet)*100]
# 
# p2 <- ggplot(data=eq_per, aes(x= closetm_SELL, y = per_res))+
#   geom_line(colour = "green")+
#   geom_point(colour = "green")+
#   dark_theme_gray()+ylab("Daily % ")+xlab("")+
#   ggtitle("Win/Bet percentage")+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(axis.text.x = element_blank())+
#   theme(panel.grid.minor = element_blank())+
#   scale_y_continuous(breaks = round(seq(0, max(eq_per$per_res), by = 0.1)))
# p2
# 
# # Number of trades
# ntrades <- copy(orders_upd1_closed)
# ntrades[, closetm_SELL := substr(closetm_SELL, 1, 10)]
# ntrades[, closetm_SELL := as.Date(closetm_SELL)]
# ntrades <- ntrades[,.(PAIR, closetm_SELL)]
# ntrades <- ntrades[, list(ntrades=.N), by = closetm_SELL]
# setorder(ntrades,closetm_SELL)
# ntrades <- ntrades[, list(ntrades = sum(ntrades)), by = closetm_SELL]
# 
# p3 <- ggplot(data=ntrades, aes(x= closetm_SELL, y = ntrades))+
#   geom_line(colour = "green")+
#   geom_point(colour = "green")+
#   dark_theme_gray()+ylab("Daily #")+xlab("Date")+
#   ggtitle("Number of trades")+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(panel.grid.minor = element_blank())
# 
# 
# 
# duration <- copy(orders_upd1_closed)
# duration[, closetm_SELL := substr(closetm_SELL, 1, 10)]
# duration[, closetm_SELL := as.Date(closetm_SELL)]
# duration[, closetm_BUY := substr(closetm_BUY, 1, 10)]
# duration[, closetm_BUY := as.Date(closetm_BUY)]
# 
# duration <- duration[,.(PAIR, closetm_BUY,closetm_SELL)]
# duration <- duration[, mean(closetm_SELL-closetm_BUY), by = PAIR]
# setorder(duration,-V1)
# 
# duration$PAIR <- factor(duration$PAIR, levels = duration$PAIR)
# 
# p4 <- ggplot(data=duration, aes(x =PAIR, y = V1 ))+
#   geom_col(colour = "yellow", fill = "darkgreen")+
#   dark_theme_gray()+
#   ylab("Days")+xlab("Pair")+
#   ggtitle("Average duration of a trade by pair")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(panel.grid.minor = element_blank())+
#   geom_hline(yintercept = mean(duration$V1), colour = "red")
# 
# # Number of trades
# ntrades <- copy(orders_upd1_closed)
# ntrades[, closetm_SELL := substr(closetm_SELL, 1, 10)]
# ntrades[, closetm_SELL := as.Date(closetm_SELL)]
# ntrades <- ntrades[,.(PAIR, closetm_SELL)]
# ntrades <- ntrades[, list(ntrades=.N), by = PAIR]
# setorder(ntrades,-ntrades)
# 
# ntrades$PAIR <- factor(ntrades$PAIR, levels = ntrades$PAIR)
# 
# p5 <- ggplot(data=ntrades, aes(x =PAIR, y = ntrades ))+
#   geom_col(colour = "yellow", fill = "darkgreen")+
#   dark_theme_gray()+
#   ylab("# trades")+xlab("Pair")+
#   ggtitle("Total number of trades by pair")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(panel.grid.minor = element_blank())+
#   geom_hline(yintercept = mean(ntrades$ntrades), colour = "red")
# 
# 
# p6 <- ggarrange(p1, p2, p3, nrow= 3)
# p6
# # p6
# p7 <- ggarrange(p4, p5, nrow= 2)
# ggarrange(p6,p7, ncol = 2)
# 
# 
# # 
# crypto_holdings <- orders_upd1[STATUS_BUY == "CLOSED" & STATUS_SELL != "CLOSED"]
# crypto_holdings[, vol_exec_BUY:= as.numeric(vol_exec_BUY)]
# View(crypto_holdings[, sum(vol_exec_BUY), by = PAIR])
# # 
# 
tmp <- copy(orders_upd1_closed)
tmp[, closetm_SELL := as.Date(substr(closetm_SELL, 1, 10))]
tmp <- tmp[, sum(quote_result_clean), by = c("closetm_SELL", "PAIR")]
setorder(tmp, -closetm_SELL, -V1)
tmp[, daily_sum := sum(V1), by = closetm_SELL]
tmp <- tmp[!is.na(closetm_SELL)]
# pp<-ggplot(data = unique(tmp[, .(closetm_SELL, daily_sum)]), aes(x = closetm_SELL, y = daily_sum))+
#   geom_bar(stat = "identity", colour = "black")+
#   geom_hline(yintercept = unique(tmp[, .(closetm_SELL, daily_sum)])[, mean(daily_sum)], colour = "red")
# print(pp)
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

# 
# dat[, cumsum := cumsum(daily_sum)]
# ggplot(data = dat, aes(x = cumsum, y = daily_sum))+
#   geom_line(colour = "black")+
#   geom_smooth(method = "lm")
# fit <- lm(daily_sum~cumsum, data =dat)
# newdata = data.frame(cumsum = seq(1200, 5000, by= 50))
# dat1 <- rbind(dat, data.frame(cumsum=newdata,  daily_sum = predict(fit, newdata = newdata)), fill =T)
# 
# ggplot(data = dat1, aes(x = cumsum, y = daily_sum))+
#   geom_point(colour = "black")+
#   geom_smooth(method = "lm")
