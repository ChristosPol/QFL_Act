rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

first <- read_csv(grep("csv",list.files("~/Repositories/QFL_Act/Data/trading_sessions/First",
                                        full.names = T),
                       value = T))
second <- read_csv(grep("csv",list.files("~/Repositories/QFL_Act/Data/trading_sessions/Second",
                                        full.names = T),
                       value = T),col_types = cols(MESSAGE = col_character()))

third <- read_csv(grep("csv",list.files("~/Repositories/QFL_Act/Data/trading_sessions/Third",
                                         full.names = T),
                        value = T),col_types = cols(MESSAGE = col_character()))

fourth <- read_csv(grep("csv",list.files("~/Repositories/QFL_Act/Data/trading_sessions/Fourth",
                                        full.names = T),
                       value = T),col_types = cols(MESSAGE = col_character()))


all <- rbind(first, second, third, fourth)
orders <- copy(all)

setDT(orders)
init <- 1150
key = API_Key
secret = API_Sign
# 
offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"
# 
# # url = "https://api.kraken.com/0/private/OpenOrders"
# # tt<- get_trade_history(url, key, secret, offset)
# # length(tt$result$open)
# 
# 
# 
i <- 1
trades_raw <- list()
while (offset <= 19000) {

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

dfs <- list()
namen <- list()
for(i in 1:length(trades_raw)){
  print(i)
  dfs[[i]] <- data.frame(opentm = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "opentm")),
                         closetm = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "closetm")),
                         vol = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "vol")),
                         vol_exec = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "vol_exec")),
                         cost = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "cost")),
                         fee = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "fee")),
                         price = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "price")),
                         status = unlist(lapply(trades_raw[[i]]$result$closed, "[[", "status")))
  namen[[i]] <- names(trades_raw[[i]]$result$closed)
  
}

df <- rbindlist(dfs, fill =T)
str(df)
setDT(df)
df$ids <- unlist(namen)

df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]

# Check for duplicated order ids and sum


# FIx market trades
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), ORDER_SELL_ID := substr(MESSAGE, 1, 19)]
orders[STATUS_SELL == "CANCELLED" & !is.na(MESSAGE), STATUS_SELL := "CLOSED"]

df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")

orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "ids_BUY",all.x =T) 
df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "ids_SELL",all.x =T) 
orders_upd1[!is.na(ORDER_SELL_ID), duplicated(ORDER_SELL_ID)]


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
orders_upd1_closed$cost_SELL
orders_upd1_closed[, cost_SELL_clean := cost_SELL - fee_SELL]
orders_upd1_closed[, quote_result_clean := cost_SELL_clean - cost_BUY_clean]
orders_upd1_closed[, percent_result_clean := ((cost_SELL_clean - cost_BUY_clean)/cost_BUY_clean) *100]


# Percent win or total risked
all_trades <- copy(orders_upd1_closed[!is.na(quote_result_clean)])


# Table for markdown
summary <- data.table(Column = c("Start", "End", "Initial funds available (USD)",
                      "Returns in USD", "Returns in %", "N trades",
                      "Wins", "Losses", "Win ratio", "Average win %",
                      "Biggest win %", "Biggest loss %", "Days trading",
                      "Average n trades per day"),
           Value = c(as.character(min(all_trades$closetm_SELL)), as.character(max(all_trades$closetm_SELL)), 1150,
                     sum(all_trades$quote_result_clean), round(sum(all_trades$quote_result_clean)/1150*100,1), nrow(all_trades),
                     nrow(all_trades[quote_result_clean>0]), nrow(all_trades[quote_result_clean<0]),
                     round(nrow(all_trades[quote_result_clean>0])/nrow(all_trades)*100,1), round(mean(all_trades$percent_result_clean),1),
                     round(max(all_trades$percent_result_clean),1),round(min(all_trades$percent_result_clean),1),
                     round(max(all_trades$closetm_SELL) - min(all_trades$closetm_SELL),0),
                     nrow(all_trades)/as.numeric(round(max(all_trades$closetm_SELL) - min(all_trades$closetm_SELL),0))))


# orders_upd1_closed[, sum(cost_BUY_clean), by = PAIR]
load("Data/evaluation/all_trades.rdata")
save(all_trades, file = "Data/evaluation/all_trades.rdata")

eq <- copy(orders_upd1_closed[!is.na(quote_result_clean)])
eq[, closetm_SELL := substr(closetm_SELL, 1, 10)]
eq[, closetm_SELL := as.Date(closetm_SELL)]


wins_overall <- eq[, list(sum_risked=sum(cost_BUY_clean),sum_earned=sum(quote_result_clean) )]
wins_overall[, percent := sum_earned/sum_risked*100]

wins_overall
round(wins_overall$sum_earned/init*100,2)


# wins_all <- eq[, list(sum_risked_USD=round(sum(cost_BUY_clean), 3),sum_earned_USD=round(sum(quote_result_clean),3),n_trades=.N ), by = c("closetm_SELL")]
# wins_all[, percent_earned := round(sum_earned_USD/sum_risked_USD*100, 2)]
# wins_all[, average_win_USD := round(sum_earned_USD/n_trades,2)]
# setorder(wins_all,closetm_SELL)
# round(sum(wins_all$sum_risked_USD),1)
# 
# p1 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = sum_risked_USD))+
#   geom_line(colour = "green")+
#   geom_point(colour = "green")+
#   dark_theme_gray()+ylab("Amount risked in USD")+xlab("")+
#   ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   # theme(axis.text.x = element_blank())+
#   theme(panel.grid.minor = element_blank())
# p1
# 
# p2 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = percent_earned))+
#   geom_line(colour = "green")+
#   geom_point(colour = "green")+
#   dark_theme_gray()+ylab("Percent earned from risked amount")+xlab("")+
#   ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   # theme(axis.text.x = element_blank())+
#   theme(panel.grid.minor = element_blank())
# p2
# 
# p3 <- ggplot(data=wins_all, aes(x= closetm_SELL, y = sum_risked_USD))+
#   geom_line(colour = "green")+
#   geom_point(colour = "green")+
#   dark_theme_gray()+ylab("Amount risked in USD(green)\n Percent earned (white)")+xlab("")+
#   ggtitle(paste0("Total risked amount in USD:", round(sum(wins_all$sum_risked_USD),2)))+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   # theme(axis.text.x = element_blank())+
#   theme(panel.grid.minor = element_blank())+
#   geom_line(aes(x= closetm_SELL, y = percent_earned))+
#   scale_y_continuous(breaks = round(seq(0, max(wins_all$sum_risked_USD), by = 5)))
# p3
# 
# risked <- wins_all[, .(closetm_SELL, sum_risked_USD)]
# risked[, flag:="risked"]
# colnames(risked)[2] <- "sum"
# earned <- wins_all[, .(closetm_SELL, sum_earned_USD)]
# earned[, flag:="earned"]
# colnames(earned)[2] <- "sum"
# risked_earned <- rbind(risked, earned)
# risked_earned[, Per:=round(sum[flag=="earned"]/sum[flag=="risked"]*100,2), by = closetm_SELL]
# risked_earned[flag == "risked", Per := 100-Per]
# risked_earned[,closetm_SELL := as.Date(closetm_SELL)]
# 
# p4 <- ggplot(risked_earned, aes(x = closetm_SELL, y = sum, fill = flag)) +
#   geom_col()+dark_theme_gray()+ylab("")+
#   ggtitle("Risked vs Earned in USD")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(panel.grid.minor = element_blank())+
#   theme(legend.position = "bottom")+
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10)))+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")
# p4
# 
# p5 <- ggplot(risked_earned, aes(x = closetm_SELL, y = Per, fill = flag)) +
#   geom_col()+dark_theme_gray()+ylab("")+
#   ggtitle("Risked vs Earned in %")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(panel.grid.minor = element_blank())+
#   theme(legend.position = "bottom")+
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10)))+
#   scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")
# p5
# 
# 
# 
# wins_only_pair <- eq[, list(sum_risked_USD=round(sum(cost_BUY_clean), 3),sum_earned_USD=round(sum(quote_result_clean),3),n_trades=.N ), by = c("PAIR")]
# wins_only_pair[, percent_earned := round(sum_earned_USD/sum_risked_USD*100, 2)]
# wins_only_pair[, average_win_USD := round(sum_earned_USD/n_trades,2)]
# setorder(wins_only_pair,-percent_earned)
# wins_only_pair$sum_earned_USD
# 
# p6 <- ggplot(wins_only_pair, aes(x = reorder(PAIR, -sum_earned_USD), y = sum_earned_USD)) +
#   geom_col(colour = "green")+dark_theme_gray()+ylab("")+
#   ggtitle("Risked vs Earned in %")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   #theme(panel.grid.minor = element_blank())+
#   theme(legend.position = "bottom")
# p6




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
  # geom_point(colour = "darkgreen", size = 0.2,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("")+xlab("")+
  ggtitle("Daily cumulative returns in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="30 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = round(seq(0, max(quote_equity$cumul)+50,  50)),
                     sec.axis = sec_axis( trans=~./init*100, name="% Gain",breaks = round(seq(0, 135,  10))))

p1
ggsave(filename = "Data/evaluation/dce.png", height = 5, width =10)
save(quote_equity, file="Data/evaluation/quote_equity.Rdata")

rm(list=ls())
load(file="Data/evaluation/quote_equity.Rdata")
sd = min(quote_equity$closetm_SELL)
ed = max(quote_equity$closetm_SELL)

calendar <- data.table(calendar_time= seq(sd, ed, "days"), flag = 1)
quote_equity <- merge(calendar, quote_equity, by.x = "calendar_time", by.y = "closetm_SELL", all.x = T)
quote_equity$cumul <- na.locf(quote_equity$cumul)
quote_equity[is.na(quote_result_clean), quote_result_clean := 0]
quote_equity[is.na(quote_per_clean), quote_per_clean := 0]



init <- 1150
p1 <- ggplot(data=quote_equity, aes(x= calendar_time, y = cumul))+
  geom_line(colour = "green", size =1)+
  geom_point(colour = "darkgreen", size = 0.2,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("Cumulative earnings in USD")+xlab("")+
  ggtitle("Cumulative equity in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="30 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = round(seq(0, max(quote_equity$cumul)+10,  10)),
                     sec.axis = sec_axis( trans=~./init*100, name="% Gain"))


p1

# plot(quote_equity$nro, quote_equity$cumul)
# fit <- lm(quote_equity$cumul~ quote_equity$nro)
# abline(fit)
# summary(fit)
# fut <- data.table(nro = 1:3000)
# diff(predict(fit, newdata=fut))
# 
# quote_equity[, slope := 2.551419]
# quote_equity[, intercept := 53.39218]
# quote_equity[, fitt:=fitted(fit)]
# quote_equity[, cumul_percent := 1150+cumsum(quote_result_clean)]
# quote_equity[, precent := quote_result_clean/cumul_percent*100]



# plot(quote_equity$nro, quote_equity$quote_result_clean)
# fit <- lm(quote_equity$quote_result_clean~quote_equity$nro)
# abline(fit)
# 
# 
# plot(1:10000 ,1:10000*unique(quote_equity$slope)-unique(quote_equity$intercept), type = "l")
# points(quote_equity$nro, quote_equity$cumul, type = "l", col= "red")
# 
# crypto_holdings <- orders_upd1[STATUS_BUY == "CLOSED" & STATUS_SELL != "CLOSED"]
# crypto_holdings[, vol_exec_BUY:= as.numeric(vol_exec_BUY)]
# leftovers <- crypto_holdings[, sum(vol_exec_BUY), by = PAIR]

# Get last price
# url <- paste0("https://api.kraken.com/0/public/Ticker")
# tb <- jsonlite::fromJSON(url)
# price_info <- data.table(PAIR = names(tb$result),
#                          PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
# leftovers <- merge(leftovers,price_info, by = "PAIR", all.x = T)
# leftovers[, VALUE := V1*PRICE]
# sum(leftovers$VALUE)+259.018149
# 329/1150


# Trading days
# days <- tail(quote_equity$closetm_SELL, 1)-head(quote_equity$closetm_SELL, 1)
# nrow(orders_upd1_closed)/as.numeric(days)#7.5 trades (buy sell)
# (sum(leftovers$VALUE)+259.018149)/as.numeric(days)#7.5 trades (buy sell)
# 
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
library(stringr)
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]

mah_list  <- list()
for (i in 1:length(all_pairs$PAIR)){
  
  df <- simple_OHLC(interval = 1440, pair = all_pairs$PAIR[i])
  df[, Date_POSIXct := as.Date(Date_POSIXct)]
  df <- df[Date_POSIXct >= "2022-10-13"]
  df[, diff := c(0, diff(close))]
  df[, cum_diff := cumsum(diff)]
  df[, cum_diff_per := cum_diff/close[1]*100]
  df[, PAIR := all_pairs$PAIR[i]]
  mah_list[[i]]<- df
  Sys.sleep(0.5)
  print(i)
}

dfdf <- rbindlist(mah_list)
colnames(dfdf)
dfdf <- dfdf[, .(PAIR, Date_POSIXct,cum_diff_per)]

# load("quote_equity.Rdata")
# load("quote_equity_lastest.Rdata")
# quote <- rbind(quote_equity, quote_equity_lastest)
# quote[, cumul := NULL]
# setorder(quote, closetm_SELL)
# quote[, quote_per_clean := NULL]
# quote[, cumul := cumsum(quote_result_clean)]
quote_equity[, PAIR := "CP"]
quote_equity[,cum_diff_per := cumul/1150*100]
setnames(quote_equity, c("calendar_time"), c("Date_POSIXct"))
quote_equity$cumul <- NULL
quote_equity$quote_result_clean <- NULL

df1 <- rbind(dfdf, quote_equity[, .(PAIR ,Date_POSIXct ,cum_diff_per)])
alpha <- copy(df1)

save(alpha, file = "Data/evaluation/alpha.Rdata")
# df2 <- df1[Date_POSIXct <= df1[PAIR == "CP", max(Date_POSIXct)]]
# unique(df2$PAIR)[i]
load(file = "Data/evaluation/alpha.Rdata")
ggplot(data=alpha, aes(x = Date_POSIXct, y= cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2) + theme(legend.position = "none")+
  theme_bw()

status <- alpha[Date_POSIXct == max(Date_POSIXct)]
setorder(status, -cum_diff_per)
quantile(status$cum_diff_per, probs = .82)


load("Data/evaluation/alpha.Rdata")
ggplot(data = alpha, aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2)+
  geom_line(data=alpha[PAIR == "CP"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "white")+
  dark_theme_gray()+
  coord_cartesian(ylim = c(-200, 700))+
  ylab("Percentage cumulative returns")+
  xlab("Date")+
  ggtitle("Strategy returns vs single asset portfolio, currently at 0.82 quantile")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="90 day")+
  theme(legend.position = "none")
ggsave(filename = "Data/evaluation/alpha.png", height = 5, width =10)

# 
# 
# path_alerts <- "Alerts"
# pdf(paste0(path_alerts, "/Alpha_latest.pdf"), onefile = TRUE)
# i <- 1
# for (i in 1:length(unique(df2$PAIR))){
#   
#   p<- ggplot(data = df2[PAIR %in% c(unique(df2$PAIR)[i], "CP")], aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
#     geom_line()+dark_theme_gray()+theme(legend.position = "bottom")
#   print(p)
#   Sys.sleep(0.2)
# }
# dev.off()
# 
# ggplot(data = df2, aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
#   geom_line()+dark_theme_gray()+theme(legend.position = "bottom")
