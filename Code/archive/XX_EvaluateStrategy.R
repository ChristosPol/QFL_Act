# Preamble ---------------------------------------------------------------------
rm(list = ls())
init <- 375

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

key = API_Key
secret = API_Sign

url = "https://api.kraken.com/0/private/OpenOrders"
openoders <- get_trade_history(url, key, secret, offset=1500)
ids <- names(openoders$result$open)
openoders <- rbindlist(lapply(openoders$result$open, function(x) {data.frame(t(sapply(x,c)))}))
setDT(openoders)

key.sub <- c("opentm", "vol", "vol_exec")
openoders <- openoders[, ..key.sub]
openoders[, ids := ids]
openoders[, opentm := anytime(as.numeric(as.character(opentm)))]
openoders[, vol := unlist(vol)]
openoders[, vol_exec := unlist(vol_exec)]

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

i <- 1
trades_raw <- list()
while (offset <= 3500) {
  
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

# df <- df[status == "closed"]
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

setnames(openoders,"opentm","opentm_OPEN")
openoders[, opentm_OPEN:=as.character(opentm_OPEN)]
orders_upd1 <- merge(orders_upd1, openoders[, .(ids, opentm_OPEN)], by.x = "ORDER_BUY_ID", by.y = "ids", all.x = T)
orders_upd1 <- merge(orders_upd1, openoders[, .(ids, opentm_OPEN)], by.x = "ORDER_SELL_ID", by.y = "ids", all.x = T)
# orders_upd1[!is.na(opentm_OPEN.x), opentm_OPEN := opentm_OPEN.x]
# orders_upd1[is.na(opentm_OPEN.x) & !is.na(opentm_OPEN.y), opentm_OPEN := opentm_OPEN.y]
# orders_upd1[, opentm_OPEN.y := NULL]
# orders_upd1[, opentm_OPEN.x := NULL]
# orders_upd1 <- orders_upd1[STATUS_BUY !="CANCELLED",]
orders_upd1 <- orders_upd1[STATUS_BUY !="FAILED",]
setnames(orders_upd1, c("opentm_OPEN.x", "opentm_OPEN.y"), c("opentm_OPEN.BUY", "opentm_OPEN.SELL"))

# Check filled orders
key <- c("ORDER_SELL_ID", "ORDER_BUY_ID", "PAIR", "STATUS_BUY", "STATUS_SELL", "opentm_BUY","closetm_BUY", "opentm_SELL","closetm_SELL", "opentm_OPEN.BUY", "opentm_OPEN.SELL")
filled_df <- orders_upd1[, ..key]



filled_df1 <- copy(filled_df)
filled_df1[STATUS_SELL=="", STATUS_SELL := "NOT_INITIATED"]

filled_df1[, c("opentm_BUY", "closetm_BUY", "opentm_SELL", "closetm_SELL",
               "opentm_OPEN.BUY", "opentm_OPEN.SELL") := list(as.Date(substr(opentm_BUY, 1, 10)),
                                                              as.Date(substr(closetm_BUY, 1, 10)),
                                                              as.Date(substr(opentm_SELL, 1, 10)),
                                                              as.Date(substr(closetm_SELL, 1, 10)),
                                                              as.Date(substr(opentm_OPEN.BUY, 1, 10)),
                                                              as.Date(substr(opentm_OPEN.SELL, 1, 10)))]

test1 <- filled_df1[STATUS_BUY %in%c("CANCELLED", "CLOSED")]
opened_at_CLOSED <- test1[, .N, by = c("opentm_BUY","STATUS_BUY") ]
setnames(opened_at_CLOSED, "opentm_BUY", "Date")

test2 <- filled_df1[STATUS_BUY %in%c("OPEN")]
opened_at_OPEN <- test2[, .N, by = c("opentm_OPEN.BUY","STATUS_BUY") ]
setnames(opened_at_OPEN, "opentm_OPEN.BUY", "Date")

test3 <- rbind(opened_at_CLOSED, opened_at_OPEN)
test3[, N_SUM := sum(N), by = Date]
test3[, per := round((N/N_SUM)*100, 1)]
test3$Date <- as.character(test3$Date)

p1 <- ggplot(test3, aes(x = Date, y = per, fill = STATUS_BUY)) +
  geom_col()+dark_theme_gray()+ylab("")+
  ggtitle("Status buy in %")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  theme(legend.position = "bottom")
p2 <- ggplot(test3, aes(x = Date, y = N, fill = STATUS_BUY)) +
  geom_col()+dark_theme_gray()+ylab("")+
  ggtitle("Status buy in N")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  theme(legend.position = "bottom")
p3 <- ggarrange(p1, p2, nrow =2)
p3

View(filled_df1)

orders_upd1_closed <- orders_upd1[STATUS_BUY == "CLOSED" & STATUS_SELL == "CLOSED"]

orders_upd1_closed[, cost_BUY := unlist(as.numeric(cost_BUY))]
orders_upd1_closed[, cost_SELL := unlist(as.numeric(cost_SELL))]
orders_upd1_closed[, fee_BUY := unlist(as.numeric(fee_BUY))]
orders_upd1_closed[, fee_SELL := unlist(as.numeric(fee_SELL))]

orders_upd1_closed[, cost_BUY_clean := cost_BUY + fee_BUY]
orders_upd1_closed[, cost_SELL_clean := cost_SELL - fee_SELL]
orders_upd1_closed[, quote_result_clean := cost_SELL_clean - cost_BUY_clean]
orders_upd1_closed[, percent_result_clean := ((cost_SELL_clean - cost_BUY_clean)/cost_BUY_clean) *100]

View(orders_upd1_closed[, sum(quote_result_clean), by = PAIR])

eq <- copy(orders_upd1_closed)
eq[, closetm_SELL := substr(closetm_SELL, 1, 10)]
eq[, closetm_SELL := as.Date(closetm_SELL)]
quote_equity <- eq[,.(PAIR, closetm_SELL,cost_BUY_clean, quote_result_clean)]
quote_equity <- quote_equity[, .(quote_result_clean = sum(quote_result_clean),
                                 quote_per_clean = sum(quote_result_clean)/sum(cost_BUY_clean)
), by = closetm_SELL]
# > DT[,.(V4.Sum=sum(V4)), by=.(V1,V2)] 
colnames(quote_equity)[2] <- "quote_result_clean"
setorder(quote_equity,closetm_SELL)
quote_equity[, cumul := cumsum(quote_result_clean)]

p1 <- ggplot(data=quote_equity, aes(x= closetm_SELL, y = cumul))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Cumulative earnings in USD")+xlab("")+
  ggtitle("Cumulative equity in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())+
  # Custom the Y scales:
  scale_y_continuous(
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./init*100, name="% of Initial balance"))


eq_per <- copy(orders_upd1_closed)
eq_per[, closetm_SELL := substr(closetm_SELL, 1, 10)]
eq_per[, closetm_SELL := as.Date(closetm_SELL)]
eq_per <- eq_per[,.(PAIR, closetm_SELL, cost_BUY_clean, quote_result_clean)]
eq_per <- eq_per[, list(quote = sum(quote_result_clean), bet = sum(cost_BUY_clean)), by = closetm_SELL]
setorder(eq_per,closetm_SELL)
eq_per[, cumul_result := cumsum(quote)]
eq_per[, cumul_bet := cumsum(bet)]
eq_per[, per_res :=  (cumul_result/cumul_bet)*100]

p2 <- ggplot(data=eq_per, aes(x= closetm_SELL, y = per_res))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Daily % ")+xlab("")+
  ggtitle("Win/Bet percentage")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_blank())+
  theme(panel.grid.minor = element_blank())


# Number of trades
ntrades <- copy(orders_upd1_closed)
ntrades[, closetm_SELL := substr(closetm_SELL, 1, 10)]
ntrades[, closetm_SELL := as.Date(closetm_SELL)]
ntrades <- ntrades[,.(PAIR, closetm_SELL)]
ntrades <- ntrades[, list(ntrades=.N), by = closetm_SELL]
setorder(ntrades,closetm_SELL)
ntrades <- ntrades[, list(ntrades = sum(ntrades)), by = closetm_SELL]

p3 <- ggplot(data=ntrades, aes(x= closetm_SELL, y = ntrades))+
  geom_line(colour = "green")+
  geom_point(colour = "green")+
  dark_theme_gray()+ylab("Daily #")+xlab("Date")+
  ggtitle("Number of trades")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())



duration <- copy(orders_upd1_closed)
duration[, closetm_SELL := substr(closetm_SELL, 1, 10)]
duration[, closetm_SELL := as.Date(closetm_SELL)]
duration[, closetm_BUY := substr(closetm_BUY, 1, 10)]
duration[, closetm_BUY := as.Date(closetm_BUY)]

duration <- duration[,.(PAIR, closetm_BUY,closetm_SELL)]
duration <- duration[, mean(closetm_SELL-closetm_BUY), by = PAIR]
setorder(duration,-V1)

duration$PAIR <- factor(duration$PAIR, levels = duration$PAIR)

p4 <- ggplot(data=duration, aes(x =PAIR, y = V1 ))+
  geom_col(colour = "yellow", fill = "darkgreen")+
  dark_theme_gray()+
  ylab("Days")+xlab("Pair")+
  ggtitle("Average duration of a trade by pair")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  geom_hline(yintercept = mean(duration$V1), colour = "red")

# Number of trades
ntrades <- copy(orders_upd1_closed)
ntrades[, closetm_SELL := substr(closetm_SELL, 1, 10)]
ntrades[, closetm_SELL := as.Date(closetm_SELL)]
ntrades <- ntrades[,.(PAIR, closetm_SELL)]
ntrades <- ntrades[, list(ntrades=.N), by = PAIR]
setorder(ntrades,-ntrades)

ntrades$PAIR <- factor(ntrades$PAIR, levels = ntrades$PAIR)

p5 <- ggplot(data=ntrades, aes(x =PAIR, y = ntrades ))+
  geom_col(colour = "yellow", fill = "darkgreen")+
  dark_theme_gray()+
  ylab("# trades")+xlab("Pair")+
  ggtitle("Total number of trades by pair")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  geom_hline(yintercept = mean(ntrades$ntrades), colour = "red")


p6 <- ggarrange(p1, p2, p3, nrow= 3)
p6
# p6
p7 <- ggarrange(p4, p5, nrow= 2)
ggarrange(p6,p7, ncol = 2)

