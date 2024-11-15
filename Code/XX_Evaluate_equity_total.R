rm(list = ls())
library(patchwork)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Investor table
# investor_tab <- data.table(First_name = c("Elena", "Christos"),
#                            Last_name = c("V", "P"),
#                            Initials = c("EV", "CP"),
#                            Starting_date =as.Date(c("2024-05-17", "2022-11-01")),
#                            Initial_Funds_CHF = c(2000, 0),
#                            Converted_Funds_USD = c(2202.9590, 922.85),
#                            Conversion_fees = c(0, 0),
#                            Price_conversion = c("0.90787 CHF", "Already in CHF"),
#                            Order_id = c("OMHCLI-6RAMG-4VMGUG", "Already in CHF"),
#                            Trade_Id = c("TTZ7VW-3BNMP-2QLQZF", "Already in CHF"))


investor_tab <- data.table(First_name = c("Elena", "Christos"),
                           Last_name = c("V", "P"),
                           Initials = c("EV", "CP"),
                           Starting_date =as.Date(c("2024-05-17", "2023-01-01")),
                           Initial_Funds_CHF = c(2000, 0),
                           Converted_Funds_USD = c(2202.9590, 1207.28),
                           Conversion_fees = c(0, 0),
                           Price_conversion = c("0.90787 CHF", "Already in CHF"),
                           Order_id = c("OMHCLI-6RAMG-4VMGUG", "Already in CHF"),
                           Trade_Id = c("TTZ7VW-3BNMP-2QLQZF", "Already in CHF"))

# Load equity
equity <- read_csv(file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/usd_equity.csv")
setDT(equity); equity <- equity[, .(date, equity)]
equity[, date := as.Date(date, format = "%m/%d/%y")]

# equity <- equity[date>="2022-11-01"]
equity <- equity[date>="2023-01-01"]
equity[, diff :=c(0,  diff(equity))]
equity[, percent_change := diff/shift(equity, 1) *100]


portfolio_contribution <- data.table(date= as.Date("2024-05-17"), EV_USD = 2202.9590, CP_USD = 6625.155 - 2202.9590, total = 6625.155)
portfolio_contribution[, EV_contribution := EV_USD/total]
portfolio_contribution[, CP_contribution := CP_USD/total]

equity[date < "2024-05-17", CP_contribution :=1]
equity[date < "2024-05-17", EV_contribution :=0]
equity[date >= "2024-05-17", CP_contribution := portfolio_contribution$CP_contribution]
equity[date >= "2024-05-17", EV_contribution := portfolio_contribution$EV_contribution]


equity[, CP_equity := equity*CP_contribution]
equity[, returns_USD_CP := c(0, diff(CP_equity))]
equity[, returns_percent_CP := (returns_USD_CP/shift(CP_equity, 1))*100]
equity[is.na(returns_percent_CP), returns_percent_CP:=0]
equity[, returns_percent_cumulative_CP:= cumsum(returns_percent_CP)]
equity[, returns_USD_cumulative_CP:= cumsum(returns_USD_CP)]

# Compared to market
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
  df <- df[Date_POSIXct >= "2022-11-01"]
  df[, diff := c(0, diff(close))]
  df[, cum_diff := cumsum(diff)]
  df[, cum_diff_per := cum_diff/close[1]*100]
  df[, PAIR := all_pairs$PAIR[i]]
  mah_list[[i]]<- df
  Sys.sleep(1)
  print(i)
}

dfdf <- rbindlist(mah_list)
colnames(dfdf)
dfdf <- dfdf[, .(PAIR, Date_POSIXct,cum_diff_per)]
market_cumulative_returns <- copy(dfdf)
colnames(market_cumulative_returns)
PAIR <- "CP"
alpha <- cbind(PAIR, equity[, .(date, returns_percent_cumulative_CP)])
colnames(alpha) <- colnames(market_cumulative_returns)

alpha <- rbind(alpha,market_cumulative_returns)
ggplot(data = alpha, aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2)+
  geom_line(data=alpha[PAIR == "CP"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "white")+
  geom_line(data=alpha[PAIR == "ADAUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "blue")+
  geom_line(data=alpha[PAIR == "SOLUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "red")+
  # geom_line(data=alpha[PAIR == "XXBTZUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "yellow")+
  geom_line(data=alpha[PAIR == "XETHZUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "yellow")+
  dark_theme_gray()+
  coord_cartesian(ylim = c(-200, 1500))+
  ylab("Percentage cumulative returns")+
  xlab("Date")+
  # ggtitle("Strategy returns vs single asset portfolio, currently at 0.84 quantile")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="90 day")+
  theme(legend.position = "none")
# ggsave(filename = "Data/evaluation/alpha.png", height = 5, width =10)


ggplot(data = alpha[PAIR %in% c("XXBTZUSD","XETHZUSD","SOLUSD","XXRPZUSD",
                                "XDGUSD","PEPEUSD","TAOUSD","FTMUSD", "CP")], aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2)+
  # geom_line(data=alpha[PAIR == "CP"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "white")+
  # geom_line(data=alpha[PAIR == "ADAUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "blue")+
  # geom_line(data=alpha[PAIR == "SOLUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "red")+
  # # geom_line(data=alpha[PAIR == "XXBTZUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "yellow")+
  # geom_line(data=alpha[PAIR == "XETHZUSD"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "yellow")+
  dark_theme_gray()+
  coord_cartesian(ylim = c(-200, 1500))+
  ylab("Percentage cumulative returns")+
  xlab("Date")+
  # ggtitle("Strategy returns vs single asset portfolio, currently at 0.84 quantile")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="90 day")+
  theme(legend.position = "none")+
  geom_label_repel(aes(label = PAIR),
                   nudge_x = 1,
                   na.rm = TRUE)


best <- alpha[Date_POSIXct =="2024-09-30"]
setorder(best, -cum_diff_per)
View(best)
ecdf(best$cum_diff_per)
