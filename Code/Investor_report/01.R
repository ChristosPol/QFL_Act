rm(list = ls())
library(patchwork)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Investor table
investor_tab <- data.table(First_name = c("Elena", "Christos"),
                           Last_name = c("V", "P"),
                           Initials = c("EV", "CP"),
                           Starting_date =as.Date(c("2024-05-17", "2023-11-15")),
                           Initial_Funds_CHF = c(2000, 0),
                           Converted_Funds_USD = c(2202.9590, 2821.719),
                           Conversion_fees = c(0, 0),
                           Price_conversion = c("0.90787 CHF", "Already in CHF"),
                           Order_id = c("OMHCLI-6RAMG-4VMGUG", "Already in CHF"),
                           Trade_Id = c("TTZ7VW-3BNMP-2QLQZF", "Already in CHF"))

# Load equity
equity <- read_csv(file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/usd_equity.csv")
setDT(equity); equity <- equity[, .(date, equity)]

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


equity[, EV_equity := equity*EV_contribution]
equity[, returns_USD_EV := c(0, diff(EV_equity))]
equity[date== "2024-05-17", returns_USD_EV := 0]
equity[, returns_percent_EV := (returns_USD_EV/shift(EV_equity, 1))*100]
equity[is.na(returns_percent_EV), returns_percent_EV:=0]
equity[, returns_percent_cumulative_EV:= cumsum(returns_percent_EV)]
equity[, returns_USD_cumulative_EV:= cumsum(returns_USD_EV)]


EV <- equity[EV_contribution!=0, .(date, EV_equity, returns_USD_EV,returns_percent_EV,returns_percent_cumulative_EV,returns_USD_cumulative_EV)]
EV <- melt(setDT(EV), id.vars = c("date"), variable.name = "values")

save(EV, file=paste0(getwd(), "/code/Investor_report/data/EV.Rdata"))

CP <- equity[CP_contribution!=0, .(date, CP_equity, returns_USD_CP,returns_percent_CP,returns_percent_cumulative_CP,returns_USD_cumulative_CP)]
CP <- melt(setDT(CP), id.vars = c("date"), variable.name = "values")

# secondary_axis <- function(eq){
#   ret <- c(0, diff(eq))
#   per <- ret/shift(eq, 1)*100
#   per[is.na(per)] <- 0
#   per <- cumsum(per)
#   return(per)
# }

p1 <- ggplot(data=EV[values == "EV_equity"], aes(x= date, y = value))+
  geom_hline(yintercept = first(EV[values == "EV_equity", value]), linetype="dashed")+
  geom_hline(yintercept = last(EV[values == "EV_equity", value]), linetype="dashed")+
  geom_line(colour = "grey", size =1)+
  geom_point(colour="white",fill="darkgreen", size = 3,stroke = 0.5, shape =21)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Total Equity, EV")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # theme(panel.grid.minor = element_blank())+
  # scale_y_continuous( limits = c(2150, 2290), breaks = seq(2150, 2290, 10))
p1 <- p1 + annotate(x = as.Date("2024-05-30"), y = 2260, geom = "text", label = "+3.34%\n+73USD", colour="green")

p2 <- ggplot(data=EV[values %in% c("returns_USD_EV")], aes(x= date, y = value))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_bar(stat = "identity", fill = ifelse(EV[values %in% c("returns_USD_EV"), value]>0, "darkgreen", "tomato"))+
  # facet_grid(~values)+
  ggtitle("Daily P/L of equity in USD")+
  dark_theme_gray()+ylab("USD")+xlab("")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(panel.grid.minor = element_blank())
  scale_y_continuous( limits = c(-30, 80), breaks = seq(-30, 80, 5))
p2

p3 <- ggplot(data=EV[values %in% c("returns_USD_cumulative_EV")], aes(x= date, y = value))+
  geom_line(colour = "grey", size =1)+
  geom_point(colour="white",fill="darkgreen", size = 3,stroke = 0.5, shape =21)+
  # facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Daily cumulative returns in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # theme(panel.grid.minor = element_blank())
  scale_y_continuous( limits = c(-40, 85), breaks = seq(-40, 85, 5))
p3

p4 <- ggplot(data=EV[values %in% c("returns_percent_EV")], aes(x= date, y = value))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_bar(stat = "identity", fill = ifelse(EV[values %in% c("returns_percent_EV"), value]>0, "darkgreen", "tomato"))+
  # facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Daily P/L of equity in %")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # theme(panel.grid.minor = element_blank())
p4 

p5 <- ggplot(data=EV[values %in% c("returns_percent_cumulative_EV")], aes(x= date, y = value))+
  geom_line(colour = "grey", size =1)+
  geom_point(colour="white",fill="darkgreen", size = 3,stroke = 0.5, shape =21)+
  # facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Daily cumulative returns in %")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="1 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1
(p2+p3)/(p4+p5)




p1 <- ggplot(data=CP[values == "CP_equity"], aes(x= date, y = value))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())

p2 <- ggplot(data=CP[values %in% c("returns_USD_CP")], aes(x= date, y = value))+
  geom_bar(stat = "identity")+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
p3 <- ggplot(data=CP[values %in% c("returns_USD_cumulative_CP")], aes(x= date, y = value))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())


p4 <- ggplot(data=CP[values %in% c("returns_percent_CP")], aes(x= date, y = value))+
  geom_bar(stat = "identity")+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
p5 <- ggplot(data=CP[values %in% c("returns_percent_cumulative_CP")], aes(x= date, y = value))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())

p1/(p2+p3)/(p4+p5)


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
  df <- df[Date_POSIXct >= "2023-11-15"]
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
save(market_cumulative_returns, file=paste0(getwd(), "/code/Investor_report/data/market_cumulative_returns.Rdata"))

View(market_cumulative_returns)

save(alpha, file = "Data/evaluation/alpha.Rdata")
# df2 <- df1[Date_POSIXct <= df1[PAIR == "CP", max(Date_POSIXct)]]
# unique(df2$PAIR)[i]
load(file = "Data/evaluation/alpha.Rdata")
ggplot(data=alpha, aes(x = Date_POSIXct, y= cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2) + theme(legend.position = "none")+
  theme_bw()

status <- alpha[Date_POSIXct == max(Date_POSIXct)]
setorder(status, -cum_diff_per)
quantile(status$cum_diff_per, probs = .84)


load("Data/evaluation/alpha.Rdata")
ggplot(data = alpha, aes(x = Date_POSIXct, y = cum_diff_per, colour = PAIR))+
  geom_line(alpha = 0.2)+
  geom_line(data=alpha[PAIR == "CP"],  aes(x = Date_POSIXct, y = cum_diff_per), colour = "white")+
  dark_theme_gray()+
  coord_cartesian(ylim = c(-200, 1500))+
  ylab("Percentage cumulative returns")+
  xlab("Date")+
  ggtitle("Strategy returns vs single asset portfolio, currently at 0.84 quantile")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="90 day")+
  theme(legend.position = "none")
ggsave(filename = "Data/evaluation/alpha.png", height = 5, width =10)
percentile <- ecdf(1:10)
percentile(8)

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


# Compount returns analysis
P_CP<- CP[date==max(date) & values == "CP_equity", value]
r_CP <- CP[values == "returns_percent_CP", mean(value)]/100
days_CP <- 3*360
A_CP <- compound_returns(P=P_CP, r = 0.0025, n = 1:days_CP,t = 1:days_CP)
A_df_CP <- data.frame(A = A_CP, days = 1:days_CP)

view(A_df_CP)

ggplot(data=A_df_CP, aes(x = days, y = A))+
  geom_line()+
  dark_theme_gray()

P_EV<- EV[date==min(date) & values == "EV_equity", value]
r_EV <- EV[values == "returns_percent_EV", mean(value)]/100
days_EV <- 1*360
A_EV <- compound_returns(P=P_EV, r = r_EV, n = 1:days,t = 1:days_EV)
A_df_EV <- data.frame(A = A_EV, days = 1:days_EV)

ggplot(data=A_df_EV, aes(x = days, y = A))+
  geom_line()+
  dark_theme_gray()+
  geom_point(x = 21, y = 2291.00571225, colour = "green")

View(EV)
