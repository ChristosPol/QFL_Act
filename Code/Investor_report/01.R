rm(list = ls())
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Investor table
investor_tab <- data.table(First_name = c("Elena"),
                           Last_name = c("V"),
                           Initials = c("EV"),
                           Starting_date =as.Date(c("2024-05-17")),
                           Initial_Funds_CHF = c(2000),
                           Converted_Funds_USD = c(2202.9590),
                           Conversion_fees = 0,
                           Price_conversion = "0.90787 CHF",
                           Order_id = "OMHCLI-6RAMG-4VMGUG",
                           Trade_Id = "TTZ7VW-3BNMP-2QLQZF")

# Load equity
equity <- read_csv(file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/usd_equity.csv")
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

secondary_axis <- function(eq){
  ret <- c(0, diff(eq))
  per <- ret/shift(eq, 1)*100
  per[is.na(per)] <- 0
  per <- cumsum(per)
  return(per)
}

secondary_axis(eq = equity[CP_contribution !=0, CP_equity])

equity[CP_contribution !=0, CP_equity]
# CP
p1 <- ggplot(data=equity[CP_contribution !=0], aes(x= date, y = CP_equity))+
  # geom_hline(yintercept = equity$CP_equity[1], colour = "red", linetype = "dotted")+
  # geom_hline(yintercept = equity$CP_equity[length(equity$CP_equity)], colour = "red", linetype = "dotted")+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Total Equity in USD, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = round(seq(min(equity$CP_equity)-500, max(equity$CP_equity)+100,  100)), sec.axis = ~secondary_axis(.))
p1





p2 <- ggplot(data=equity[CP_contribution !=0], aes(x= date, y = returns_USD_CP))+
  geom_bar(stat = "identity",fill= "white", colour = ifelse(equity$returns_USD_CP >0, "darkgreen", "red"))+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Daily returns in USD, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
  # scale_y_continuous(breaks = round(seq(round(min(equity$returns_USD_CP), -1), round(max(equity$returns_USD_CP), -1),  100)))

p3 <- ggplot(data=equity[CP_contribution !=0], aes(x= date, y = returns_percent_CP))+
  geom_bar(stat = "identity",fill= "white", colour = ifelse(equity$returns_USD_CP >0, "darkgreen", "red"))+
  dark_theme_gray()+ylab("%")+xlab("")+
  ggtitle("Daily returns in percent, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
# scale_y_continuous(breaks = round(seq(round(min(equity$returns_USD_CP), -1), round(max(equity$returns_USD_CP), -1),  100)))


p4 <- ggplot(data=equity[CP_contribution !=0], aes(x= date, y = returns_percent_cumulative_CP))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("%")+xlab("")+
  ggtitle("Cumulative daily percentage returbs, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
  # scale_y_continuous(breaks = round(seq(0, max(equity$CP_equity),  100)))

ggarrange(p1,p4, nrow =2)
ggarrange(p2,p3, nrow =2)



# EV
p1 <- ggplot(data=equity[EV_contribution !=0], aes(x= date, y = EV_equity))+
  geom_hline(yintercept = 2202.959, colour = "red", linetype = "dotted")+
  geom_hline(yintercept = equity$EV_equity[length(equity$EV_equity)], colour = "red", linetype = "dotted")+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Total Equity in USD, EV")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
  # scale_y_continuous(breaks = round(seq(0, max(equity$EV_equity),  100)))
p1



p2 <- ggplot(data=equity[EV_contribution !=0], aes(x= date, y = returns_USD_EV))+
  geom_bar(stat = "identity",fill= "grey", colour = ifelse(equity[EV_contribution !=0, returns_USD_EV] >0, "darkgreen", "red"))+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Daily returns in USD, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
# scale_y_continuous(breaks = round(seq(round(min(equity$returns_USD_CP), -1), round(max(equity$returns_USD_CP), -1),  100)))

p3 <- ggplot(data=equity[EV_contribution !=0], aes(x= date, y = returns_percent_EV))+
  geom_bar(stat = "identity",fill= "white", colour = ifelse(equity[EV_contribution !=0, returns_percent_EV] >0, "darkgreen", "red"))+
  dark_theme_gray()+ylab("%")+xlab("")+
  ggtitle("Daily returns in percent, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
# scale_y_continuous(breaks = round(seq(round(min(equity$returns_USD_CP), -1), round(max(equity$returns_USD_CP), -1),  100)))


p4 <- ggplot(data=equity[EV_contribution !=0], aes(x= date, y = returns_percent_cumulative_EV))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("%")+xlab("")+
  ggtitle("Cumulative daily percentage returns, CP")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
# scale_y_continuous(breaks = round(seq(0, max(equity$CP_equity),  100)))

ggarrange(p1,p4, nrow =2)
ggarrange(p2,p3, nrow =1)
