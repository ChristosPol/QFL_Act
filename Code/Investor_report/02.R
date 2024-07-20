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
                           Trade_Id = c("TTZ7VW-3BNMP-2QLQZF", "Already in CHF"),
                           Type = c("Deposit", "Existing"))

# Load equity
equity <- read_csv(file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/usd_equity.csv")
setDT(equity); equity <- equity[, .(date, equity)]
equity[]


# equity[date >=investor_tab[Type %in% c("Deposit"), Starting_date], equity := equity - investor_tab[Type %in% c("Deposit"), Converted_Funds_USD]]
# 
# equity[, diff :=c(0,  diff(equity))]
# equity[, percent_change := diff/shift(equity, 1) *100]


inv_names <- investor_tab[, Initials]
i <- 1
for(i in 1:length(inv_names)){
  print(paste0("Working for ", inv_names[i], ".."))
  equity_inv <- copy(equity)
  contribution <- case_when(equity_inv$date==investor_tab[Initials %in%inv_names[i], Starting_date] ~  investor_tab[Initials %in%inv_names[i], Converted_Funds_USD]/equity)
  
  contribution <- equity_inv[date==investor_tab[Initials %in%inv_names[i], Starting_date], investor_tab[Initials %in%inv_names[i], Converted_Funds_USD]/equity]
  equity_inv[date >= investor_tab[Initials %in%inv_names[i], Starting_date], equity_upd := equity*contribution]

  
  not_names <- inv_names[!inv_names %in% inv_names[i]]
  not_tab <-investor_tab[Initials %in% not_names]
  equity <- equity[date>investor_tab[Initials %in%inv_names[i], Starting_date]]
  equity[, equity_upd := investor_tab[Initials %in%inv_names[i], Converted_Funds_USD]*(percent_change/100)]
  equity[is.na(equity_upd), equity_upd:=0]
  equity[, equity_upd :=cumsum(equity_upd)+investor_tab[Initials %in%inv_names[i], Converted_Funds_USD]]
  
  
  
}



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
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())

p2 <- ggplot(data=EV[values %in% c("returns_USD_EV")], aes(x= date, y = value))+
  geom_bar(stat = "identity")+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
p3 <- ggplot(data=EV[values %in% c("returns_USD_cumulative_EV")], aes(x= date, y = value))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())


p4 <- ggplot(data=EV[values %in% c("returns_percent_EV")], aes(x= date, y = value))+
  geom_bar(stat = "identity")+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
p5 <- ggplot(data=EV[values %in% c("returns_percent_cumulative_EV")], aes(x= date, y = value))+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  facet_grid(~values)+
  dark_theme_gray()+ylab("USD")+xlab("")+
  # ggtitle("Total Equity in USD, CP")+
  # scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())

p1/(p2+p3)/(p4+p5)




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


