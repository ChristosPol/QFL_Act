rm(list = ls())
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Load equity
equity <- read_csv(file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/usd_equity.csv")
setDT(equity); equity <- equity[, .(date, equity)]
CP_equity <- equity$equity[1]
CP_starting_date <- equity$date[1]
# Investor table
investor_tab <- data.table(First_name = c("N", "E"),
                           Last_name = c("A", "V"),
                           Initials = c("NA", "EV"),
                           Starting_date =as.Date(c("2024-01-01", "2024-02-01")),
                           Funds_USD = c(100, 250))

# Merge equity to investor_tab
i <- 1
inv_list_eq <- list()
for(i in 1:nrow(investor_tab)){
  equity_inv <- equity[date >= investor_tab[i, Starting_date]]
  equity_inv[, returns_USD:= c(0, diff(equity))]
  equity_inv[, returns_percent:= (returns_USD/equity)*100]
  equity_inv[, returns_percent_cumulative:= cumsum(returns_percent)]
  equity_inv[, equity_inv := investor_tab[i, Funds_USD] + (returns_percent_cumulative/100) * investor_tab[i, Funds_USD]]
  inv_list_eq[[i]] <- equity_inv
}
dat <- inv_list_eq[[1]]
p1 <- ggplot(data=dat, aes(x= date, y = equity_inv))+
  geom_hline(yintercept = dat$equity_inv[1], colour = "red", linetype = "dotted")+
  geom_hline(yintercept = dat$equity_inv[length(dat$equity_inv)], colour = "red", linetype = "dotted")+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("Cumulative earnings in USD")+xlab("")+
  
  # ggtitle("Cumulative equity in USD")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = round(seq(0, max(dat$equity_inv),  1)))

p2 <- ggplot(data=dat, aes(x= date, y = returns_percent))+
  geom_hline(yintercept = 0, colour = "red", linetype = "dotted")+
  geom_hline(yintercept = mean(dat$returns_percent), colour = "lightblue", linetype = "dotted")+
  geom_line(colour = "green", size =0.5)+
  geom_point(colour = "darkgreen", size = 1,stroke = 1, shape =1)+
  dark_theme_gray()+ylab("Daily returns in percentage")+xlab("")+
  scale_x_date(date_labels="%d-%m-%Y",date_breaks  ="7 days")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.minor = element_blank())
ggarrange(p1,p2, nrow =2)
