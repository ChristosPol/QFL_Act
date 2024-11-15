rm(list=ls())
gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)


load("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/60_minutes_HDXUSD2024-11-15 17:00:15.251066.Rdata")

# Need to adjust for how much hodl percentage are the returns, returns need to be relevant, related

d <- rbindlist(daily_res)
d[is.nan(percent),  percent:=0]
d[is.nan(total_bet),  total_bet:=0]
d[is.nan(quote_res),  quote_res:=0]
d[ , concatenated := paste(splits_reset,exit,start,maxim,sl,n_trades , sep = "_")]
setorder(d, concatenated,day)
data_unique <- d[ , .SD[1], by = .(concatenated, day)]



metrics <- data_unique[,   list(aver_percent=mean(percent),
                                mean_bet=mean(total_bet),
                                mean_aver_quote=mean(quote_res-total_bet),
                                sum_bet = sum(total_bet),
                                sum_quote = sum(quote_res-total_bet),
                                entered = sum(total_bet>0),
                                positive_percent = sum(percent>0)), by=.(concatenated)]

ggplot(data=metrics, aes(x=aver_percent, y= mean_aver_quote))+
  geom_point(colour ="darkred")
