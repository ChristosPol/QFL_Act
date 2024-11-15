rm(list=ls())
gc()
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
pair <- "MANAUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Load data
ticks <- c(60)
units <- c("minutes")
intervals <- paste(ticks, units, sep = " ") 

df <- trades_to_OHLC(pair = pair,
                     interval = intervals,
                     from_date = "2023-03-27",
                     to_date = "2023-03-30",
                     date_subset = F)
df1 <- df[[1]]
# candles(df1)
df1[, date:=as.Date(interval)]
gc()
# Fix vector of dates
dates_vector <- unique(as.Date(df1$interval))
dates_vector <- sample(x = dates_vector, 10, replace = F)

# dates_vector <- dates_vector[-1]



# get the datasets
dataframes <- lapply(dates_vector, function(x){
  df <- df1[date >=x & date < x+(days(7))]
  # df <- df1[date ==x ]
  return(df)
})



# Set parameters table
splits_reset <- data.table(splits_reset=c(1),flag=1)
exit_points <- data.table(exit=seq(0.01,0.20 ,0.01), flag=1)
start <-data.table(start= seq(0.01, 0.1, 0.01),flag=1)
maxim <- data.table(maxim = c(0.1, 0.2, 0.3),flag=1)
n_trades <- data.table(n_trades=seq(10, 60, 10),flag=1)
SL <- data.table(sl=seq(0.01, 0.1, 0.01),flag=1)
params <- left_join(splits_reset,exit_points)%>%left_join(start)%>%left_join(maxim)%>%left_join(n_trades)%>%left_join(SL)
# params <- params[splits_reset==1 & exit == 0.05 & start == 0.005 & maxim ==0.06 & n_trades==40]


# bet <- 50
# params[, total_risked_daily := bet*n_trades*2]
# params <- params[splits_reset == 1& exit == 0.05 & start == 0.05 &maxim == 0.1 & n_trades == 60]




# For trade Ids
all_chars <- c(LETTERS, 0:9)
str_len <- 20


x <-1
# 
h <- 1
# s <- 1
gc()
# Loop through calendar days ---------------------------------------------------
daily_res <- list()
for (x in 1:length(dataframes)){
  # df <- trades_to_OHLC(pair = pair,
  #                      interval = intervals,
  #                      from_date = dates_vector[x],
  #                      to_date = dates_vector[x],
  #                      date_subset = T)
  
  df1 <- dataframes[[x]]
  
  # Loop through parameter setting ---------------------------------------------
  param_res <- list()
  # params <- params[splits_reset ==1&exit ==0.001 &start ==0.005& maxim ==0.10&n_trades==60]
  for (h in 1:nrow(params)){

    first_low <- head(df1$low,1)
    first_high <- head(df1$high,1)
      
    # Initiate step
    step <- (params$maxim[h] - params$start[h])/(params$n_trades[h])
      
    # Initiate short grid
    short_grid <- data.table(batch = sample(x = 1:10E5,1),
                             grid = first_high*(1+seq(params$start[h], params$maxim[h], by=step)),
                             init_price = first_high,
                             status_enter="open",
                             status_exit = "open")
    short_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    short_grid[, interval_enter :=POSIXct()]
    short_grid[, interval_exit :=POSIXct()]
    short_grid[, position := "short"]
    short_grid[, exits:=min(short_grid[, grid])*(1-params$exit[h])]
    setorder(short_grid, grid)
    short_grid[, SL := max(grid)+max(grid)*params$sl[h]]
    short_grid[, SL_act := F]
    short_grid[, bet:= seq(5, nrow(short_grid)*5, 5)]
    # Initiate long grid
    long_grid <- data.table(batch = sample(x = 1:10E5,1),
                            grid = first_low*(1-seq(params$start[h], params$maxim[h], by=step)),
                            init_price = first_low,
                            status_enter="open",
                            status_exit = "open")
    long_grid[, trade_id := replicate(nrow(short_grid), paste(sample(all_chars, str_len, replace = TRUE), collapse = ""))]
    long_grid[, interval_enter :=POSIXct()]
    long_grid[, interval_exit :=POSIXct()]
    long_grid[, position := "long"]
    long_grid[, exits:=max(long_grid[, grid])*(1+params$exit[h])]
    setorder(long_grid, -grid)
    long_grid[, SL := min(grid)-min(grid)*params$sl[h]]
    long_grid[, SL_act := F]
    long_grid[, bet:= seq(5, nrow(long_grid)*5, 5)]
    
    # Combine grids
    grid <- rbind(short_grid, long_grid)
    grid[, bar_entered:=NA_real_]
    grid[, bar_exited:=NA_real_]
    
    # grid <- rbind(grid, closed_orders)
    # Define entries
    high <- df1[, high]
    low <- df1[, low]
    close <- df1[, close]
    # Entries
    enter_long <- grid[position =="long" & status_enter =="open", grid]
    enter_short <- grid[position =="short" & status_enter =="open", grid]
    
    # Get indeces of entries and short
    first_index_long <- sapply(enter_long, function(x) which(low<x)[1])
    first_index_short <- sapply(enter_short, function(x) which(high > x)[1])
    
    # Update grid 
    grid[position =="long" & status_enter =="open", interval_enter:=df1[first_index_long, interval]]
    grid[position =="long" & status_enter =="open", bar_entered:=first_index_long]
    grid[position =="short" & status_enter =="open", interval_enter:=df1[first_index_short, interval]]
    grid[position =="short" & status_enter =="open", bar_entered:=first_index_short]
    grid[!is.na(interval_enter), status_enter := "closed"]
    
    # Exits
    exit_long <- unique(grid[position =="long" & status_enter =="closed" & status_exit=="open", exits])
    exit_short <- unique(grid[position =="short" & status_enter =="closed" & status_exit=="open", exits])
    
    # Indeces of exits for long and short
    first_index_long <- sapply(grid[position =="long" & status_enter =="closed" & status_exit=="open", bar_entered],
                               function(x) which(high[x:length(high)]>exit_long)[1]+x)
    first_index_long[is.na(first_index_long)] <- 10E5
    exit_df_long <- data.table(tp = first_index_long, pos = "long", type="tp")
    
    
    first_index_short <- sapply(grid[position =="short" & status_enter =="closed" & status_exit=="open", bar_entered],
                                function(x) which(low[x:length(high)]<exit_short)[1]+x)
    first_index_short[is.na(first_index_short)] <- 10E5
    # names(first_index_short) <- rep("exit", length(first_index_short))
    exit_df_short <- data.table(tp = first_index_short, pos = "short", type="tp")
    
    # Sls
    SL_long <- unique(grid[position =="long" & status_enter =="closed" & status_exit=="open", SL])
    SL_short <- unique(grid[position =="short" & status_enter =="closed" & status_exit=="open", SL])
    
    # Indeces of SLs for long and short
    SL_index_long <- sapply(grid[position =="long" & status_enter =="closed" & status_exit=="open", bar_entered],
                               function(x) which(close[x:length(close)]<SL_long)[1]+x)
    SL_index_long[is.na(SL_index_long)] <- 10E5
    SL_df_long <- data.table(sl = SL_index_long, pos = "long", type = "sl")
    
    SL_index_short <- sapply(grid[position =="short" & status_enter =="closed" & status_exit=="open", bar_entered],
                                function(x) which(close[x:length(close)]>SL_short)[1]+x)
    SL_index_short[is.na(SL_index_short)] <- 10E5
    SL_df_short <- data.table(sl = SL_index_short, pos = "short", type = "sl")
    

    
    if(nrow(grid[position =="long" & status_enter =="closed"& status_exit=="open"])>0){
      exit_sl_tp_long <- cbind(SL_df_long,exit_df_long)
      exit_sl_tp_long[, SL_TRUE:= sl<tp]
      grid[position =="long" & status_enter =="closed"& status_exit=="open", interval_exit:=df1[ifelse(exit_sl_tp_long$SL_TRUE == T, exit_sl_tp_long$sl, exit_sl_tp_long$tp), interval]]
      grid[position =="long" & status_enter =="closed"& status_exit=="open", bar_exited:=ifelse(exit_sl_tp_long$SL_TRUE == T, exit_sl_tp_long$sl, exit_sl_tp_long$tp)]
      grid[position =="long" & status_enter =="closed"& status_exit=="open", SL_act:=ifelse(exit_sl_tp_long$SL_TRUE == T, T, F)]  
    } 
    
    
    if(nrow(grid[position =="short" & status_enter =="closed"& status_exit=="open"])>0){
      exit_sl_tp_short <- cbind(SL_df_short, exit_df_short)
      exit_sl_tp_short[, SL_TRUE:= sl<tp]
      grid[position =="short" & status_enter =="closed"& status_exit=="open", interval_exit:=df1[ifelse(exit_sl_tp_short$SL_TRUE == T, exit_sl_tp_short$sl, exit_sl_tp_short$tp), interval]]
      grid[position =="short" & status_enter =="closed"& status_exit=="open", bar_exited:=ifelse(exit_sl_tp_short$SL_TRUE == T, exit_sl_tp_short$sl, exit_sl_tp_short$tp)]
      grid[position =="short" & status_enter =="closed"& status_exit=="open", SL_act:=ifelse(exit_sl_tp_short$SL_TRUE == T, T, F)]  
    }
    
    grid[!is.na(interval_exit), status_exit := "closed"]
  
    # Here calculate result, add total bet, returns, day, hodl etc
    
    grid[status_enter =="closed" & status_exit =="closed", final_exit := exits]
    grid[status_enter =="closed" & status_exit =="closed" & SL_act == T, final_exit := SL]
    grid[status_enter =="closed" & status_exit =="open" & is.na(final_exit), final_exit := tail(df1[, close], 1)]
    
    grid[, percent := (final_exit-grid)/grid]
    grid[position == "short", percent:=-1*percent]
    grid[, quote_res_no_fees := bet+bet*percent]
    grid[, quote_res_clean := quote_res_no_fees-(0.65/100)*quote_res_no_fees]
    grid[, bet := bet]
    grid <- grid[status_enter =="closed"]
    
    param_result <- copy(params[h,])
    param_result[, percent := (sum(grid$quote_res_clean)-sum(grid$bet))/(sum(grid$bet))]
    param_result[, total_bet := sum(grid$bet)]
    param_result[, quote_res := total_bet*percent+total_bet]
    param_result[, day := dates_vector[x]]
    param_result[, hodl := (tail(df1[, close], 1)-head(df1[, close], 1))/head(df1[, close], 1)]
    param_res[[h]] <- param_result
    # print(param_res)
    # Sys.sleep(0.5)
    print(cbind(params[h,], h,x, percent=param_result[, percent], result_usd=param_result[, quote_res]-param_result[, total_bet]))
    
  }
  
  daily_res[[x]] <- rbindlist(param_res)
  print(x)
}
# Save
save(daily_res, file=paste0("~/Repositories/Private/QFL_Act/Code/Parameter_optim/Grid/results/", paste0(ticks,"_", units,"_", pair, Sys.time()), ".Rdata"))

# View(rbindlist(daily_res))
# # test <- rbindlist(daily_res)
# # test[is.nan(total_bet), total_bet:=0]
# # test[is.nan(quote_res), quote_res:=0]
# # sum(test$quote_res - test$total_bet)
# grid<- closed_orders

# candles(data =df1)+
# 
#   geom_point(data=grid[!is.na(interval_enter) & position == "long"], aes(x=interval_enter, y=grid), fill="lightblue3",colour="black", shape =24, size=2)+
#   geom_point(data=grid[!is.na(interval_exit) & position == "long" & SL_act ==F], aes(x=interval_exit, y=final_exit), fill="lightblue3", colour="black",shape =25, size=2)+
#   geom_point(data=grid[!is.na(interval_exit) & position == "long" & SL_act ==T], aes(x=interval_exit, y=final_exit), fill="lightblue3", colour="black",shape =25, size=2)+
#   geom_point(data=grid[!is.na(interval_enter) & position == "short"], aes(x=interval_enter, y=grid), fill="darkorchid1", colour="black", shape =25, size=2)+
#   geom_point(data=grid[!is.na(interval_exit) & position == "short"& SL_act ==F], aes(x=interval_exit, y=final_exit), fill="darkorchid1", colour="black",shape =24, size=2)+
# geom_point(data=grid[!is.na(interval_exit) & position == "short" & SL_act ==T], aes(x=interval_exit, y=final_exit), fill="darkorchid1", colour="black",shape =24, size=2)
# geom_vline(xintercept = grid[position == "short", interval_exit])
# 
# 
# 
