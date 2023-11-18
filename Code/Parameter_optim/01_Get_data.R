# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R

rm(list = ls())

.rs.restartR()

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
pair <- "MNGOUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Code/Parameter_optim/Data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = T)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Select initial id based on unix_time arg
initial_id <- select_period(unix_time,  diff_time = 600)

# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 1, hist_id = initial_id, pair = pair)



ticks <- c(5)

units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")
library(tidyquant)


df <- trades_to_OHLC(pair = pair,
               interval = intervals,
               from_date = "2022-02-01",
               to_date = "2022-03-03",
               date_subset = F)

# df <- df[[1]]


candles(data =df[[1]][1:5000,], date_interval = "2 week")

candles <- function(data,date_interval = "2 week"){
  # data[, date := as.Date(date)]
  data[close>open, colour := "green"]
  data[close<open, colour := "red"]
  data[close==open, colour := "black"]
  data[, colour:= as.factor(data$colour)]

  p <- ggplot(data = data)+
    geom_segment(aes(x =date,
                     xend=date,
                     y =open,
                     yend =close,
                     colour= colour),
                 size=1.5)+
    geom_segment(aes(x = date,
                     xend=date,
                     y =high,
                     yend =low,colour= colour))+
    scale_color_manual(values=c("black","Forest Green","Red"))+
    theme_bw()+
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
  # +
    # scale_x_datetime(labels = date_format("%H:%M:%S"))
  # +
  #   scale_x_date(date_labels="%d-%m-%Y", date_breaks  =date_interval)
  return(p)
}


env <- environment()
dt <- dt[t==get('t',env)]
mean(dt$b)
data[, get('date',env)]

# all <- lapply(pairs, function(x)trades_to_OHLC(pair = x,
#                                                interval = intervals,
#                                                from_date = "2022-02-01",
#                                                to_date = "2022-03-03",
#                                                date_subset = F))