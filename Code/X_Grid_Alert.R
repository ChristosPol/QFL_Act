rm(list = ls())
options(scipen = 999)

library(tidyquant)
library(stringr)
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path_alerts <- "Alerts"
dir.create(path_alerts,  showWarnings = FALSE)


url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)

all_pairs <- names(tb$result)
all_pairs <- data.table(pairs = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs[, leverage := unlist(lapply(lapply(tb$result,"[[", "leverage_buy"), length))]

# all_pairs <- all_pairs[CUR%in%c("EUR", "USD", "CAD", "GBP", "JPY", "CHF")]
all_pairs <- all_pairs[CUR%in%c( "USD")]
# all_pairs <- all_pairs[leverage>0]
# all_pairs <- all_pairs[CUR%in%c( "XBT")]
EUR_pairs   <- sort(all_pairs$pairs)

# avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
# all_pairs <- names(avail_pairs[[2]])
# 
# # Get only EUR related crypto pairs
# EUR_pairs <- grep("USD", all_pairs, value = T)
# # EUR_pairs <- grep(paste("EUR", "ETH", sep = "|"), all_pairs, value = T)
# 
# # Remove Forex pairs
# to_remove <- grep(paste(c("EUR",
#                           ".d",
#                           "AUD",
#                           "CAD",
#                           "JPY",
#                           "CHF",
#                           "GBP",
#                           "PAX",
#                           "DAI",
#                           "BAT"), collapse ="|"), EUR_pairs, value = T)
# EUR_pairs <- EUR_pairs[!EUR_pairs %in% to_remove]
# csv_path <- paste0("Data/minimums_calculated.csv")
# EUR_pairs <- read_csv(csv_path)
# EUR_pairs <- EUR_pairs$COIN
# USD_pairs <- gsub("USD", "EUR", EUR_pairs )
# EUR_pairs <- c(EUR_pairs, USD_pairs)
# EUR_pairs <- sort(EUR_pairs)

# Dynamic support and resistance
# Get OHLC data and determine trends

# look_back1 <- 100
# look_back2 <- 500
# n_sort <- 5
# n_exclude <- 10
interval <- 60


pdf(paste0(path_alerts, "/plots_", interval, "minutes.pdf"), onefile = TRUE)
i <- 10
for (i in 1:length(EUR_pairs)){
  msg <- tryCatch({
    df <- simple_OHLC(interval = interval, pair = EUR_pairs[i])
    df[, candle_type := ifelse(close > open, "green", "red")]
    # df$week <- lubridate::week(as.Date(df$Date_POSIXct))
    # df$day <- lubridate::yday(as.Date(df$Date_POSIXct))
    df$week <- isoweek(as.Date(df$Date_POSIXct))
    df$weekday <- weekdays(as.Date(df$Date_POSIXct))
    weeks <- as.character(unique(df$week))
    weeks <- weeks[!weeks %in%names(which.min(table(df$week)))]
    
    
    SP <- c()
    sp_test <- list()
    rs_test <- list()
    RS <- c()
    j <-1
    # for(j in 1:length(unique(splits[, seq]))){
    for(j in 1:length(weeks)){
      # subdf <- df[splits[seq == j, idx], ]
      subdf <- df[week == weeks[j], ]
      
      SP[j] <- median(head(sort(subdf[, close]), 5))
      sp_test[[j]] <- c(min(head(sort(subdf[, close]), 5)), max(head(sort(subdf[, close]), 5)))
      rs_test[[j]] <- c(min(tail(sort(subdf[, close]), 5)), max(tail(sort(subdf[, close]), 5)))
      RS[j] <- median(tail(sort(subdf[, close]), 5))
    }
  
    # last_close <- tail(df[, close], 1)
    # SP <- SP[SP < last_close]
    # RS <- RS[RS > last_close]
    
    df$x <- 1:nrow(df)
    df[, volume_quote := close*volume]
    
    min_sp <- unlist(lapply(sp_test, "[", 1))
    max_sp <- unlist(lapply(sp_test, "[", 2))
    
    min_rs <- unlist(lapply(rs_test, "[", 1))
    max_rs <- unlist(lapply(rs_test, "[", 2))
    xmin <- 1
    xmax <- nrow(df)
    
    
    p1 <- df %>%
      ggplot(aes(x = x, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close),fill_up ="green3", fill_down ="red" ,colour_up = "green3", colour_down = "red") +
      geom_hline(yintercept = tail(df$close, 1), linetype ="twodash", color = "black", size = 0.3)+
      scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
      geom_vline(xintercept = df[, max(x), by  = week][, V1] , linetype="dotted", color = "black", size = 0.3)+ theme_classic()#+
    p2 <- p1+geom_hline(yintercept = SP[SP<tail(df$close, 1)], linetype ="twodash", color = "blue", size = 0.3) 
    p3 <- p2+geom_hline(yintercept = RS[RS>tail(df$close, 1)], linetype ="twodash", color = "red", size = 0.3) 
    # p2 <- p1 + annotate("rect", xmin = xmin, xmax = xmax, ymin = min_sp, ymax = max_sp,fill = "blue", alpha= 0.2)
    # p3 <- p2 + annotate("rect", xmin = xmin, xmax = xmax, ymin = min_rs, ymax = max_rs,fill = "red", alpha= 0.2)
    
    per <- c()
    for(k in 1:length(unique(df$week))){
      # subdf <- df[splits[seq == k, idx], ]
      subdf <- df[week == unique(df$week)[k], ]
      
      per[k] <- round((tail(subdf$close, 1) - head(subdf$close,1))/head(subdf$close, 1)*100, 2)
      
    }
    p4 <- p3 +  annotate("text", x = df[, max(x), by  = week][, V1]-20, y = max(df$close), label = per,size = 2.5)
    
    p5 <- qplot(x=x,xend=x,y=0,yend=volume_quote,data=df,geom="segment", color = candle_type)+
      scale_color_manual(values=c("green", "red"))+
      theme(legend.position = "none", axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    
    
    print(grid.arrange(p4, p5, 
                       ncol = 1, nrow = 2,
                       top = textGrob(paste0("pair: ", EUR_pairs[i], " Export: ", Sys.time()) ,gp=gpar(fontsize=20,font=3))))
    
    
  }, error = function(e){
  })
  print(i/length(EUR_pairs))
  # print(paste0("Sharpe Ratio for: ",  EUR_pairs[i]," ", round(unique(df$sharpe), 4))) 
  Sys.sleep(1.1)
  
}
dev.off()


# df %>%
#   ggplot(aes(x = Date_POSIXct, y = close)) +
#   geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#   geom_ma(color = "darkgreen") +
#   coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#                ylim = c(75, 125))


# df %>%
#   ggplot(aes(x = x, y = close)) +
#   geom_candlestick(aes(open = open, high = high, low = low, close = close),fill_up ="green3", fill_down ="red" ,colour_up = "green3", colour_down = "red") +
#   geom_hline(yintercept = RS, linetype ="solid", color = "grey3", size = 0.3)+
#   geom_hline(yintercept = SP, linetype="solid", color = "turquoise2", size = 0.3)+
#   geom_hline(yintercept = tail(df$close, 1), linetype ="twodash", color = "black", size = 0.3)+
#   scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
#   geom_vline(xintercept = splits[, tops] , linetype="dotted", color = "black", size = 0.3)+ theme_classic()#+
# # coord_x_date(xlim = c(100, 200))
#   
# library(emayili)
# library(magrittr)
# 
# email <- envelope()
# 
# email <- email %>%
#   from("@gmail.com") %>%
#   to("@gmail.com")
# 
# email <- email %>% subject("Hourly report")
# # email <- email %>% text("Hello!")
# # path_alerts <- "/media/chris/DATA/Documents/Bot_Trading/Alerts"
# 
# email <- email %>% attachment(list.files(path_alerts, full.names = T))
# 
# 
# smtp <- server(host = "smtp.gmail.com",
#                port = 465,
#                username = "@gmail.com",
#                password = "!")
# smtp(email, verbose = TRUE)
# 
# print(email, details = TRUE)
# 
