rm(list = ls())
library(ggrepel)
library(stringr)
# print(paste0("#1 Script initiated at: ",Sys.time()))

# Source functions
path_source <- "/Users/christos.polysopoulos/Repositories/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


coin_equity_previous <- read.csv(file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/coin_equity.csv")
coin_equity_previous$X <- NULL
coin_equity_previous$date <- as.Date(coin_equity_previous$date)
usd_equity_previous <- read.csv(file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/usd_equity.csv")
usd_equity_previous$X <- NULL
usd_equity_previous$date <- as.Date(usd_equity_previous$date)
usd_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                           key= API_Key,
                           secret = API_Sign)
avail <- data.table(coin = names(unlist(usd_balance$result)),
                    as.numeric(unlist(usd_balance$result)))
avail <- avail[V2 != 0]

url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]
all_pairs[, coin := gsub("ZUSD|USD", "", PAIR)]
all_pairs[coin == "BL", coin := "BLZ"]
all_pairs <- all_pairs[!PAIR %in% c("TUSDUSD", "USDTZUSD")]

avail <- merge(avail, all_pairs, by = "coin", all.x = T)


avail <- avail[!coin%in% avail$coin[grep("*\\.S", avail$coin)]]

avail[coin %in% c("CHZ","KFEE", "USDT", "ZUSD"), PAIR:= c("CHZUSD","KFEE", "USDTZUSD", "ZUSD")]
avail[coin == "XXDG", PAIR := "XDGUSD"]

avail <- avail[coin != "PLA"]
tb <- jsonlite::fromJSON("https://api.kraken.com/0/public/Ticker")
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
avail <- merge(avail, price_info, by = "PAIR", all.x = T)
avail[coin %in% c("KFEE", "ZUSD"), PRICE:= c(0, 1)]
avail[, equity := PRICE*V2]

usd_equity <- data.table(date =Sys.Date(), equity = sum(avail$equity))
coin_equity <- avail[, list(equity = sum(equity)), by = coin]
coin_equity <- coin_equity[, date := Sys.Date()]

usd_equity <- rbind(usd_equity_previous,usd_equity)
coin_equity <- rbind(coin_equity_previous,coin_equity)
ggplot(data=usd_equity, aes(x = date, y= equity))+
  geom_line(colour = "black")+
  geom_point(colour = "red")+
  theme_bw()
print(usd_equity)
setDT(coin_equity)
coin_equity <- coin_equity[coin != "ZUSD"]
ggplot(data=coin_equity, aes(x = date, y= equity, colour = coin))+
  # geom_point(alpha= 0.3)+
  # geom_label_repel(data =coin_equity[date == max(date)],  aes(label = coin),
  #                  nudge_x = 1,
  #                  na.rm = TRUE, max.overlaps = 10)+
  geom_line(alpha= 0.3)+
  theme_bw()+ theme(legend.position = "none")

write.csv(coin_equity, file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/coin_equity.csv")
write.csv(usd_equity, file= "/Users/christos.polysopoulos/Repositories/QFL_Act/Data/equity/usd_equity.csv")

