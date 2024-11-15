rm(list = ls())
options(scipen = 999)
library(ggrepel)
library(stringr)
# print(paste0("#1 Script initiated at: ",Sys.time()))

# Source functions
path_source <- "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

coin_equity_previous <- read.csv(file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/coin_equity.csv")
coin_equity_previous$X <- NULL
coin_equity_previous$date <- as.Date(coin_equity_previous$date)

usd_equity_previous <- read.csv(file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/usd_equity.csv")
setDT(usd_equity_previous)
# usd_equity_previous[, date := as.Date(date, format = "%m/%d/%y")]
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
# LSK is not returned in this list, ask support
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]

all_pairs[, coin := gsub("ZUSD|USD", "", PAIR)]
all_pairs[PAIR == "CHZUSD", coin := "CHZ"]
all_pairs[coin == "BL", coin := "BLZ"]
all_pairs <- all_pairs[!PAIR %in% c("TUSDUSD", "USDTZUSD")]
all_pairs[coin == "XT", coin := "XTZ"]


avail <- merge(avail, all_pairs, by = "coin", all.x = T)

avail <- avail[!coin%in% avail$coin[grep("*\\.S", avail$coin)]]


# avail[coin %in% c("CHZ", "USDT", "ZUSD"), PAIR:= c("CHZUSD", "USDTZUSD", "ZUSD")]

avail[coin == "XXDG", PAIR := "XDGUSD"]

avail <- avail[coin != "PLA"]

tb <- jsonlite::fromJSON("https://api.kraken.com/0/public/Ticker")
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
avail <- merge(avail, price_info, by = "PAIR", all.x = T)
avail[coin %in% c("ZUSD"), PRICE:= c(1)]
avail[, equity := PRICE*V2]


usd_equity <- data.table(date =Sys.Date(), equity = sum(avail$equity, na.rm=T))
coin_equity <- avail[, list(equity = sum(equity, na.rm=T)), by = coin]
coin_equity <- coin_equity[, date := Sys.Date()]

usd_equity <- rbind(usd_equity_previous,usd_equity)
coin_equity <- rbind(coin_equity_previous,coin_equity)


sd = min(usd_equity$date)
ed = max(usd_equity$date)
calendar <- data.table(calendar_time= seq(sd, ed, "days"), flag = 1)
usd_equity <- merge(calendar, usd_equity, by.x = "calendar_time", by.y = "date", all.x = T)
usd_equity$equity <- na.locf(usd_equity$equity)
usd_equity$flag <- NULL
setnames(usd_equity, "calendar_time", "date")

ggplot(data=usd_equity, aes(x = date, y= equity))+
  geom_line(colour = "black")+
  geom_point(colour = "red")+
  theme_bw()
ggplot(data=usd_equity[date>"2024-05-17"], aes(x = date, y= equity))+
  geom_line(colour = "black")+
  geom_point(colour = "red")+
  theme_bw()

print(usd_equity)
setDT(coin_equity)
coin_equity <- coin_equity[coin != "ZUSD"]
# usd_equity <- usd_equity[date != "2024-07-16"]

write.csv(coin_equity, file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/coin_equity.csv")
write.csv(usd_equity, file= "/Users/christospolysopoulos/Repositories/Private/QFL_Act/Data/equity/usd_equity.csv")

