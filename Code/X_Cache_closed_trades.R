# Preamble ---------------------------------------------------------------------
rm(list = ls())

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

# load("trades/trades.Rdata")
# df_hist <- copy(df)
# rm(df)
key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

i <- 1
trades_raw <- list()
namen <- list()
dfs <- list()
while (offset <= 1000) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  trades_raw <- lapply(trades_raw[[i]]$result$closed, function(x){x$descr <- NULL;x})
  namen[[i]] <- names(trades_raw)
  dfs[[i]] <- rbindlist(trades_raw, fill = T)
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}


df <- rbindlist(dfs, fill = T)
df[, ids := unlist(namen)]
df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, .(ids, opentm, closetm, vol, vol_exec, cost, fee, price)]
df <- rbind(df_hist, df[ids %in% df$ids[!df$ids %in% df_hist$ids],])

save(df, file ="trades/trades.Rdata")
