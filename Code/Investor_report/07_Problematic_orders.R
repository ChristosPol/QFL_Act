# Preamble ---------------------------------------------------------------------
rm(list = ls())

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))


csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)
orders <- orders[PAIR != "PLAUSD"]


View(orders[STATUS_BUY == "CLOSED" & STATUS_SELL ==""])
