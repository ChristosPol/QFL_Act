current_avail_funds <- function(){
  usd_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                             key= API_Key,
                             secret = API_Sign)
  usd_balance <- usd_balance$result$ZUSD
  Sys.sleep(1)
  open_ord <- myfun(url = "https://api.kraken.com/0/private/OpenOrders",
                    key= API_Key,
                    secret = API_Sign)
  df <- open_ord$result$open %>%
    enframe() %>%
    unnest_wider(value)
  
  df$type_der <- unlist(lapply(df$descr, "[", "type"))
  df$price_der <- unlist(lapply(df$descr, "[", "price"))
  df$pair_der <- unlist(lapply(df$descr, "[", "pair"))
  df_buy <- subset(df, df$type_der == "buy")
  
  res <- as.numeric(usd_balance) - sum(as.numeric(df_buy$price_der)*as.numeric(df_buy$vol))
  return(res)
 
}