get_balance <- function (url, key, secret) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  ??RCurl::base64Encode(sign))
  curl <- RCurl::getCurlHandle(useragent = "whatever")
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  return(query_result)
}

