# Trading platform QFL_DCA

![4%](https://progress-bar.dev/4/?scale=500)
![4%](https://progress-bar.dev/420/?scale=500&title=funds&width=120&color=babaca&suffix=$)
![4%](https://progress-bar.dev/4/?scale=500)

## Centralised exchange
[Kraken](https://www.kraken.com) is a United States–based cryptocurrency exchange, founded in 2011. It was one of the first bitcoin exchanges to be listed on Bloomberg Terminal and is reportedly valued at US$10.8 billion, as of mid-summer 2022.
Kraken is hosting a [REST API](https://docs.kraken.com/rest/) that is organised into publicly accessible endpoints (market data, exchange status, etc.), and private authenticated endpoints (trading, funding, user data) which require requests to be signed.

## Software
The entirety of the trading platform is been set up in R. There are no available libraries/packages in R to manage Kraken API, so everything needed to be coded from scratch.
Workflow of the trading platform:
- Market data - Asset info, Ticker, OHLC, Historical raw trades (Public API endpoints)
- User data - Account, Open/Closed Orders info, Balance, Trades (Private API endpoints)
- User trading - Add/Cancel orders (Private API endpoints)
- Vectorised back-testing and optimized parameters
- Scheduling processes with cron jobs (manage of rate limits)
- Live trading and evaluation of strategy

## Strategy details
This strategy aims to take advantage of two different aspects of the cryptocurrency markets. First is to identify bases (complicated support levels) and apply a grid of buy limit orders DCA-ing its way back to mean (market reaction), and the second one is to take advantage of an inefficient market state on low liquidity
coins and thin order books.
The platform can trade all available USD pairs (>200) at the same time, and it can be extended to more quotes (BTC, ETH, USDT, CHF, EUR, etc).
This strategy strongly depends on available funds to increase the possibility of entering trades. It could be possibly scaled up to 150000 USD.

## Example trades:
![Alt text](/Images/2.png?raw=true)
![Alt text](/Images/3.png?raw=true)
![Alt text](/Images/4.png?raw=true)
![Alt text](/Images/5.png?raw=true)

## Performance
The first test live trading session started on Oct 13, 2022 and is running to this day. Process is active on a local machine, but the plan is to utilize a server environment in the future. Initial funds were 1150 USD which were used to trade minimum order volumes for all available USD pairs.

| Column      | Value |
| ----------- | ----------- |
|Start|2022-10-13|
|End|2024-04-21|
|Initial funds available (USD)|1150|
|Returns in USD|2438|
|Returns in %|211.9|
|N trades|5186|
|Wins|5139|
|Losses|47|
|Win ratio|99.1|
|Average win %|10.1|
|Biggest win %|177|
|Biggest loss %|-55.3|
|Days trading|555|
|Average n trades per day|9.34|


![Alt text](/Images/dce.png?raw=true)
![Alt text](/Images/alpha.png?raw=true)