source('functions.R')

self <- MyAssets$new()

table <- 'assets'
acct <- '불리오'
cur <- '달러'

df <- ma$get_trading_record(table, acct, cur)

get_assets_trade() |> names()

names(df2)

md <- MyData$new('mydata.sqlite')


ma$get_trading_record(table, acct, cur)

get_trading_record <- function(table, acct, cur){
  df1 <- md$read(table)
  df2 <- md$read(paste0(table,'_daily'))
  
  df2 |> left_join(
    (df1 |> transmute(계좌, 통화, 종목코드, 종목명)), 
    by = '종목코드') |> 
    mutate(매입비용 = 현금지출-매입액,
           매매수익 = 매도액 - 매도원금,
           매도비용 = 매도액 + 이자배당액 - 현금수입,
           순수익 = 매매수익 + 이자배당액 - 매도비용 - 매입비용,
           순현금수입 = 입출금 + 현금수입 - 현금지출,
           잔액 = cumsum(순현금수입)) |> 
    select(거래일자, 계좌, 통화, 종목코드, 종목명, 매입수량:순현금수입) |> 
    mutate(across(매입수량:입출금, ~if_else(is.na(.x),0,.x))) |> 
    filter(계좌==acct, 통화==cur) |> 
    arrange(계좌, 통화, 거래일자) 
}

df4 <- get_assets_trade('assets')
  
df4 <- df3 |> filter(계좌 == '불리오', 통화 == '달러') |> 
  mutate(현금잔고 = cumsum(순현금수입))

df1 <- df1 |> mutate(행번호=row_number(), .before=1)
df2 <- df2 |> mutate(행번호=row_number(), .before=1)
df1
