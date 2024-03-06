md <- MyData$new('mydata.sqlite')

get_assets_trade()

get_assets_trade <- function(){
  df1 <- md$read('assets')
  df2 <- md$read('assets_daily')
  
  df2 |> 
    left_join(
      (df1 |> transmute(종목코드,종목명,계좌,통화)),
      by = '종목코드') |> 
    mutate(매입비용 = 현금지출-매입액,
           매매수익 = 매도액 - 매도원금,
           매도비용 = 매도액 + 이자배당액 - 현금수입,
           순수익 = 매매수익 + 이자배당액 - 매도비용 - 매입비용,
           순현금수입 = 입출금 + 현금수입 - 현금지출) |> 
    arrange(계좌, 통화, 거래일자)
}
  
df4 <- df3 |> filter(계좌 == '불리오', 통화 == '달러') |> 
  mutate(현금잔고 = cumsum(순현금수입))
