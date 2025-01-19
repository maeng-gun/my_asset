source('functions.R')

library(httr)
library(timetk)
library(plotly)

self <- Scrap_econ$new()

df <- self$scrap_daily(start = '2008-12-16', 
                       end = '2024-06-10',item = '미국기준금리')

df %>% self$upsert_econ('econ_daily')

scrap_daily() %>% upsert_econ('econ_daily')


stats <- "금 선물"

c('미국기준금리','미국채(10년)') %>% 
  self$plot_time_series("2020", "2024")

a <- c('K55301BU3904', 'K55105BU1096', 'K55105BV3794')

bs_pl <- self$bs_pl_book_p |> 
  filter(거래일자 == self$today) |> 
  left_join(price, by=c("계좌","종목코드")) %>% 
  left_join(
    self$ks$stock_list %>% 
      select(종목코드, 종가),
    by='종목코드'
  ) %>% 
  filter(평잔!=0) %>% 
  mutate(
    장부금액 = if_else(장부금액<1, 0, 장부금액))

bs_pl %>% 
  left_join(
    bs_pl %>% 
      filter(str_sub(종목코드,1,2)=='K5') %>% 
      select(종목코드) %>% 
      mutate(
        기준가 = get_fund_price(종목코드)
      ),
    by='종목코드'
  ) %>% 
  mutate(
    평가금액 = case_when(
      !is.na(평가금액) ~ 평가금액,
      !is.na(종가) ~ 종가*보유수량,
      !is.na(기준가) ~ 기준가*보유수량/1000,
      TRUE ~ 장부금액),
    평가손익증감 = 평가금액 - 장부금액,
    운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
    평가손익 = 평가금액 - 장부금액,
    평가수익률 = 평가손익 / 장부금액 * 100
  ) %>% 
  select(-종가) %>% 
  arrange(desc(통화), desc(평가금액))






'K55301B96890' %>% 


day_1m <- today() %-time% '1 month'
day_5y <- floor_date(today() %-time% '5 year', "year")
ytd <- floor_date(today(),"year")


df <- query_econ('krweur')
query_econ




plotlyOutput()

query_econ('krweur') %>% 
  tail(50)
query_econ(c('krweur', 'krwusd')) %>% 
  ggplot(aes(x=date, y=value, color=new_name)) +
  geom_line(linewidth=1) +
  labs(x='', y='')

self$read_obj('idx_info') %>% pull(item_name)
  
c('미국기준금리') %>% 
  self$query_daily() %>% 
  filter(between_time(date, "2020","2024")) %>% 
  group_by(item_name) %>% 
  plot_ly(x = ~date, y = ~value, color=~item_name, colors = RColorBrewer::brewer.pal(3, "Set2")) %>% 
  add_lines() %>% 
  layout(showlegend = T,
         xaxis = list(rangeselector=list(
                        buttons=list(
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=3, label="3m", step="month", stepmode="backward"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward")
                        ))))


bind_rows(df2,df3) %>% 
  left_join(ma$read('idx_info') %>% select(index, new_name), by="index") %>% 
  pivot_wider(id_cols = date, names_from=new_name, values_from=value)




a <- function(x){
  df2 %>% transmute(value = .data[[x]])
}



tq_get("GC=F", from="2024-05-14")

vignette('programming')
df2 %>% transmute_('value' = 'krwcny')

df2

tq_get('DGS5', 
       get = 'economic.data', 
       from = "20240514"
)

  
df2 %>% 
  full_join(df3, by='date')
  
s1$stat_code

ma <- MyData$new('mydata.sqlite')
df <- 
  readRDS('ecos_items.rds') %>% 
  mutate(site = 'ecos', .before=1)

editFiles()

tq_get(c('GC=F', 'SI=F', 'CL=F'))

tq_get('^FVX', get='stock.price', from='2000-01-01')



library(dplyr)
a$get_history(start = '2000-01-01') %>% 
  tibble()


df <- list('DSG3MO', 'DGS1', 'DGS2', 'DGS5', 'DGS10', 'DGS20', 'DGS30') %>% 
  purrr::map_dfr(
    ~tq_get(.x, get='economic.data', from='2024-04-25')
  ) %>% 
  pivot_wider(names_from = symbol, values_from = price)

df

df <- editData(df)

tq_exchange('NYSE')




ecos <- Ecos$new()


self$read_items()$item_name


ecos::statSearch('721Y001')

self$find_items('731Y001')

ecos::ecos.setKey('QNY131XKGR9OTK7LSNLS')

self$read_items()



ui=fluidPage(
  textInput("mydata","mydata",value="mtcars"),
  editableDTUI("editableDT"),
  tableOutput('table')
)

server=function(input,output,session){
  data=reactive({
    myget(input$mydata)
  })
  result=callModule(editableDT, "editableDT", data=data)
  output$table <- renderTable({re})
}
shinyApp(ui=ui,server=server)









ecos::statTableList()
map


df <- self$read_items() %>% 
  split(.$new_name) %>% 
  map_dfr(
    ~ecos::statSearch(stat_code = .x$stat_code, 
                     item_code1 = .x$item_code,
                     start_time = '20230101',
                     cycle = 'D')%>% 
      as_tibble() %>% 
      transmute(code=.x$new_name,
                date=ymd(time),
                value=data_value) 
  )

x <- self$read_items() %>% 
  split(.$new_name) %>% .$ktr10

ecos::statSearch(stat_code = x$stat_code, 
                 item_code1 = x$item_code,
                 start_time = '20200101',
                 cycle = 'D')


df %>% 
  pivot_wider(names_from = 'code', values_from = value) %>% 
  arrange(date) %>% 
  filter(if_all(2, ~!is.na(.)))
  



ecos::statSearch(stat_code =df1$stat_code, 
                        item_code1 = df1$item_code,
                        start_time = strftime(last(df2$date)+1,'%Y%m%d'),
                        cycle = 'D') %>% 
  as_tibble() %>% 
  transmute(code=df1$new_name,
            date=ymd(time),
            value=data_value)



names(df1)


self <- MyAssets$new()
ma$bs_pl_book_a %>% 
  filter(거래일자=='2024-05-01')

md <- MyData$new('mydata.sqlite')

md$read('inflow')


library(ggplot2)

df1 <- self$bs_pl_book_a %>% 
  group_by(거래일자) %>% 
  summarise(장부금액 = sum(장부금액)) %>% 
  left_join(
    self$bs_pl_book_a %>% 
      filter(자산군=='현금성', 통화=='원화') %>% 
      group_by(거래일자) %>% 
      summarise(현금성자산 = sum(장부금액)),
    by = '거래일자'
  ) 

y <- df1 %>% filter(거래일자 == today()-1)

inflow_table <- 
  self$md$read('inflow') %>% 
  filter(거래일자 > today())

df2 <- df1 %>% 
  filter(거래일자 >= today()) %>%
  select(거래일자) %>% 
  left_join(
    inflow_table, 
    by='거래일자') %>% 
  replace_na(list(순자금유입=0, 만기상환=0)) %>% 
  mutate(across(-거래일자, cumsum)) %>% 
  transmute(
    거래일자,
    장부금액 = y$장부금액+순자금유입,
    현금성자산 = y$현금성+순자금유입+만기상환)

df <- df1 %>% 
  filter(거래일자 < today()) %>% 
  bind_rows(df2)


self$inflow_plot <- df %>% 
  pivot_longer(cols=-거래일자, names_to = '구분', values_to = '금액') %>% 
  mutate(금액 = 금액/10000) %>% 
  ggplot(aes(x=거래일자,y=금액, color=구분))+
  geom_line(linewidth=2)+
  facet_grid(rows='구분', scales = 'free_y')


#재무제표 수집
source('stocks.R')
ks <- KrxStocks$new()

company = c('롯데케미칼','LG화학')
start = 2000
end = 2023


read_bspl <- function(company, start, end, annual=T){
  xw <- XlWings$new('bspl.xlsm')
  ks$find_code(company)$종목코드 %>% xw$paste('A12', T)
  c(start, end) %>% xw$paste('B7')
  if(annual){freq <- 'Annual'} else {freq <-'All Gross'}
  xw$paste(freq, 'B5')
  xw$refresh()
  xw$ws$range('f10:z10')$copy(xw$ws$range('f11'))
  xw$read_table('a11') %>% 
    rename_with(~stringr::str_sub(., end=-4), ends_with('(원)')) %>% 
    filter(!if_all(-Symbol:-주기, is.na)) %>% 
    select(-Symbol, -결산월, -주기) %>% 
    rename(종목명=Name, 연도=회계년)
}


df <- xw$read_table('a11') %>% 
  rename_with(~stringr::str_sub(., end=-4), ends_with('(원)')) %>% 
  filter(!if_all(-Symbol:-주기, is.na)) %>% 
  select(-Symbol, -결산월, -주기) %>% 
  rename(종목명=Name, 연도=회계년)

df %>% group_by(종목명) %>% 
  transmute(연도, across(-1, timetk::diff_vec, .names = "{.col}_증감")) %>% 
  tidyr::gather('계정명','금액',!c(종목명,연도)) %>% 
  mutate(금액 = 금액/1000000000000) %>% 
  group_by(종목명,연도) %>% 
  arrange(종목명, 연도, desc(abs(금액)))

ks$find_code('롯데케미칼')$종목코드 %>% 
  self$paste('A12')

self$refresh()

self$clear_table('A7')
rep('20240711',4) %>% 
  self$paste('C4')

df$종목코드 %>% 
  self$paste('A7',T)
self$refresh()


df <- self$read_table('A6') %>% 
  setNames(c('종목코드','종목명','대분류','중분류',
             '소분류', '시장구분','시가총액')) %>% 
  arrange(desc(시가총액))


df %>% 
  arrange(desc(시가총액)) %>% 
  group_by(중분류) %>% 
  reframe(종목명 = head(종목명, n=3),
          시가총액 = head(시가총액, n=3)/1000000000000)

df %>% filter(중분류=='미디어')

self$kill()



py$paste(self$ws$range('A7'), df$종목코드, T)


self$stock_list %>% 
  filter(종목코드 %in% c('060310', '005930'))

self <- MyAssets$new()




source("functions.R", echo=F)

self <- MyAssets$new()

self$initialize()

df1 <- self$bs_pl_mkt_a %>% 
  mutate(계정='투자만기') %>% 
  bind_rows(
    self$bs_pl_mkt_p %>% mutate(계정='연금만기')
  )

df2 <- df1 %>% 
  filter(자산군=='채권', 세부자산군=='직접', 
         통화=='원화', 평가금액>0) %>% 
  select(계정, 종목코드, 평가금액) %>% 
  left_join(
    self$assets %>% 
      bind_rows(self$pension) %>% 
      select(종목코드, 만기일),
    by='종목코드'
  ) %>% 
  filter(만기일>=self$today)


last_y <- df2 %>% arrange(만기일) %>% 
  pull() %>% last() %>% year()


df <- tibble(
  거래일자 = seq(self$today, make_date(last_y,12,31), by=1)) %>% 
  left_join(
    df1 %>% 
      filter(자산군=='현금성', 통화=='원화', 평가금액>0) %>% 
      select(거래일자, 계정,평가금액) %>% 
      group_by(거래일자, 계정) %>% summarise(만기상환=sum(평가금액)) %>% 
      bind_rows(
        df2 %>% select(거래일자=만기일, 계정, 만기상환=평가금액)
      )%>% 
      group_by(거래일자,계정) %>% 
      summarise(만기상환=sum(만기상환)) %>% 
      ungroup() %>% 
      spread(계정,만기상환,fill = 0),
    by='거래일자'
  ) %>% 
  left_join(
    self$read('inflow') %>% select(-행번호),
    by='거래일자'
  ) %>% 
  mutate(across(-거래일자, ~replace_na(.,0))) %>% 
  group_by(거래연월=tsibble::yearmonth(거래일자)) %>% 
  summarise(across(-거래일자,sum)) %>% 
  transmute(거래월= tsibble::yearmonth(거래연월) %>% 
              format(format = '%Y-%m') %>% 
              as.character(), 
            투자가용자금=투자만기+투자유출입, 
            연금가용자금=연금만기+연금유출입,
            총가용자금=투자가용자금+연금가용자금)

df3 <- df1 %>% filter(통화=='원화', 자산군!='현금성') %>% 
  group_by(계정) %>% 
  summarise(평가금액=sum(평가금액)) %>% 
  spread(계정, 평가금액)

df4 <- df2 %>% 
  group_by(계정) %>% 
  summarise(만기상환=sum(평가금액)) %>% 
  ungroup() %>% 
  spread(계정,만기상환,fill = 0)

s1 <- sum(df$투자가용자금)
s2 <- sum(df$연금가용자금)
s3 <- df3$투자만기 - sum(df4$투자만기)
s4 <- df3$연금만기 - sum(df4$연금만기)


df %>% 
  add_row(거래월 = c('자금누계', '보유자산', '총자산누계'),
          투자가용자금 = c(s1, s3, s1+s3),
          연금가용자금 = c(s2, s4, s2+s4),
          총가용자금= c(s1+s2, s3+s4, s1+s2+s3+s4))