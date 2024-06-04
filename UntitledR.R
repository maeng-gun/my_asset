library(tidyquant)
library(editData)
library(datapasta)
library(ecos)
source('functions.R')

e <- Ecos$new()

ma <- MyData$new('mydata.sqlite')

scrap_econ_daily <- function(start=NULL, end=NULL){
  
  if(is.null(start)){
    start <- ma$read_obj('econ_daily') %>% 
      distinct(date) %>% 
      dbplyr::window_order(desc(date)) %>% 
      filter(row_number()==1) %>% pull() %>% -10
  }
  
  if(is.null(end)){end <- today()}
  
  info <- ma$read('idx_info')
  
  df <- info %>% 
    filter(site=='ecos') %>%
    select(stat_code, item_code, cycle, index) %>%
    pmap_dfr(
      ~tryCatch({
        statSearch(stat_code = ..1,
                  item_code1 = ..2,
                  cycle = ..3,
                  start_time = strftime(start,"%Y%m%d"),
                  end_time = strftime(end,"%Y%m%d")
        ) %>% 
          as_tibble() %>% 
          transmute(date = ymd(time), index = ..4, value = data_value)
        }, error=function(e){
          tibble()
        }
      )
    )

  get_code <- list('fred' = c('economic.data', 'price'),
                   'yahoo' = c("stock.prices", 'close'))
  
  
  df2 <- info %>% 
    filter(site!='ecos') %>%
    select(item_code, site, index) %>% 
    pmap_dfr(
      ~tryCatch({
          tq_get(..1, 
                 get = get_code[[..2]][1], 
                 from = start,
                 to = end,
          ) %>% 
            as_tibble() %>% 
            transmute(date = ymd(date), index = ..3, value= .data[[get_code[[..2]][2]]])
      }, error=function(e){
          tibble()
      }
      )
    )
  
  bind_rows(df,df2) %>% 
    filter(!is.na(value))
}

upsert_econ <- function(record, table){
  dbxUpsert(ma$con, table, record, c("date", "index"))
}

scrap_econ_daily() %>% 
  upsert_econ('econ_daily')






query_econ <- function(stats, start=NULL, end=NULL){
  
  if(is.null(start)){start <- '2000-01-01'}
  if(is.null(end)){end <- today()}
  
  ma$read_obj('econ_daily') %>% 
    filter(between(date, start, end)) %>% 
    right_join(
      ma$read_obj('idx_info') %>% 
        filter(new_name %in% stats) %>% 
        select(index, new_name),
      by='index'
    ) %>% 
    select(date, new_name, value) %>% 
    collect()
}

library(timetk)

day_1m <- today() %-time% '1 month'
day_5y <- floor_date(today() %-time% '5 year', "year")
ytd <- floor_date(today(),"year")




query_econ("ktr10")

query_econ("ktr10") %>% 
  ggplot(aes(x=date, y=value, color=new_name)) +
  geom_line() +
  labs(x='', y='') +
  theme(legend.position = 'none')




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


