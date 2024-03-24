source('functions.R')













self <- MyAssets$new()

md <- MyData$new('mydata.sqlite')


self$bs_pl_book_a %>% 
  group_by(거래일자) %>% 
  summarise(장부금액 = sum(장부금액)) %>% 
  left_join(
    self$bs_pl_book_a %>% 
      filter(자산군=='현금성', 통화=='원화') %>% 
      group_by(거래일자) %>% 
      summarise(현금성 = sum(장부금액)),
    by = '거래일자'
  ) %>% 
  filter(거래일자 <= today())
