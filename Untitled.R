# 총자산배분 : t_class
# 계좌별 자산배분 : t_comm4
# 상품별 보유현황1 : t_comm
# 상품별 보유현황2 : t_comm2

# 자산군별 손익현황 : t_comm3
# 계좌별 손익현황 : t_comm5
# 상품별 손익현황 : comm_profit


self <- MyAssets$new('karekano85!@')
self$run_book()
self$run_valuation()


df1 <- self$bs_pl_mkt_a %>% 
  filter(통화=='원화') %>% 
  bind_rows(self$bs_pl_mkt_p) %>% 
  mutate(
    is_target = (자산군 == '외화자산' & 세부자산군 == '달러자산'),
    자산군 = if_else(is_target, '주식', 자산군),
    세부자산군 = if_else(is_target, '선진국', 세부자산군),
    세부자산군2 = if_else(is_target, '종목', 세부자산군2)
  ) %>% 
  select(-is_target)

summ_fun <- function(df){
  df %>% 
    summarise(
      장부금액=sum(장부금액),
      평잔 = sum(평잔),
      비용 = sum(비용),
      평가금액=sum(평가금액),
      평가손익=sum(평가손익),
      실현손익=sum(실현손익),
      평가손익증감=sum(평가손익증감),
      총손익=sum(총손익), 
      .groups = 'drop'
    )
}


df2 <- df1 %>% 
  group_by(자산군,세부자산군,세부자산군2) %>% 
  summ_fun()

df3 <- df1 %>% 
  group_by(자산군, 세부자산군) %>% 
  summ_fun() %>% 
  mutate(세부자산군2="", .after=2)

df4 <- df1 %>% 
  group_by(자산군) %>% 
  summ_fun() %>% 
  mutate(세부자산군="", 세부자산군2="", .after = 1)

df5 <- df1 %>%
  summ_fun() %>% 
  mutate(자산군="<합계>", 세부자산군="", 세부자산군2="",.before=1)
  
self$t_comm3 <- bind_rows(df2,df3,df4,df5)%>%
  mutate(
    자산군 = factor(자산군, levels = self$class_order),
    세부자산군 = factor(세부자산군, levels = self$class2_order),
    세부자산군2 = factor(세부자산군2, levels = self$class3_order),
    비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
    실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
    평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
    총수익률 = 실현수익률 + 평가증감률
    ) %>% 
  select(-비용) %>% 
  arrange(자산군, 세부자산군, 세부자산군2)

df7 <- df1 %>% 
  group_by(계좌, 자산군) %>% 
  summ_fun()

df8 <- df1 %>% group_by(계좌) %>% 
  summ_fun() %>% 
  mutate(자산군='', .after=1)
  
self$t_comm <- bind_rows(df7,df8)%>%
  mutate(
    계좌 = factor(계좌, levels=self$acct_order),
    자산군 = factor(자산군, levels = self$class_order),
    비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
    실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
    평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
    총수익률 = 실현수익률 + 평가증감률
  ) %>% 
  select(-비용) %>% 
  arrange(계좌, 자산군)



self$read('assets') %>% 
  bind_rows(self$read('pension')) %>% 
  group_by(통화, 자산군, 세부자산군, 세부자산군2) %>% 
  summarise(기초평가손익=sum(기초평가손익,na.rm=T))

self$read_obj('return') %>% 
  filter(기준일=='2024-12-31') %>% 
  collect()


df1 %>% 
  filter(자산군=='주식',세부자산군=='선진국',세부자산군2=='테마')
  