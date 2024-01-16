library(R6)
library(glue)

get_config <- function(){
  yaml::read_yaml(file = 'config.yaml', 
                  readLines.warn = F)
}


AutoInvest <- R6Class(
  classname = 'AutoInvest',
  public=list(
    
    # 속성 선언
    token_tmp=NULL, APP_KEY=NULL, APP_SECRET=NULL, ACCT=NULL, 
    URL_BASE=NULL, MY_AGENT=NULL, base_headers=NULL,
    token_headers=NULL,
    
    # 속성 초기화
    initialize = function(account="my"){
      cfg <- get_config()
      self$token_tmp <- paste0("KIS", account)
      
      if (!file.exists(token_tmp)) {
        file.create(token_tmp)
      }
      
      self$APP_KEY <- cfg[[paste0(account, '_app')]]
      self$APP_SECRET <- cfg[[paste0(account, '_sec')]]
      self$ACCT <- cfg[[paste0(account, '_acct')]]
      self$URL_BASE <- cfg$prod
      self$MY_AGENT <- cfg$agent
      self$base_headers <- list(
        "Content-Type" = "application/json",
        "Accept" = "text/plain",
        "charset" = "UTF-8",
        'User-Agent' = self$MY_AGENT
      )
      self$token_headers <- list(
        # "authorization" = paste0("Bearer ", self$auth()),
        "appkey" = self$APP_KEY,
        "appsecret" = self$APP_SECRET,
        self$base_headers
      )
    },
    
    #메서드(1)
    save_token = function(my_token, my_expired, token_tmp) {
      valid_date <- 
        as.POSIXct(my_expired, format='%Y-%m-%d %H:%M:%S', tz='UTC')
      writeLines(c(paste('token:', my_token),
                   paste('valid-date:', 
                         format(valid_date, '%Y-%m-%d %H:%M:%S'))),
                 self$token_tmp)
    }
    
    read_token <- function(token_tmp) {
      tryCatch({
        # 토큰이 저장된 파일 읽기
        tkg_tmp <- yaml::read_yaml(token_tmp, encoding='UTF-8')
        
        # 토큰 만료 일,시간
        exp_dt <- format(as.POSIXct(tkg_tmp$`valid-date`), '%Y-%m-%d %H:%M:%S')
        # 현재일자,시간
        now_dt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        # 저장된 토큰 만료일자 체크 (만료일시 > 현재일시 인경우 보관 토큰 리턴)
        if (exp_dt > now_dt) {
          return(tkg_tmp$token)
        } else {
          cat('Need new token: ', tkg_tmp$`valid-date`, '\n')
          return(NULL)
        }
      }, error = function(e) {
        cat('read token error: ', e, '\n')
        return(NULL)
      })
    }
    
    
  )
)


cfg <- get_config()

cfg['prod']

self <- AutoInvest$new()

self$base_headers

self$cfg

ex1 <- exam$new()
ex1$b

exam$b

# 선언을 생략한 R6 클래스 정의
MyClass <- R6Class(
  class"MyClass",
  public = list(
    a=1,
    # 메서드
    initialize = function() {
      self$a <- 1
      self$b <- self$a + 1
    }
  )
)

# 객체 생성
my_object <- MyClass$new()

MyClass
my_object

# 속성 확인
print(my_object$a)  # 1
print(my_object$b)  # 