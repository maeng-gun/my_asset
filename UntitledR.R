library(R6)
library(glue)

get_config <- function(){
  yaml::read_yaml(file = 'config.yaml', 
                  readLines.warn = F)
}


AutoInvest <- R6Class(
  classname = 'AutoInvest',
  public=list(
    # 속성 설정
    APP_KEY=NULL, APP_SECRET=NULL, ACCT=NULL, 
    URL_BASE=NULL, MY_AGENT=NULL, base_headers=NULL,
    token_headers=NULL,
    
    # 속성 초기화
    initialize=function(account="my"){
      cfg <- get_config()
      token_tmp <- paste0("KIS", account)
      
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