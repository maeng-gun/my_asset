library(R6)

exam <- R6Class(
  classname = 'exam',
  public=list(
    a=1,
    initialize=function(){
      self$b = self$a+2
    }
  )
)

ex1 <- exam$new()
ex1$b

exam$b

# 선언을 생략한 R6 클래스 정의
MyClass <- R6Class(
  "MyClass",
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