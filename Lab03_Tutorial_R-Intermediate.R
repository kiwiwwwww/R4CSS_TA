# purl("/Users/dennistseng/Documents/R1092/Lab03_Tutorial_R-Intermediate.Rmd")

library(dplyr)

## ---- message=F,warning=F,results=F-------------------------------------
n_quota <- 10;n_apply <- 20
if (n_quota > n_apply) {
  print("供 > 需")
} else if (n_quota == n_apply) {
  print("供 = 需")
} else {
  print("供 < 需")
}

## ----warning=F,message=F, results=F-------------------------------------
# +, -, x, /; 加減乘除
1+1 > 1
# %%, %/%; 餘數與商數
11 %% 3
11 %/% 3
# >, <, ==, !=
3 == 2
3 != 2

## ----warning=F,message=F, results=F-------------------------------------
# &, |: for 向量
c(1,3,-4) > 0 & c(-1,2,4) < 0
c(1,3,-4) > 0 | c(-1,2,4) < 0 

# &&, ||: for 單一值
2 > 0 && -1 < 0
3 > 0 || 1 < 0 

c(1,3) > 0 && c(-1,2) < 0
c(1,3) > 0 && c(2,-1) < 0

# %in%, ==
3 %in% c(1,2,3,4,5) 
c(1,3,6) %in% c(1,2,3,4,5) 
c(1,3,6) == c(1,2,3,4,5) 


## ----warning=F,message=F,eval=F-----------------------------------------
print_month_bad <- function(input_date) {
  input_date <- as.Date(input_date)
  print(months(input_date))
}

print_month_bad("2021-03-09")
print_month_bad(20210309)


## ----warning=F,message=F,eval=F-----------------------------------------
print_month_good <- function(input_date) {
  if(!is.character(input_date)) {
    print("請依照 '1997-01-01' 格式輸入")
  } else {
    input_date <- as.Date(input_date)
    print(months(input_date))
  }
}

print_month_good("2021-03-09")
print_month_good(20210309)


## ----warning=F,message=F,eval=F-----------------------------------------
# ok
print_month_good("2021-03-09")
print_month_good(20210309)
# not ok
print_month_good("20210309")


## ----warning=F,message=F,eval=F-----------------------------------------
library(lubridate)
print_month_better <- function(input_date) {
  input_date <- lubridate::as_date(input_date)
  if(is.na(input_date)) {
    print("請依照 '1997-01-01' 格式輸入")
  } else {
    print(lubridate::month(input_date))
  }
}
# ok
print_month_better("2021-03-09");print_month_better("20210309")
print_month_better("420210309")
# not ok...
print_month_better(20210309)


## ----warning=F,message=F------------------------------------------------
library(lubridate)
ymd("20110604");mdy("06-04-2011");dmy("04/06/2011");
mdy("May 11, 1996");mdy("September 12  2001");dmy("1/July/1988")


## ----message=F, warning=F-----------------------------------------------
vec <- c(1,4,9)
for (i in 1:length(vec)) {
  print(vec[i] + 10)
}


## ----message=F, warning=F, eval=F---------------------------------------
vec <- c(1,4,9)
for (i in 1:length(vec)) {
  print(vec[i] + 10)
}


## ----message=F, warning=F, eval=F---------------------------------------
vec <- c(1,4,9)
for (i in 1:length(vec)) {
  if(vec[i] %% 2 == 1) {
    print("good")
  } else {
    print("bad")
  }
}


## ----message=F, warning=F, eval=F---------------------------------------
vec_01 <- c(1,4,9)
vec_02 <- c(2,4)
for (i in 1:length(vec_01)) {
  for (j in 1:length(vec_02)) {
    if(vec_01[i]-vec_02[j] %% 2 == 1) {
      print("good")
    } else {
      print("bad")
    }
  }
}


## ----message=F, warning=F, eval=F---------------------------------------
x <- 1
y <- 5
while (x <= y) {
  print("good")
  x <- x + 1
}


## ----message=F, warning=F, eval=F---------------------------------------
now <- 1
destination_A <- 8;destination_B <- 9
while (now <= destination_B) {
  if(now == 4) {
    now <- now + 1
    next
  }
  else {
    if(now == destination_A) {break}
    print(stringr::str_c("電梯", now, "樓"))
  }
  now <- now + 1
}



## ----message=F, warning=F-----------------------------------------------
ten_time_add_two <- function(x){
  y = x * 10 + 2
  print(y)
}

ten_time_add_two(1)


## ----message=F, warning=F-----------------------------------------------
bad_reply <- function(name_seller){
  value_bad <- "真的很差勁"
  stringr::str_c(name_seller, value_bad)
}

bad_reply("賣家b04701103")
bad_reply(c("賣家b04701103","賣家r09342011","賣家ooxx"))

## ----message=F, warning=F-----------------------------------------------
bad_reply <- function(name_seller, score = 1, value_bad = "真的很差勁"){
  if(!is.integer(score)){
    print("請重新輸入: score 為 1 - 5 的正整數")
  } else {
  stringr::str_c(name_seller, value_bad, " 怒給", score, "星")
  }
}

bad_reply("賣家b04701103", score = 2)
bad_reply("賣家r09342011", "沒有誠信")
bad_reply("賣家ooxx", score = 1.2)
bad_reply("賣家ooxx", score = "爛")


## ----message=F, warning=F-----------------------------------------------
y = 1
add_five <- function(x){y = x + 5; return(y)}
add_five(3)
y


## ----message=F, warning=F, results=F------------------------------------
df_head <- head(iris, 1)
df_several <- iris[c(1:3), ]
df_last <- tail(iris, 1)
rbind(df_head, df_last)
rbind(df_head, df_several)


## ----message=F, warning=F, results=F------------------------------------
df_head <- slice_head(iris, n= 1)
df_several <- slice(iris, 1:3)
df_last <- slice_tail(iris, n= 1)
bind_rows(df_head, df_last)


## ----message=F, warning=F, results=F------------------------------------
iris_c12 <- iris[ ,c("Sepal.Length","Sepal.Width")]
iris_c34 <- iris[ ,c(3:4)]
iris_c1234 <- cbind(iris_c12,iris_c34)


## ----message=F, warning=F, results=F------------------------------------
iris_c12 <- select(iris, Sepal.Length, Sepal.Width)
iris_c34 <- select(iris, 3, 4)
iris_c1234 <- bind_cols(iris_c12,iris_c34)

