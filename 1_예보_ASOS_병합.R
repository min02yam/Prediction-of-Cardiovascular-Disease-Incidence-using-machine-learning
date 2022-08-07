# 0.패키지 및 데이터 로드
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/final")

suppressPackageStartupMessages({
  require(tidyverse)
  require(magrittr)
  require(data.table)
require(dplyr)
})

install.packages(c("DataExplorer","imputeTS"))
suppressPackageStartupMessages({
library(DataExplorer) #EDA
library(imputeTS) # imputation
})


f2012= read.csv("f2012.csv")
f2013= read.csv("f2013.csv", fileEncoding="euc-kr")
f2014= read.csv("f2014.csv")
f2015= read.csv("f2015.csv")
f2016= read.csv("f2016.csv")

##################################################
# 1.랭글링
## 1-1.년도병합
예보 = bind_rows(f2012, f2013, f2014, f2015, f2016)
#예보 %>% mutate(date= parse_date_time(date, orders="ymd")) %>% head

## 1-2.hospital 테이블 형식으로 변환
DT= rbind(예보, 예보)
DT %<>% 
  arrange(date, area)  %>% 
  mutate(sex = rep(1:2, times=nrow(DT)/2 ))

## step1. 새로운 date 테이블 만들기 만들기 
new_date= seq(as.Date('20120101', '%Y%m%d'), as.Date('20161231', '%Y%m%d'), 1)
new_date =rep(new_date , times= 17)
new_date= as.data.table(new_date)
colnames(new_date) = "date"
new_date %<>% arrange(date)

## step2. area변수 추가
area_list= c("강원", "경기", "경남", "경북", "광주", "대구",
	"대전","부산","서울","세종","울산","인천",
	"전남", "전북","제주" , "충남","충북")
new_date$area =  rep(area_list, times=nrow(new_date)/17) 

## step3. sex변수 추가 
new_date_dt = rbind(new_date, new_date) #공간만들기
new_date_dt %<>% arrange(date,area) # 순서지정
new_date_dt$sex = rep(1:2,   times=nrow(new_date_dt)/2)

## step4. merge전 타입 변경
new_date_dt[, date := as.character(date)]
DT %<>%as.data.table
#DT[, date  := as.character(date)]


## step5. 최종병합
new_date_dt %>% left_join(DT, by=c("date","area","sex")) -> 예보_최종

# 확인
예보_최종 %>% str
예보_최종 %<>%  mutate(date= parse_date_time(date, orders="ymd"))

##################################################
# 2.결측치 대체

## step1. 확인
예보_최종_na = 예보_최종[!complete.cases(예보_최종),]
예보_최종_na %>% summary
apply(예보_최종, 2, function(x) sum(is.na(x)))

## step2. 칼만필터 알고리즘 적용

예보_최종 %>% group_split(area)  %>%
  lapply(function(x){na_kalman(x, model = "StructTS", smooth = TRUE)}) -> imp_list
rbindlist(imp_list) -> imp_dt
##################################################
# 3. 최종 테이블 병합

#step1. 병합할 데이터 로드
final = fread("ASOS_최종합_최종_최종.csv")
final %>% str

#step2. date변수명 변경
colnames(imp_dt)[1] = "yyyymmdd"

#step3. date 형식 변경
final %<>% mutate(yyyymmdd= parse_date_time(yyyymmdd, orders="ymd"))
new =  final  %>% right_join(imp_dt, by=c("yyyymmdd", "area","sex"))
new %>% str 
####################################################
4. 저장
write.csv(new, "ASOS_예보_최종합.csv", row.names=FALSE)