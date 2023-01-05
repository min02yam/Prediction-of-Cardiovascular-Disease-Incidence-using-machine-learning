# 0.패키지 및 데이터 로드
setwd("C:/Users/alsdu/Downloads/3_기타/2.인구")
suppressPackageStartupMessages({
  require(tidyverse)
  require(magrittr)
  require(data.table)
})



#rm(list=ls())
고령인구비율= read.csv ("C:/Users/alsdu/Downloads/고령인구비율_시도_시_군_구__20220807221446.csv", fileEncoding = "euc-kr")
성별인구수= read.csv ("행정구역_시군구_별__성별_인구수_20220730183620.csv", fileEncoding = "euc-kr")

고령인구비율 %>% head
성별인구수 %>% head

#변수명 변경
names(고령인구비율)= c("area", "yyyymm","고령인구비율")
names(성별인구수) = c("area", "yyyymm","총인구수","남자인구수","여자인구수")

고령인구비율 %>% summary
성별인구수 %>% summary

####################################################################
# 1.파생변수 생성
## 1-1.성별인구수

성별인구수 %<>% as.data.table
#char -> int 형으로 변경
성별인구수[, 총인구수 := as.integer(총인구수)]
성별인구수[, 남자인구수 := as.integer(남자인구수)]
성별인구수[, 여자인구수 := as.integer(여자인구수)]


성별인구수 %>% filter( is.na(총인구수) | is.na(남자인구수) | is.na(여자인구수))

성별인구수 %>% filter(area=='세종특별자치시', yyyymm	=='2012.07')

성별인구수[is.na(총인구수), 총인구수 :=103127]
성별인구수[is.na(남자인구수), 남자인구수 :=52624]
성별인구수[is.na(여자인구수), 여자인구수 :=50503]
# is.na(성별인구수) %>% sum # 확인

성별인구수[, 남자인구비율 := 남자인구수/총인구수*100]
성별인구수[, 여자인구비율 := 여자인구수/총인구수*100]

#지역 인구비율 변수 생성
성별인구수 %>% filter(area=='전국') %>%
  select(yyyymm, 총인구수) -> 전국
성별인구수 %<>% filter(! area=='전국')
성별인구수 = merge(성별인구수,전국, by='yyyymm')
head(성별인구수)

성별인구수[, 월별인구비율 := 총인구수.x/총인구수.y*100]
성별인구수 %>%head


## 1-2.최종변수선택
성별인구수 %<>% select(area, yyyymm, 월별인구비율, 남자인구비율,여자인구비율)

## 1-3. area 변수 맞추기

성별인구수$area %>% table
# area 변수 맞추기
#1. 앞에 세글자 파싱해서
성별인구수 %<>% mutate(area = gsub("특별시", " ", area),
                area= gsub("광역시", " ",area))

성별인구수 %<>% mutate(area = case_when(area == '경상남도'~ '경남',
                          area == '경상북도' ~ '경북',
                          area == '전라남도' ~ '전남',
                          area == '전라북도' ~ '전북',
                          area == '충청남도' ~ '충남',
                          area == '충청북도' ~ '충북',
                          area == '강원도' ~ '강원',
                          area == '경기도' ~ '경기',
                          area == '제주특별자치도' ~ '제주',
                          area == '세종특별자치시' ~ '세종',
                          TRUE~ area)) 

###############################################################################
# 2. 고령인구비율
고령인구비율 %>% head

고령인구비율 %<>% mutate(area = gsub("특별시", " ", area),
                  area= gsub("광역시", " ",area))


고령인구비율 %<>% mutate(area = case_when(area == '경상남도'~ '경남',
                                   area == '경상북도' ~ '경북',
                                   area == '전라남도' ~ '전남',
                                   area == '전라북도' ~ '전북',
                                   area == '충청남도' ~ '충남',
                                   area == '충청북도' ~ '충북',
                                   area == '강원도' ~ '강원',
                                   area == '경기도' ~ '경기',
                                   area == '제주특별자치도' ~ '제주',
                                   area == '세종특별자치시' ~ '세종',
                                   TRUE~ area)) 


#step1. 고령인구비율 세종 2012.1~2012.6월 값 생성(2012년 7월 값으로)

고령인구비율 %>% filter( area == '세종', yyyymm=='2012.07') 

#step2. 임시테이블 생성
고령인구비율 %>% filter( area == '충남') %>% head(6) ->tmp


tmp %>% mutate(area = gsub('충남','세종',area),
               고령인구비율 = rep(16.2, times= nrow(tmp))) -> tmp_세종
#step3. rbind
고령최종 = rbind(고령인구비율, tmp_세종)
  
  고령최종%<>% arrange(yyyymm,area) 
#write.csv(성별인구수, "성별인구수.csv", row.names=FALSE)
#write.csv(고령인구비율, "고령인구비율_최종.csv", row.names=FALSE)
#######################################################################################
#3. 인구 데이터 병합
성별인구수 %<>% as.data.table
고령최종 %<>% as.data.table


#성별인구수$yyyymm = as.character(성별인구수$yyyymm)
#고령인구비율$yyyymm = as.character(고령인구비율$yyyymm)
#성별인구수 $area %>% table


성별인구수$yyyymm = as.character(성별인구수$yyyymm)
고령최종$yyyymm = as.character(고령최종$yyyymm)
#step1.  . 제거
성별인구수$yyyymm =  gsub('[.]','', 성별인구수$yyyymm)
성별인구수 %>% str
#step2. 10월 만들기
성별인구수 %<>% mutate(yyyymm = case_when( yyyymm == '20121' ~ '201210',
                                     yyyymm == '20131' ~ '201310',
                                     yyyymm == '20141' ~ '201410',
                                     yyyymm == '20151' ~ '201510',
                                     yyyymm == '20161' ~ '201610',
                                     TRUE ~ yyyymm
                                     )
                  ) 

#step1.  . 제거
고령최종$yyyymm =  gsub('[.]','', 고령최종$yyyymm)
고령최종%<>% mutate(yyyymm = case_when( yyyymm == '20121' ~ '201210',
                                    yyyymm == '20131' ~ '201310',
                                    yyyymm == '20141' ~ '201410',
                                    yyyymm == '20151' ~ '201510',
                                    yyyymm == '20161' ~ '201610',
                                    TRUE ~ yyyymm)
                    ) 

고령최종 %>% str
성별인구수 %>% str
################################################################################
setkey(성별인구수, yyyymm,area)
setkey(고령최종,yyyymm,area)

인구 = 성별인구수[고령최종] 

인구 %>% str

##################################################################################
#step1. 날짜데이터 형식 변경
인구$ yyyymm = as.character(인구$yyyymm)
인구 %>% mutate( yyyymm =parse_date_time(yyyymm, orders="ym"),
                   year= year(yyyymm),
                   month= month(yyyymm)) ->test
#write.csv(test,"test4.csv", row.names=FALSE)
#write.csv(인구,"인구.csv", row.names=FALSE)

####################################################################################

## step1. 새로운 date 테이블 만들기 만들기 
new_date= seq(as.Date('20120101', '%Y%m%d'), as.Date('20161231', '%Y%m%d'), 1)
new_date =rep(new_date , times= 17)
new_date= as.data.table(new_date)

colnames(new_date) = "yyyymmdd"

new_date %>% mutate( year=year(yyyymmdd),
                     month= month(yyyymmdd)) -> new_date
new_date %<>% arrange(yyyymmdd)

## step2. area변수 추가
area_list= c("강원", "경기", "경남", "경북", "광주", "대구",
             "대전","부산","서울","세종","울산","인천",
             "전남", "전북","제주" , "충남","충북")
new_date$area =  rep(area_list, times=nrow(new_date)/17) 

## step3. sex변수 추가 
new_date_dt = rbind(new_date, new_date) #공간만들기
new_date_dt %<>% arrange(yyyymmdd,area) # 순서지정
new_date_dt$sex = rep(1:2,   times=nrow(new_date_dt)/2)
###########################################################################
new_test = rbind(test, test) #공간만들기
new_test %<>% arrange(year,month,area) # 순서지정
new_test $sex = rep(1:2,   times=nrow(new_test )/2)

#############################################################################
require(sqldf)
new =  sqldf("select * 
  from new_date_dt 
  left join new_test
  using(year,month,area,sex) 
      ") 


new_test %>% str
new$area %>% table
test = test[,-2]
#new = new[,-6] # yyyymm 삭제

########################################################################
write.csv(new, "인구_final.csv", row.names=FALSE)
