# 0.패키지 및 데이터 로드
setwd("C:/Users/alsdu/Downloads/3_기타/2.인구")
suppressPackageStartupMessages({
  require(tidyverse)
  require(magrittr)
  require(data.table)
})



#rm(list=ls())
고령인구비율= read.csv ("고령인구비율_시도_시_군_구__20220730191254.csv", fileEncoding = "euc-kr")
성별인구수= read.csv ("행정구역_시군구_별__성별_인구수_20220730183620.csv", fileEncoding = "euc-kr")

고령인구비율 %>% head
성별인구수 %>% head

#변수명 변경
names(고령인구비율)= c("area", "yyyymm","고령인구비율","65세이상인구","전체인구")
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
고령인구비율 %<>% select(area, yyyymm, 고령인구비율) %>%
  filter(! area == '전국') 

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
고령인구비율$area %>% table


#write.csv(성별인구수, "성별인구수.csv", row.names=FALSE)
#write.csv(고령인구비율, "고령인구비율.csv", row.names=FALSE)
