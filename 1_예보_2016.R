getwd()
require(data.table)
require(tidyverse)
require(plyr)
require(magrittr)
require(lubridate)


###############################2016년##################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/강원")
######### 1.강원-2016
rm(list=ls())
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}


final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head


##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}


final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}


final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head

#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
강원=join_all(LIST, by="date", type="left") 
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/경기")
######### 2.경기-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경기=join_all(LIST, by="date", type="left") 

경기 %>% head
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/경남")
######### 3.경남-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경남=join_all(LIST, by="date", type="left") 


################################################################################
################################################################################
### 경북 zip형태로 따로 되어있음
#rm(list=ls())
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/경북")
######### 4.경북-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경북=join_all(LIST, by="date", type="left") 


################################################################################
################################################################################
#2013처럼 데이터 구조 되어있음
#위치 확인
#rm(list=ls())
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/광주")
######### 5.광주-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") #1개만 존재 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                col.names = c("day", "hour", "forecast", "value"),
                nrows = which - 1)
                )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
  parse_date_time(orders="ymd")
#필터링
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
광주=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/대구")
######### 6.대구-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
                )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
대구=join_all(LIST, by="date", type="left") 


################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/대전")
######### 7.대전-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
대전=join_all(LIST, by="date", type="left") 


################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/부산")
######### 8.부산-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
부산=join_all(LIST, by="date", type="left") 


################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/서울")
######### 9. 서울-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
서울=join_all(LIST, by="date", type="left") 



################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/세종")
######### 10.세종-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
세종=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/울산")
######### 11.울산-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

file %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
울산=join_all(LIST, by="date", type="left") 
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/인천")
######### 12.인천-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}



final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
인천=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/전남")
######### 13.전남-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
전남=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/전북")
######### 14.전북-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
전북=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/제주")
######### 15.제주-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
제주=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/충남")
######### 16.충남-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
충남=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2016/충북")
######### 16.충북-2016
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 인덱스 번호 확인
#step2. 반복 시작되기 전 행까지 다시 불러옴
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], read.csv(list [i],col.names = c("day", "hour", "forecast", "value")) )
  which = which(str_detect(file$day, 'format')) 
  file = assign(list[i], read.csv(list [i],
                                  col.names = c("day", "hour", "forecast", "value"),
                                  nrows = which - 1)
  )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],10,13)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,15))-1)
  for (j in which(is.na(file$hour))){
    
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],10,13)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],14,15)
  }
  file %<>% filter(! is.na(hour))  #na행 추가되어있음
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
충북=join_all(LIST, by="date", type="left") 

########################################################################
#######################################################################
##area 변수 생성
강원$area = rep("강원", times=nrow(강원))
경기$area = rep("경기", times=nrow(경기))
경남$area = rep("경남", times=nrow(경남))
경북$area = rep("경북", times=nrow(경북))
광주$area = rep("광주", times=nrow(광주))

대구$area = rep("대구", times=nrow(대구))
대전$area = rep("대전", times=nrow(대전))
부산$area = rep("부산", times=nrow(부산))
서울$area = rep("서울", times=nrow(서울))
세종$area = rep("세종", times=nrow(세종))

울산$area = rep("울산", times=nrow(울산))
인천$area = rep("인천", times=nrow(인천))
전남$area = rep("전남", times=nrow(전남))
전북$area = rep("전북", times=nrow(전북))
제주$area = rep("제주", times=nrow(제주))

충남$area = rep("충남", times=nrow(충남))
충북$area = rep("충북", times=nrow(충북))


##최종병합

new = bind_rows(강원,경기,경남,경북,광주,대구,대전,부산,
	서울,세종,울산,인천,전남,전북,제주,충남,충북)
new $area %>% table


##저장
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/final")
write.csv(new,    "f2016.csv" , row.names=FALSE)

test=fread("f2016.csv")

test$area %>% table
