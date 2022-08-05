getwd()
require(data.table)
require(tidyverse)
require(plyr)
require(magrittr)
require(lubridate)


###############################2013년##################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/강원")
######### 1.강원-2013
rm(list=ls())
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
강원=join_all(LIST, by="date", type="left") 
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/경기")
######### 2.경기-2013
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
경기=join_all(LIST, by="date", type="left") 

경기 %>% head
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/경남")
######### 3.경남-2013
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
경남=join_all(LIST, by="date", type="left") 
#경남 %>% tail

################################################################################
################################################################################
#rm(list=ls())
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/경북")
######### 4.경북-2013
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
경북=join_all(LIST, by="date", type="left") 
#경북 %>% tail

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/광주")
######### 5.광주-2013
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
#광주 %>% tail 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/대구")
######### 6.대구-2013
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
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/대전")
######### 7.대전-2013
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
#대전 %>% tail

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/부산")
######### 8.부산-2013
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

#부산 %>% tail
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/서울")
######### 9. 서울-2013
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

#서울 %>% tail

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/세종")
######### 10.세종-2013
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
#세종 %>% tail
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/울산")
######### 11.울산-2013
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
울산 %>% head
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/인천")
######### 12.인천-2013
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
#인천 %>% tail
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/전남")
######### 13.전남-2013
###1-1.3시간기온
list = list.files(pattern = "3시간기온") #1개만 존재 
#데이터 로드 & 전처리
#step1.먼저 형식 다른 두 개만 따로 처리
final <-NULL
ind1 = c(1:2,5,8,9,11,12)
for (i in ind1 ){
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
#step2. 나머지 처리 후 
final2 <-NULL
ind2 = c(3,4,6,7,10,13)
for (i in ind2 ){
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
  final2 = rbind(final2,file)
  cat("\n",i) 
}
  
  
final %>% str
final2 %>% str
  
#step3. 두개 병합
FINAL = rbind(final, final2)
#FINAL %>% tail
# 필터
FINAL %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
FINAL[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head

##################################################################
###1-2.습도
list = list.files(pattern = "습도") 
#데이터 로드 & 전처리
#step1.먼저 형식 다른 두 개만 따로 처리
final <-NULL
ind1 = c(1:2,5,8,9,11,12)
for (i in ind1 ){
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
#step2. 나머지 처리 후 
final2 <-NULL
ind2 = c(3,4,6,7,10,13)
for (i in ind2 ){
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
  final2 = rbind(final2,file)
  cat("\n",i) 
}


final %>% str
final2 %>% str

#step3. 두개 병합
FINAL = rbind(final, final2)
FINAL %>% tail

FINAL %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
FINAL[ , .(f평균습도 = mean(value)), by=date] ->평균습도
평균습도 %>% head

##################################################################
###1-3.하늘상태
list = list.files(pattern = "하늘상태") 
#데이터 로드 & 전처리
#step1.먼저 형식 다른 두 개만 따로 처리
final <-NULL
ind1 = c(1:2,5,8,9,11,12)
for (i in ind1 ){
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
#step2. 나머지 처리 후 
final2 <-NULL
ind2 = c(3,4,6,7,10,13)
for (i in ind2 ){
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
  final2 = rbind(final2,file)
  cat("\n",i) 
}


final %>% str
final2 %>% str

#step3. 두개 병합
FINAL = rbind(final, final2)
FINAL %>% tail

FINAL %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
FINAL[ , .(f평균하늘상태 = round(mean(value))), by=date] ->평균하늘상태
평균하늘상태 %>% head


##################################################################
###1-4. 강수형태
list = list.files(pattern = "강수형태") 
#데이터 로드 & 전처리
#step1.먼저 형식 다른 두 개만 따로 처리
final <-NULL
ind1 = c(1:2,5,8,9,11,12)
for (i in ind1 ){
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
#step2. 나머지 처리 후 
final2 <-NULL
ind2 = c(3,4,6,7,10,13)
for (i in ind2 ){
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
  final2 = rbind(final2,file)
  cat("\n",i) 
}


final %>% str
final2 %>% str

#step3. 두개 병합
FINAL = rbind(final, final2)
FINAL %>% tail

FINAL %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) %>% data.table
FINAL[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head

#병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
전남=join_all(LIST, by="date", type="left") 
전남 %>% head



################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/전북")
######### 14.전북-2013
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
#final %>% head

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
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/제주")
######### 15.제주-2013
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
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/충남")
######### 16.충남-2013
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
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013/충북")
######### 16.충북-2013
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

#최종병합
final_list=list(강원, 경기, 경남, 경북,광주,
                  대구, 대전, 부산, 서울, 세종,
                  울산, 인천, 전남, 전북, 제주,
                  충남, 충북)
f2013=join_all(final_list, by="date", type="left") 

#저장
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2013")
write.csv(f2013,"f2013.csv", row.names = FALSE)
