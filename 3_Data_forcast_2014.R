getwd()
require(data.table)
require(tidyverse)
require(plyr)
require(magrittr)
require(lubridate)


##################2014년######################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/강원")
######### 1.강원-2014
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

LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
강원_2014=join_all(LIST, by="date", type="left") 
View(강원_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/경기")
######### 2.경기-2014
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
경기_2014=join_all(LIST, by="date", type="left") 
View(경기_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/경남")
######### 3.경남-2014
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
경남_2014=join_all(LIST, by="date", type="left") 
View(경남_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/경북")
######### 4.경북-2014
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
경북_2014=join_all(LIST, by="date", type="left") 
View(경북_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/광주")
######### 5.광주-2014
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
광주_2014=join_all(LIST, by="date", type="left") 
View(광주_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/대구")
######### 7.대구-2014
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
대구_2014=join_all(LIST, by="date", type="left") 
View(대구_2014)


################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/대전")
######### 6.대전-2014
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
대전_2014=join_all(LIST, by="date", type="left") 
View(대전_2014)



################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/부산")
######### 8.부산-2014
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
부산_2014=join_all(LIST, by="date", type="left") 
View(부산_2014)

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/서울")
######### 9.서울-2014
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
서울_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/세종")
######### 10.세종-2014
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
세종_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/울산")
######### 11.울산-2014
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
울산_2014=join_all(LIST, by="date", type="left") 
################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/인천")
######### 12.인천-2014
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
인천_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/전남")
######### 13.전남-2014
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
전남_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/전북")
######### 14.전북-2014
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
전북_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/제주")
######### 15.제주-2014
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
제주_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/충남")
######### 16.충남-2014
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
충남_2014=join_all(LIST, by="date", type="left") 

################################################################################
################################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2014/충북")
######### 16.충북-2014
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
충북_2014=join_all(LIST, by="date", type="left") 
##########################################################################
#######################################################################
##area 변수 생성
강원_2014$area = rep("강원", times=nrow(강원_2014))
경기_2014$area = rep("경기", times=nrow(경기_2014))
경남_2014$area = rep("경남", times=nrow(경남_2014))
경북_2014$area = rep("경북", times=nrow(경북_2014))
광주_2014$area = rep("광주", times=nrow(광주_2014))

대구_2014$area = rep("대구", times=nrow(대구_2014))
대전_2014$area = rep("대전", times=nrow(대전_2014))
부산_2014$area = rep("부산", times=nrow(부산_2014))
서울_2014$area = rep("서울", times=nrow(서울_2014))
세종_2014$area = rep("세종", times=nrow(세종_2014))

울산_2014$area = rep("울산", times=nrow(울산_2014))
인천_2014$area = rep("인천", times=nrow(인천_2014))
전남_2014$area = rep("전남", times=nrow(전남_2014))
전북_2014$area = rep("전북", times=nrow(전북_2014))
제주_2014$area = rep("제주", times=nrow(제주_2014))

충남_2014$area = rep("충남", times=nrow(충남_2014))
충북_2014$area = rep("충북", times=nrow(충북_2014))


##최종병합

new = bind_rows(강원_2014,경기_2014,경남_2014,경북_2014,광주_2014,
	대구_2014,대전_2014, 부산_2014,서울_2014,세종_2014,울산_2014,인천_2014,
	전남_2014,전북_2014,제주_2014,충남_2014,충북_2014)
new $area %>% table


##저장
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/final")
write.csv(new,"f2014.csv",row.names=FALSE)

#확인
test=fread("f2014.csv")
test$area %>% table
