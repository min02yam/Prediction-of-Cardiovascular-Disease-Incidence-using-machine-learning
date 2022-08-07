rm(list=ls())


getwd()
require(data.table)
require(tidyverse)
require(plyr)
require(magrittr)
require(lubridate)


##################2012년######################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/강원")
######### 1.강원-2012
rm(list=ls())
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
    for (j in which(is.na(file$hour))){
      file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
      file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
    }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
평균3시간기온 %>% head

LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
강원=join_all(LIST, by="date", type="left") 

################################################################
################################################################
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/경기")
######### 2.경기-2012
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태
평균강수형태 %>% head
평균3시간기온 %>% head
###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경기=join_all(LIST, by="date", type="left") 


########################################################################
##3.경남_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/경남")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경남=join_all(LIST, by="date", type="left") 


########################################################################
##4.경북_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/경북")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
경북=join_all(LIST, by="date", type="left") 



########################################################################
##5.광주_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/광주")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
광주=join_all(LIST, by="date", type="left") 

########################################################################
#######################################################################
##6.대구_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/대구")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
대구=join_all(LIST, by="date", type="left") 


##########################################################################
#######################################################################
#대전 데이터 없음 

#######################################################################
##8. 부산_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/부산")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
부산=join_all(LIST, by="date", type="left") 


####################################################################
##9. 서울_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/서울")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
서울=join_all(LIST, by="date", type="left") 


####################################################################
##10. 세종_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/세종")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
세종=join_all(LIST, by="date", type="left") 


####################################################################
##11. 울산_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/울산")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
울산=join_all(LIST, by="date", type="left") 



####################################################################
##12. 인천_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/인천")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
인천=join_all(LIST, by="date", type="left") 


####################################################################
##13. 전남_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/전남")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
전남=join_all(LIST, by="date", type="left") 


####################################################################
##14. 전북_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/전북")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
전북=join_all(LIST, by="date", type="left") 


####################################################################
##15. 제주_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/제주")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
제주=join_all(LIST, by="date", type="left") 


####################################################################
##16. 충남_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/충남")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
충남=join_all(LIST, by="date", type="left") 
####################################################################
##16. 충북_2012
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012/충북")
###1-1.3시간기온
list = list.files(pattern = "3시간기온") 
#데이터 로드 & 전처리
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

#final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균3시간기온 = mean(value)), by=date] ->평균3시간기온 
평균3시간기온 %>% head





###1.2 습도 
list = list.files(pattern = "습도")
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균습도 = mean(value)), by=date] ->평균습도



##1.4 하늘상태
list = list.files(pattern = "하늘상태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균하늘상태 = mean(value)%>%round), by=date] ->평균하늘상태

##1.5 강수형태
list = list.files(pattern = "강수형태") 
final <-NULL 
for (i in 1:length(list)){
  file = assign(list[i], fread(list [i],col.names = c("day", "hour", "forecast", "value")) )
  file$year = substr(file[which(is.na(file$hour))[1],'day'],9,12)
  file$month = paste0('0',as.integer(substr(file[which(is.na(file$hour))[1],'day'],14,14))-1)
  for (j in which(is.na(file$hour))){
    file[j:nrow(file),(ncol(file)-1)] = substr(file$day[j],9,12)
    file[j:nrow(file),ncol(file)] = substr(file$day[j],13,14)
  }
  file %<>% filter(! grepl(c("Start"), day)) 
  file$date = paste0(file$year,"-",file$month, "-",file$day) %>%
    parse_date_time(orders="ymd")
  final = rbind(final,file)
  cat("\n",i) 
}

final %>% str
final %<>% filter(hour==1400, forecast==25) %>%
  select(date,year,month,day,value) 
final[ , .(f평균강수형태 = round(mean(value))), by=date] ->평균강수형태

###병합
LIST= list(평균3시간기온 ,평균강수형태,평균습도,평균하늘상태)
충북=join_all(LIST, by="date", type="left") 
View(충북)
########################################################################
##area 변수 생성
강원$area = rep("강원", times=nrow(강원))
경기$area = rep("경기", times=nrow(경기))
경남$area = rep("경남", times=nrow(경남))
경북$area = rep("경북", times=nrow(경북))
광주$area = rep("광주", times=nrow(광주))

대구$area = rep("대구", times=nrow(대구))
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

new = bind_rows(강원,경기,경남,경북,광주,대구,부산,서울,세종,울산,인천,전남,전북,제주,충남,충북)
new $area %>% table


##저장
setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/final")
write.csv(new,    "f2012.csv" , row.names=FALSE)


test=fread("f2012.csv")

test$area %>% table

