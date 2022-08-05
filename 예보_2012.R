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
강원_2012=join_all(LIST, by="date", type="left") 
View(강원_2012)
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
경기_2012=join_all(LIST, by="date", type="left") 
View(경기_2012)

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
경남_2012=join_all(LIST, by="date", type="left") 
View(경남_2012)

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
경북_2012=join_all(LIST, by="date", type="left") 
View(경북_2012)


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
광주_2012=join_all(LIST, by="date", type="left") 
View(광주_2012)
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
대구_2012=join_all(LIST, by="date", type="left") 
View(대구_2012)

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
부산_2012=join_all(LIST, by="date", type="left") 
View(부산_2012)

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
서울_2012=join_all(LIST, by="date", type="left") 
View(서울_2012)

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
세종_2012=join_all(LIST, by="date", type="left") 
View(세종_2012)

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
울산_2012=join_all(LIST, by="date", type="left") 
View(울산_2012)


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
인천_2012=join_all(LIST, by="date", type="left") 
View(인천_2012)

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
전남_2012=join_all(LIST, by="date", type="left") 
View(전남_2012)

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
전북_2012=join_all(LIST, by="date", type="left") 
View(전북_2012)

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
제주_2012=join_all(LIST, by="date", type="left") 
View(제주_2012)


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
충남_2012=join_all(LIST, by="date", type="left") 
View(충남_2012)

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
충북_2012=join_all(LIST, by="date", type="left") 
View(충북_2012)


setwd("C:/Users/alsdu/Downloads/예보데이터/예보데이터/2012")



#write.csv(강원_2012, "강원_2012.csv", row.names=FALSE)
#write.csv(경기_2012, "경기_2012.csv", row.names=FALSE)
#write.csv(경남_2012, "경남_2012.csv", row.names=FALSE)
#write.csv(경북_2012, "경북_2012.csv", row.names=FALSE)
#write.csv(광주_2012, "광주_2012.csv", row.names=FALSE)

#write.csv(대구_2012, "대구_2012.csv", row.names=FALSE)
#write.csv(대전_2012, "대전_2012.csv", row.names=FALSE)
#write.csv(부산_2012, "부산_2012.csv", row.names=FALSE)
#write.csv(서울_2012, "서울_2012.csv", row.names=FALSE)
#write.csv(세종_2012, "세종_2012.csv", row.names=FALSE)

#write.csv(울산_2012, "울산_2012.csv", row.names=FALSE)
#write.csv(인천_2012, "인천_2012.csv", row.names=FALSE)
#write.csv(전남_2012, "전남_2012.csv", row.names=FALSE)
#write.csv(전북_2012, "전북_2012.csv", row.names=FALSE)
#write.csv(제주_2012, "제주_2012.csv", row.names=FALSE)

#write.csv(충남_2012, "충남_2012.csv", row.names=FALSE)
#write.csv(충북_2012, "충북_2012.csv", row.names=FALSE)




#최종병합
final_list=list(강원_2012, 경기_2012, 경남_2012, 경북_2012,광주_2012,
                   대구_2012, 부산_2012, 서울_2012, 세종_2012,
                   울산_2012, 인천_2012, 전남_2012, 전북_2012, 제주_2012,
                   충남_2012, 충북_2012)
f2012=join_all(final_list, by="date", type="left") 

#저장
write.csv(f2012,"f2012.csv", row.names = FALSE)




