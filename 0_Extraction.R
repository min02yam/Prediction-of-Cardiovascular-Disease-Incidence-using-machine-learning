getwd()
tableList=dbGetQuery(conn, "show tables")
View(tableList)

hospital=dbGetQuery(conn,"select * from back_hospital")
head(hospital)

write.csv(hospital, "export/hospital.csv", row.names = FALSE)
############################################################################
############################################################################


ta = dbGetQuery(conn, “SELECT * FROM db_sfc_ta_dd WHERE stn_id = 245 limit 100”) 
View(ta)


ta = dbGetQuery(conn, "SELECT * FROM db_sfc_ta_dd WHERE 1=1 limit 1000") 
rn = dbGetQuery(conn, "SELECT * FROM db_sfc_rn_dd WHERE 1=1 limit 1000")

# 2012.01~ 2016.12.31
# 기온
ta = dbGetQuery(conn,
                 "SELECT * 
                  FROM db_sfc_ta_dd
                  WHERE (tma LIKE '2012%'
                  OR tma LIKE '2013%'
                  OR tma LIKE '2014%' 
                  OR tma LIKE '2015%'
                  OR tma LIKE '2016%')"
                )
write.csv(ta, "export/ASOS_기온.csv", row.names = FALSE)

# 구름
cloud = dbGetQuery(conn,
                "SELECT * 
                FROM db_sfc_cloud_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)

write.csv(cloud, "export/ASOS_구름.csv", row.names = FALSE)
View(cloud)
#####
getwd()
library(readr)
종관기상관측_ASOS_지점 <- read_csv("Readme/AreaInfo/종관기상관측(ASOS)지점.csv")
View(종관기상관측_ASOS_지점)
write.csv(종관기상관측_ASOS_지점, "export/종관기상관측_ASOS_지점.csv", row.names = FALSE)
####
#  적설(DSNW) dsnw
ASOS_적설 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_dsnw_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_적설, "export/ASOS_적설.csv", row.names = FALSE)
View(ASOS_적설)
# 증발량(EV) 요소 ev
ASOS_증발량 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_ev_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_증발량, "export/ASOS_증발량.csv", row.names = FALSE)
View(ASOS_증발량)
# 안개(FOG) 요소 fog
ASOS_안개 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_fog_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_안개, "export/ASOS_안개.csv", row.names = FALSE)
View(ASOS_안개)
# 일조(ICSR_SS) 요소 icsr_ss
ASOS_일조 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_icsr_ss_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_일조, "export/ASOS_일조.csv", row.names = FALSE)
View(ASOS_일조)
# 초상온도(LWT_TG) 요소 lwt_tg
ASOS_초상온도 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_lwt_tg_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_초상온도, "export/ASOS_초상온도.csv", row.names = FALSE)
View(ASOS_초상온도)
# - 기압(PRSR) 요소 prsr
ASOS_기압 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_prsr_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_기압, "export/ASOS_기압.csv", row.names = FALSE)
View(ASOS_기압)
# - 증기압(PV) 요소  pv
ASOS_증기압 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_pv_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_증기압, "export/ASOS_증기압.csv", row.names = FALSE)
View(ASOS_증기압)
# - 상대습도(RHM) 요소 rhm
ASOS_상대습도 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_rhm_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_상대습도, "export/ASOS_상대습도.csv", row.names = FALSE)
View(ASOS_상대습도)
###########################################################################
##0722
# -강수(RN) 요소 
ASOS_강수 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_rn_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_강수, "export/ASOS_강수.csv", row.names = FALSE)
View(ASOS_강수)

#- 바람(WIND) 요소 wind
ASOS_바람 = dbGetQuery(conn,
                   "SELECT * 
                FROM db_sfc_wind_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
)
write.csv(ASOS_바람, "export/ASOS_바람.csv", row.names = FALSE)
View(ASOS_바람)

#######################################################################
#######################################################################
#황사
# DB_YDST_APS (DB:황사:APS)
황사_APS_MI10AVG = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_APS
                    WHERE TM LIKE '2011%'
                    OR TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%' ")


write.csv(황사_APS_MI10AVG, "export/황사_APS_MI10AVG.csv", row.names = FALSE)

# ~~DB_YDST_AQMS_MI (DB:황사:AQMS:분) ~~ -> 2010년까지밖에없음
황사_AQMS_MI = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_AQMS_MI
                    WHERE (TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%')"
                    )
 write.csv(황사_AQMS_MI, "export/황사_AQMS_MI.csv", row.names = FALSE)

# DB_YDST_PM10_CMA_MI (DB:황사:PM10:중국기상청: 분)
황사_PM10_CMA_MI = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_PM10_CMA_MI
                    WHERE (TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%')"
                    )
 write.csv(황사_PM10_CMA_MI, "export/황사_PM10_CMA_MI.csv", row.names = FALSE)
# DB_YDST_PM10_KCCAR_MI (DB:황사:PM10:KCCAR: 분)   - 한중공동기상관측
황사_PM10_KCCAR_MI = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_PM10_KCCAR_MI
                    WHERE (TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%')"
                    )
 write.csv(황사_PM10_KCCAR_MI, "export/황사_PM10_KCCAR_MI.csv", row.names = FALSE)
# DB_YDST_PM10_KMA_MI ( DB:황사:PM10:기상청: 분)
황사_PM10_KMA_MI = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_PM10_KMA_MI 
                    WHERE (TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%')"
                    )
 write.csv(황사_PM10_KMA_MI, "export/황사_PM10_KMA_MI.csv", row.names = FALSE)
# !! DB_YDST_PM10_KMA_TIM   ( DB:황사:PM10:기상청: 시)   ##부유분진(PM10), 날씨마루
황사_PM10_KMA_H = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_YDST_PM10_KMA_TIM 
                    WHERE (TM LIKE '2012%'
                    OR TM LIKE '2013%'
                    OR TM LIKE '2014%'
                    OR TM LIKE '2015%'
                    OR TM LIKE '2016%')"
                    )
 write.csv(황사_PM10_KMA_H, "export/황사_PM10_KMA_H.csv", row.names = FALSE)


#########################################################################
#########################################################################
# WARN_CODE 구역별특보종류및상태 TM_FC(발표시각)
# 확인
WARN_CODE = dbGetQuery(conn,
                   "SELECT *
                    FROM WARN_CODE
                  WHERE (TM_FC LIKE '2012%'
                  OR TM_FC LIKE '2013%'
                  OR TM_FC LIKE '2014%'
                  OR TM_FC LIKE '2015%'
                  OR TM_FC LIKE '2016%')"
                    )

# DB_SFC_SESN_PHNMN_DD (DB:지상:계절:현상:일 DB 지상기상관측 계절:현상:일) TMA
계절현상_YN = dbGetQuery(conn,
                   "SELECT *
                    FROM DB_SFC_SESN_PHNMN_DD
                    LIMIT 10")

계절현상_YN= dbGetQuery(conn,
             "SELECT *
             FROM DB_SFC_SESN_PHNMN_DD
             WHERE (TMA LIKE '2012%'
             OR TMA LIKE '2013%'
             OR TMA LIKE '2014%'
             OR TMA LIKE '2015%'
             OR TMA LIKE '2016%')"
             )

 ##### 지점
 library(readr)
 황사관측지점 = read_csv("Readme/AreaInfo/황사관측지점.csv")
 write.csv(황사관측지점, "export/황사관측지점.csv", row.names = FALSE)
 

 동네예보구역 = read_csv("Readme/AreaInfo/동네예보구역.csv")
 특보구역 = read_csv("Readme/AreaInfo/특보구역.csv")
 특보명령 = read_csv("Readme/AreaInfo/특보명령.csv")
 특보수준 = read_csv("Readme/AreaInfo/특보수준.csv")
 특보타입 = read_csv("Readme/AreaInfo/특보타입.csv")
 
 
 write.csv(동네예보구역, "export/동네예보구역.csv", row.names = FALSE)
 write.csv(특보구역, "export/특보구역.csv", row.names = FALSE)
 write.csv(특보명령, "export/특보명령.csv", row.names = FALSE)
 write.csv(특보수준, "export/특보수준.csv", row.names = FALSE)
 write.csv(특보타입, "export/특보타입.csv", row.names = FALSE)
##############################################################################
##0726
#지면온도(DB_SFC_TS_DD)
ASOS_지면온도 = dbGetQuery(conn,
   "SELECT TMA, STN_ID, AVG_TS, MAX_TS, MIN_TS
   FROM DB_SFC_TS_DD
   WHERE (TMA LIKE '2012%'
   OR TMA LIKE '2013%'
   OR TMA LIKE '2014%'
   OR TMA LIKE '2015%'
   OR TMA LIKE '2016%')
   "
   )
write.csv(ASOS_지면온도, "export/ASOS_지면온도.csv", row.names = FALSE)

#해양기상부이(지점정보: 해양기상부이관측지점)
#DB_SEA_BUOY_DD
#시각, 지점번호, 평균풍속, 평균수온,평균최대파고, 평균유의파고,  최대최고파고, 최대유의파고, 최저최고파고, 최저유의파고
해양기상부이 = dbGetQuery(conn,
   "SELECT TMA, STN_ID, AVG_WS, AVG_WRTM, AVG_MAX_WH, AVG_ATENT_WH, MAX_MAX_WH, MAX_ATENT_WH, MIN_MAX_WH, MIN_ATENT_WH
   FROM DB_SEA_BUOY_DD
   WHERE (TMA LIKE '2012%'
   OR TMA LIKE '2013%'
   OR TMA LIKE '2014%'
   OR TMA LIKE '2015%'
   OR TMA LIKE '2016%')
   "
   )
View(해양기상부이)
write.csv(해양기상부이, "export/해양기상부이.csv", row.names = FALSE)


#등표기상관측(지점정보: 등표기상측지점)
#DB_SEA_LB_DD
#시각, 지점번호, 평균풍속, 평균수온, 평균최대파고, 평균유의파고,  최대최고파고, 최대유의파고, 최저최고파고, 최저유의파고
등표기상관측 = dbGetQuery(conn,
   "SELECT TMA, STN_ID, AVG_WS, AVG_WRTM,AVG_MAX_WH,AVG_ATENT_WH, MAX_MAX_WH, MAX_ATENT_WH, MIN_MAX_WH, MIN_ATENT_WH
   FROM DB_SEA_LB_DD
   WHERE (TMA LIKE '2012%'
   OR TMA LIKE '2013%'
   OR TMA LIKE '2014%'
   OR TMA LIKE '2015%'
   OR TMA LIKE '2016%')
   "
   )
View(등표기상관측)
write.csv(등표기상관측, "export/등표기상관측.csv", row.names = FALSE)

###지점
require(readr)
등표기상관측지점 = read_csv("Readme/AreaInfo/등표기상관측지점.csv")
해양기상부이관측지점 = read_csv("Readme/AreaInfo/해양기상부이관측지점.csv")
 write.csv(등표기상관측지점, "export/등표기상관측지점.csv", row.names = FALSE)
 write.csv(해양기상부이관측지점, "export/해양기상부이관측지점.csv", row.names = FALSE)

##########################################
##0729
##적설 다시
#  적설(DSNW) dsnw
 ASOS_적설_2016 = dbGetQuery(conn,
                      "SELECT tma, stn_id, dd_mes, dd_mefs 
                FROM db_sfc_dsnw_dd
                WHERE (tma LIKE '2012%'
                OR tma LIKE '2013%'
                OR tma LIKE '2014%' 
                OR tma LIKE '2015%'
                OR tma LIKE '2016%')"
 )
write.csv(ASOS_적설_2016, "export/ASOS_적설_2016.csv", row.names = FALSE)
View(ASOS_적설_2016)
###
##항공
 항공  = dbGetQuery(conn,
  "SELECT TM, STN_ID, MAX_WD, MAX_WS, MAX_IWS, MAX_TA, MIN_TA,MAX_PS, MIN_PS, MEFS, MES
  FROM  DB_FLGHT_DD
  WHERE (tm LIKE '2012%'
  OR tm LIKE '2013%'
  OR tm LIKE '2014%' 
  OR tm LIKE '2015%'
  OR tm LIKE '2016%')
  ")
View(항공)
write.csv(항공, "export/항공.csv", row.names = FALSE)




# 평년 구름 DB_SFC_NMYR_CLOUD_DD
##평년 구름 시작년도, 월일, 지점번호, 평균 전운량
구름_평년  = dbGetQuery(conn,
 "SELECT ST_YEAR, MD, STN_ID, AVG_TCA
  FROM  DB_SFC_NMYR_CLOUD_DD
  WHERE   ST_YEAR >2010
  ")

View(구름_평년)
write.csv(구름_평년, "export/구름_평년.csv", row.names = FALSE)


# DB_AWS_RHM_TIM (AWS_상대습도_시) 
 AWS_상대습도  = dbGetQuery(conn,
  "SELECT TMA, STN_ID, MIN_RHM, MAX_RHM
  FROM  DB_AWS_RHM_TIM
  WHERE (ST_YEAR = 2012 
  OR tm LIKE '2013%'
  OR tm LIKE '2014%' 
  OR tm LIKE '2015%'
  OR tm LIKE '2016%')
  ")

View(AWS_상대습도)
write.csv(AWS_상대습도, "export/AWS_상대습도.csv", row.names = FALSE)

# 상대습도_비교
# DB_SFC_RHM_DD_COMP : 지상_상대습도_비교
# TMA, STN_ID, AVG_RHM, MIN_RHM, FOG_DUR
# 시각, 지점번호, 평균 상대습도, 최소 상대습도, 안개 계속 시간
ASOS_상대습도_비교 = dbGetQuery(conn,
  "SELECT TMA, STN_ID, AVG_RHM, MIN_RHM, FOG_DUR
   FROM  DB_SFC_RHM_DD_COMP 
   WHERE (TMA LIKE '2012%'
   OR TMA LIKE '2013%'
   OR TMA LIKE '2014%'
   OR TMA LIKE '2015%'
   OR TMA LIKE '2016%')
   ")

View(ASOS_상대습도_비교)
write.csv(ASOS_상대습도_비교, "export/ASOS_상대습도_비교.csv", row.names = FALSE)

#DB_AWS_ICSR_SS_DD: AWS_일사
#TMA, STN_ID, SUM_SS_HR, SUM_GSR
#합계일조시간, 일조율, 합계 전천일사
AWS_일사 = dbGetQuery(conn,
  "SELECT TMA, STN_ID, SUM_SS_HR, SUM_GSR
   FROM  DB_AWS_ICSR_SS_DD
   WHERE (TMA LIKE '2012%'
   OR TMA LIKE '2013%'
   OR TMA LIKE '2014%'
   OR TMA LIKE '2015%'
   OR TMA LIKE '2016%')
   ")
View(AWS_일사)
write.csv(AWS_일사, "export/ASOS_상대습도_비교.csv", row.names = FALSE)

##
방재기상관측_AWS_지점 = read_csv("Readme/AreaInfo/방재기상관측(AWS)지점.csv")
View(방재기상관측_AWS_지점)
write.csv(방재기상관측_AWS_지점, "export/방재기상관측_AWS_지점.csv", row.names = FALSE)
