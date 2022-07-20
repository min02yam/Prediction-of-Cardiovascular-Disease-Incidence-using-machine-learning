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

#################################################
#황사

DB_YDST_PM10_KMA_TIM