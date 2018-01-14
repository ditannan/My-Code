#**************************#
# Edited by Qiao Jiaying
# Revised by Xu Zhimeng
# Last updated: 2018/01/07
# n = 300
#**************************#

#读入数据
setwd("E:/课题组/001_正式试验数据/数据清洗核对/基线问卷")
cesd <- data.frame(read.csv("抑郁问卷-未整理（1659份）.csv"), stringsAsFactors = FALSE)
baseline <- data.frame(read.csv("基线问卷-未整理（311份）.csv", stringsAsFactors = FALSE))
id <- data.frame(read.csv("id_num.csv"), stringsAsFactors = FALSE)

#以治疗号作为id匹配
#***********************************************id匹配前校正********************************************#

#基线数据治疗号修正
#baseline id (Q8为基线数据中治疗号)
names(baseline)<- paste0("Q", 1:ncol(baseline))
baseline$id <- as.numeric(as.character(baseline$Q8))

intersect(id$ptnum, baseline$id)
setdiff(id$ptnum, baseline$id) # 21个治疗号需要更正
# [1] 13742 13948 14509 14461 14556 14580 12768 14062 14293
# [10] 13766 13679 13801 14071 13734 14462  9931 13100 13173
# [19] 14541 13823 14080


baseline$id[baseline$id %in% c(207)] <- 13766 #
baseline$id[22] <- 13679  ## 编号211 #
baseline$id[31] <- 9931   ## 编号506 #
baseline$id[56] <- 13734  ## 编号301 #
baseline$id[58] <- 13742  ## 编号001 #
baseline$id[90] <- 13801  ## 编号224 #
baseline$id[93] <- 13823  ## 编号821 #
baseline$id[110] <- 13173 ## 编号516 #
baseline$id[150] <- 13948 ## 编号020 #
baseline$id[181] <- 14071 ## 编号243 #
baseline$id[192] <- 14080 ## 编号841 #
baseline$id[193] <- 13428  ### 本来就是13428，编号是137
baseline$id[200] <- 14062  ## 编号139 #
baseline$id[270] <- 360000   # 036 unknown
baseline$id[290] <- 14461  ## 编号044 #
baseline$id[291] <- 14462  ## 编号347 #
baseline$id[297] <- 14541  ## 编号548 #
baseline$id[303] <- 14556  ## 编号049 #
# baseline$id[304] <- 14541  ### 此人实际是552，填成了522，实际编号是7059，为啥要改成548的编号？ 
baseline$id[305] <- 14580  ## 编号050 #

# check function
id[id$ptid %in% 552, 'ptnum']
id[id$ptnum %in% 13766, 'ptid']
which(baseline$id == 13101)

# 043的14509木有更正，id[288]
# 133的12768木有更正，id[185]
# 149的14293木有更正，id[254]
# 510的13100木有更正，id[46]
baseline$id[288] <- 14509 #
baseline$id[185] <- 12768 #
baseline$id[254] <- 14293 #
baseline$id[46] <- 13100 #

id300 <- id$ptnum
library(dplyr)
library(magrittr)
#### 治疗号正确300基线 ####
baseline %<>% filter(id %in% id300)
# 导出核对基线总数据集
bl300 <- merge(baseline, id, by.x = 'id', by.y = 'ptnum')
write.csv(bl300, 'bl300.csv', row.names = FALSE)

#CES-D治疗号修正
#cesd id (tid为基线数据中治疗号)
names(cesd)<- paste0("C", 1 : ncol(cesd))
names(cesd)[7] <- "tid"
names(cesd)[28] <- "score"
cesd$id <- as.numeric(as.character(cesd$tid))
# 创建原读入数据索引
cesd$index <- 1 : nrow(cesd)

# 保留抑郁分数大于等于16观测
cesd %<>% filter(score >= 16)
# 填写了治疗号的cesd问卷，263份，其中4人重复填写
dep <- merge(cesd, id, by.x = 'tid', by.y = 'ptnum' ) ## 263条观测259例患者
write.csv(dep, 'depin300.csv', row.names = FALSE)
depall <- merge(cesd, id, by.x = 'id', by.y = 'ptnum', all = TRUE )
write.csv(depall, 'depall.csv', row.names = FALSE)
# 未填写治疗号患者41例
absent.idnum <- setdiff(id300, cesd$id)
# [1] 13742  4388  2984 13948 14509 14461 14556 14580
# [9] 13712  7243 14062 14293 13766 13679 13801 11099
# [17] 14071 11574 13734 11998  5748 14462 13100  4891
# [25] 13173  9889 11062 12457  5674 10552  8691  5268
# [33] 14541 14065  7059 11986 14358 13823  1640 14080
# [41] 14476 10889
# 对应编号
id[id$ptnum %in% absent.idnum, 'ptid']
# [1]   1   9  10  20  43  44  49  50 108 134 139
# [12] 149 207 211 224 231 243 246 301 312 314 347
# [23] 510 515 516 518 519 539 541 542 543 546 548
# [34] 549 552 553 556 821 826 841 852 148
# check function
id[id$ptid %in% 552, 'ptnum']
id[id$ptnum %in% 7059, 'ptid']

#多次填写ces-d，删去非纳入时填写记录
cesd$id[cesd$index %in% 146] <- NA # 014
cesd$id[cesd$index %in% 291] <- NA # 815
cesd$id[cesd$index %in% 305] <- NA # 033
cesd$id[cesd$index %in% 1433] <- NA # 008
cesd$id[cesd$index %in% 1425] <- NA # 148 10889

cesd$id[cesd$index %in% 1541] <- 14509 # 043
cesd$id[cesd$index %in% 1339] <- 14293 # 149
cesd$id[cesd$index %in% 248] <- 13100 # 510
cesd$id[cesd$index %in% 1607] <- 7059 # 552
cesd$id[cesd$index %in% 85] <- 13766  # 编号207
cesd$id[cesd$index %in% 120] <- 13679 # 211 13679
cesd$id[cesd$index %in% 209] <- 13712 # 108 13712 为什么8月18号做的cesd在25号才做基线问卷呢
cesd$id[cesd$index %in% 315] <- 13734 # 301 13734
cesd$id[cesd$index %in% 321] <- 13742 # 001 13742
cesd$id[cesd$index %in% 430] <- 4891 # 515 4891
cesd$id[cesd$index %in% 457] <- 4388 # 009 4388
cesd$id[cesd$index %in% 463] <- 13801 # 224 13801
cesd$id[cesd$index %in% 491] <- 13823 # 821 13823
cesd$id[cesd$index %in% 512] <- 2984 # 010 2984
cesd$id[cesd$index %in% 608] <- 11998 # 312 11998，填成11988
cesd$id[cesd$index %in% 612] <- 13173 # 516 13173
cesd$id[cesd$index %in% 619] <- 9889 # 518 9889
cesd$id[cesd$index %in% 646] <- 1640 # 826 1640
cesd$id[cesd$index %in% 674] <- 11062 # 519 11062
cesd$id[cesd$index %in% 723] <- 5748 # 314 5748，填成5784
cesd$id[cesd$index %in% 762] <- 11099 # 231 11099
cesd$id[cesd$index %in% 786] <- 13948 # 020 13948
cesd$id[cesd$index %in% 968] <- 14071 # 243 14071
cesd$id[cesd$index %in% 1019] <- 7243 # 134 7243，填成了7423
cesd$id[cesd$index %in% 1028] <- 14080 # 841 14080
cesd$id[cesd$index %in% 1079] <- 14062 # 139 14062，填成了139
cesd$id[cesd$index %in% 1135] <- 11574 # 246 11574
cesd$id[cesd$index %in% 1443] <- 12457 # 539 12457
cesd$id[cesd$index %in% 1501] <- 5674 # 541 5674
cesd$id[cesd$index %in% 1520] <- 10552 # 542 10552
cesd$id[cesd$index %in% 1527] <- 8691 # 543 8691
cesd$id[cesd$index %in% 1534] <- 5268 # 546 5268
cesd$id[cesd$index %in% 1555] <- 14461 # 044 14461
cesd$id[cesd$index %in% 1556] <- 14462 # 347 14462
cesd$id[cesd$index %in% 1574] <- 14476 # 852 14476
cesd$id[cesd$index %in% 1580] <- 14541 # 548 14541
cesd$id[cesd$index %in% 1582] <- 14065 # 549 14065
cesd$id[cesd$index %in% 1608] <- 11986 # 553 11986
cesd$id[cesd$index %in% 1612] <- 14556 # 049
cesd$id[cesd$index %in% 1621] <- 14580 # 050
cesd$id[cesd$index %in% 1631] <- 14358 # 556 14358
cesd$id[cesd$index %in% 725] <- 10889 # 148 10889




#### 治疗号正确300基线cesd ####
cesd %<>% filter(id %in% id300)

#*************************************id 匹配后校正******************************************************#

#merge by id 按治疗号匹配

baseall <- merge(baseline, cesd, by = "id")
names(baseall)
#baseall#为#根据治疗号匹配的CES-D及基线问卷#

#*************************************填写错误更正**************************************#
#出生日期
baseall$Q10[baseall$id %in% c(11678)] <- "1975/07/27"
baseall$Q10[baseall$id %in% c(13823)] <- "1994/09/20"
baseall$Q10[baseall$id %in% c(8170)] <- "1984/11/14"
baseall$Q10[baseall$id %in% c(13905)] <- "1993/10/10"
baseall$Q10[baseall$id %in% c(6332)] <- "1996/08/07"

#感染日期 
baseall$Q33[baseall$id %in% c(11456)] <- "2016/10/01" #319
baseall$Q33[baseall$id %in% c(10300)] <- "2015/05/01" #327
baseall$Q33[baseall$id %in% c(10601)] <- "2014/01/01" #002
baseall$Q33[baseall$id %in% c(13071)] <- "2017/06/02" #005
baseall$Q33[baseall$id %in% c(14062)] <- "2017/10/01" #139
baseall$Q33[baseall$id %in% c(12768)] <- "2017/03/01" #133
baseall$Q33[baseall$id %in% c(5368)] <- "2013/06/01"  #021
baseall$Q33[baseall$id %in% c(12476)] <- "2017/03/01" #302
baseall$Q33[baseall$id %in% c(13733)] <- "2013/06/01"  #844


#身高
baseall$Q11[baseall$Q11 < 2] <- baseall$Q11[baseall$Q11 < 2] * 100

#体重
baseall$Q12[baseall$Q12 > 90] <- baseall$Q12[baseall$Q12 > 90] / 2

#体重变化
baseall$Q14 <- as.numeric(as.character(baseall$Q14))
baseall$Q14[baseall$Q14 %in% c(-3)] <- 0
#error
baseall$Q14[baseall$id %in% c(13734)] <- 3
#error
baseall$Q14[baseall$Q14>100] <- abs(baseall$Q14[baseall$Q14>100]/2-baseall$Q12[baseall$Q14>100])
baseall$Q14[baseall$Q14>20] <- abs(baseall$Q14[baseall$Q14>20]-baseall$Q12[baseall$Q14>20])

#**************************************baseall重命名*****************************************************#
names(baseall)

base <- baseall %>% select(-c(Q1, Q4, Q5, Q6, Q7, Q8, C1, C2, C3, C4, C5, C6, tid, index))
#合并表格重命名
names(base)[1:7]<-c("number","date","time","q3_gender","q4_birth","q5_1_height","q5_2_weight")
names(base)[8:13]<-c("q6_ifweightchange","q7_weightchange","q8_huji","q9_education","q10_sexori","q11_marriage")
names(base)[14:23]<-c("q12_child","q13_dusheng","q14d_1_parient","q14d_2_child","q14d_3_spouse","q14d_4_gdsexp","q14d_5_qinqi","q14d_6_friend","q14d_7_alone","q14d_8_other")
names(base)[24:29]<-c("q15_job","q16_income","q17_m_daily","q18_m_medical","q19_infectiondate","q20_infectionroute")
names(base)[30:41]<-c("q21d_1_none","q21d_2_yigan","q21d_3_binggan","q21d_4_jiehe","q21d_5_meidu","q21d_6_linbing","q21d_7_shengzhidao","q21d_8_gaoxueya","q21d_9_tangniaobing","q21d_10_xinzangbing","q21d_11_other","q21d_12_noknow")
names(base)[42:49]<-c("q22d_1_none","q22d_2_ekouchuang","q22d_3_kabofei","q22d_4_meidu","q22d_5_pifu","q22d_6_fuxie","q22d_7_other","q22d_8_noknow")
names(base)[50:57]<-c("q23_smokepast","q24_smokejie","q25_smokeliang","q26_winepast","q27_winefre","q28d_1_pijiu","q28d_2_baijiu","q28d_3_qitajiu")
names(base)[58:61]<-c("q29_fspast","q30_fsfre","q31_dlpast","q32_dlidea")
names(base)[62:83]<-c("q33_jlyn","q34_jlday","q35_1_jlhour","q35_2_jlmin",
                         "q36_zdyn","q37_zdday","q38_1_zdhour","q38_2_zdmin",
                         "q39_jtyn","q40_jtday","q41_1_jthour","q41_2_jtmin",
                         "q42_jlylyn","q43_jlylday","q44_1_jlylhour","q44_2_jlylmin",
                         "q45_zdylyn","q46_zdylday","q47_1_zdylhour","q47_2_zdylmin",
                         "q48_1_jiuzuohour","q48_1_jiuzuomin")
names(base)[84:95] <- c("q49_1_webxingban","q49_2_comsex","q50_suiidea","q51_suitry","q52_sex",
                           "q53_condom","q54_drug","q55_1_yn","q55_2_time","q55_3_pian",
                           "q56_1_needday","q56_2_trueday")
names(base)[96:126] <- c("q57_qolrate","q58_qolhealth","q59_1_qolpanin","q59_2_qolshengli","q59_3_qolmedaical",
                          "q59_4_qolfun","q59_5_qolmeaning","q59_6_qolconfuse","q59_7_qolfear","q59_8_qoldeath",
                          "q59_9_qolattetion","q59_10_qolsafety","q59_11_qolenvio","q60_daily",
                          "q61_1_qolenergy","q61_2_qolwx","q61_3_qolmoney","q61_4_qoljieshou","q61_5_qolinfo","q61_6_qolxx",
                          "q62_1_qolsleep","q62_2_qolda","q62_3_qolwork","q62_4_qolself","q62_5_renji","q62_6_qolsex",
                          "q62_7_qolfriend","q62_8_qolzhu","q62_9_qolws","q62_10_qolsport","q63_dep")
names(base)[127:140] <- c("q64_1_stime","q64_2_stihe","q64_3_stilb","q64_4_stiburu","q64_5_stipaiji","q64_6_stizang","q64_7_stinof","q64_8_stidirty",
                             "q64_9_stiworld","q64_10_stiex","q64_11_stizg","q64_12_stipaichi",
                             "q64_13_stibushufu","q64_14_stie")
names(base)[141:150] <- c("q65_1_senanti","q65_2_sefandui","q65_3_sejianchi","q65_4_setu","q65_5_secz","q65_6_sednt","q65_7_selj","q65_8_semany",
                             "q65_9_semf","q65_10_seyfzr")
names(base)[151:160] <- c("q66_1_phqmen","q66_2_phqdi","q66_3_phqsleep","q66_4_phqpi","q66_5_phqeat",
                             "q66_6_phqnolike","q66_7_phqjz","q66_8_phqtingbuxialai","q66_9_phqdeath","q67_phqkunnan")
names(base)[161:170] <- c("q68_1_pss","q68_2_pss","q68_3_pss","q68_4_pss","q68_5_pss",
                             "q68_6_pss","q68_7_pss","q68_8_pss","q68_9_pss","q68_10_pss")
names(base)[171:190] <- c("q69_1_cope","q69_2_cope","q69_3_cope","q69_4_cope","q69_5_cope",
                             "q69_6_cope","q69_7_cope","q69_8_cope","q69_9_cope","q69_10_cope",
                             "q69_11_cope","q69_12_cope","q69_13_cope","q69_14_cope","q69_15_cope",
                             "q69_16_cope","q69_17_cope","q69_18_cope","q69_19_cope","q69_20_cope")
names(base)[191] <- c("phq9score")
names(base)[192 : 211] <- paste0("cesd_", 1:20)
names(base)[212] <- c("cesdscore")

#命名后检查
names(base)

#### 艺然人工更正 ####
base$q56_1_needday[base$number == 5181] <- 30 ## 5181患者漏服药和实际服药填写错误
baseinfo$q20_infectionroute[baseinfo$number == 4912] <- 5 ## 4912患者感染途径为5
baseinfo$q21d_1_none[baseinfo$number == 8614] <- 1
baseinfo$q55_1_yn[baseinfo$number %in% c(5321, 8724, 11836, 13069, 14197)] <- c(1, 0, 0, 1, 0)
# 和室友居住患者：c(5748, 7920, 8614, 10048, 11986, 12293, 13197, 13475, 13626, 13711, 13823, 14275)
baseinfo$q14d_8_other <- if_else(baseinfo$number %in% c(5748, 7920, 8614, 10048, 11986, 12293, 13197, 13475, 13626, 13711, 13823, 14275), 1, 0)
names(baseinfo)[which(names(baseinfo) == 'q14d_8_other')] <- 'q14d_8_roommate'
#**************************************数据导出*****************************************************#

#数据清理完成导出
write.csv(base, "base.csv", row.names = FALSE)


#**************************************加入分组信息*****************************************************#
df.info <- data.table::fread('info0105.csv', select = c(2, 4, 6, 17, 18, 20))

names(df.info)
# 重命名
names(df.info) <- c('group', 'id', 'number', 'cd4time', 'cd4', 'ruzuweek')
#merge by id 按治疗号匹配
baseinfo<-merge(base, df.info, by="number")  

head(baseinfo)
names(baseinfo)

#数据清理完成导出
write.csv(baseinfo, file="baseinfo_0106.csv", row.names = FALSE)

#******************************************* E N D *****************************************************#

mytable <- with(baseinfo,table(group))
mytable
options(digits=3)
prop.table(mytable)*100


