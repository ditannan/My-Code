# loading packages
library(dplyr)
library(plyr)
library(magrittr)

# read data
df <- data.frame(read.csv("aa-0112.csv",stringsAsFactors=FALSE))
# View data
# View(df)
# column names 
names(df)

# explore data with graph
EDA <- function (x){
  par(mfrow=c(2,2)) # 同时显示4个图  
  hist(x) # 直方图  
  dotchart(x) # 点图 
  boxplot(x,horizontal=T) # 箱式图 
  qqnorm(x);qqline(x) # 正态概率图 
  par(mfrow=c(1,1)) # 恢复单图 
}


# description function
mydes <- function(x, na.omit = FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  return(c(n = n, mean = m, stdev = s))
}


# calculate cesd score
attach(df)
df$cesdscore1 <- cesd_1 + cesd_2 + cesd_3 + cesd_4 + cesd_5 + cesd_6 +
  cesd_7 + cesd_8 + cesd_9 + cesd_10 + cesd_11 + cesd_12 + cesd_13 +
  cesd_14 - 14 + 16 - cesd_15 - cesd_16 - cesd_17 - cesd_18 +
  cesd_19 + cesd_20 -2
detach(df)


# select variables and range by q1_id
df.sub1 <- df %>% 
  select(q1_id, group, pssscore, poscope, negcope, cesdscore1, MET, 
         gsesscore, qolscore, obs, starts_with('QOL'), pssrank) %>% 
  arrange(q1_id)

# calculate the change of pss, poscope, negcope, seif efficacy, QOL
df.sub1.d <- ddply(df.sub1, .(q1_id), mutate, d_pss = pssscore[2] - pssscore[1]) %>% 
  ddply(.(q1_id), mutate, d_poscope = poscope[2] - poscope[1]) %>%
  ddply(.(q1_id), mutate, d_negcope = negcope[2] - negcope[1]) %>%
  ddply(.(q1_id), mutate, d_cesd = cesdscore1[2] - cesdscore1[1]) %>%
  ddply(.(q1_id), mutate, d_MET = MET[2] - MET[1]) %>%
  ddply(.(q1_id), mutate, d_gsesscore = gsesscore[2] - gsesscore[1]) %>%
  ddply(.(q1_id), mutate, d_qolscore = qolscore[2] - qolscore[1]) %>%
  ddply(.(q1_id), mutate, group = group[1]) %>%
  mutate(QOL.PHYS = (QOLQ3 + QOLQ4 + QOLQ14 + QOLQ21)/4*4, # 生理领域
         QOL.PSYCH = (QOLQ6 + QOLQ11 + QOLQ15 + QOLQ24 + QOLQ31)/5*4, # 心理领域
         QOL.IND = (QOLQ5 + QOLQ20 + QOLQ22 + QOLQ23)/4*4, # 独立性领域
         QOL.SOCIL = (QOLQ17 + QOLQ25 + QOLQ26 + QOLQ27)/4*4, # 社会关系领域
         QOL.ENVIR = (QOLQ12 + QOLQ13 + QOLQ16 + QOLQ18 + 
                        QOLQ19 + QOLQ28 + QOLQ29 + QOLQ30)/8*4, # 环境领域
         QOL.SPIRIT = (QOLQ7 + QOLQ8 + QOLQ9 + QOLQ10)/4*4, # 精神支柱/个人信仰领域
         QOL.OVERALL = (QOLQ1 + QOLQ2)/2*4, # 总体健康状况和生存质量
         QOL.TOTAL = QOL.PHYS + QOL.PSYCH + QOL.IND + QOL.SOCIL + QOL.ENVIR + QOL.SPIRIT #总分
  ) %>% 
  ddply(.(q1_id), mutate, d_qol.phys = QOL.PHYS[2] - QOL.PHYS[1], 
        d_qol.psych = QOL.PSYCH[2] - QOL.PSYCH[1], 
        d_qol.ind = QOL.IND[2] - QOL.IND[1], 
        d_qol.socil = QOL.SOCIL[2] - QOL.SOCIL[1], 
        d_qol.envir = QOL.ENVIR[2] - QOL.ENVIR[1], 
        d_qol.spirit = QOL.SPIRIT[2] - QOL.SPIRIT[1],
        d_qol.overall = QOL.OVERALL[2] - QOL.OVERALL[1], 
        d_qol.total = QOL.TOTAL[2] - QOL.TOTAL[1])


# baseline data
df.bl <- filter(df.sub1.d, obs == 1)
# follow data
df.fl <- filter(df.sub1.d, obs == 2)
# baseline of group A 
df.bl.gpA <- df.bl %>% filter(group == 'A')
# baseline of group B
df.bl.gpB <- df.bl %>% filter(group == 'B')
# followup of group A 
df.fl.gpA <- df.fl %>% filter(group == 'A')
# followup of group B
df.fl.gpB <- df.fl %>% filter(group == 'B')
# 均数&标准差
des_md <- function(x){
  x <- c(x)
  total <- mydes(as.numeric(unlist(df.bl[x])))
  groupA <- mydes(as.numeric(unlist(df.bl.gpA[x])))
  groupB <- mydes(as.numeric(unlist(df.bl.gpB[x])))
  des <- data.frame(total, groupA, groupB)
  return(des)
}

des_mdfl <- function(x){
  x <- c(x)
  total <- mydes(as.numeric(unlist(df.fl[x])))
  groupA <- mydes(as.numeric(unlist(df.fl.gpA[x])))
  groupB <- mydes(as.numeric(unlist(df.fl.gpB[x])))
  des <- data.frame(total, groupA, groupB)
  return(des)
}

# baseline statistical description 
df.bldes <- df.bl %>% ddply(.(group), summary)
df.bl$QOL.TOTAL %>% mydes()
df.bl.gpA$QOL.TOTAL %>% mydes()
df.bl.gpB$QOL.TOTAL %>% mydes()
# statistical description after three mongths
df.fldes <- df.fl %>% ddply(.(group), summary)

############### Chunk 1 PSS##############
### compare baseline pssscore
# test normality of pss of group A, B
# group A
shapiro.test(df.bl.gpA$pssscore)
df.bl.gpA$pssscore %>% EDA()
# group B
shapiro.test(df.bl.gpB$pssscore)
df.bl.gpB$pssscore %>% EDA()
# baseline t-test
df.bl %>% wilcox.test(pssscore ~ group, .)

### compare change of pss
# test normality of d_pss
# group A
shapiro.test(df.bl.gpA$d_pss)
df.bl.gpA$d_pss %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_pss)
df.bl.gpB$d_pss %>% EDA()
# t-test of pss_change
df.bl %>% wilcox.test(d_pss ~ group, .)

des_md('d_pss')

############### Chunk 2 P-COPE##############
### compare baseline poscope
# test normality of negcope of group A, B
# group A
shapiro.test(df.bl.gpA$poscope)
EDA(df.bl.gpA$poscope)
# group B
shapiro.test(df.bl.gpB$poscope)
df.bl.gpB$poscope %>% EDA()
# baseline t-test
df.bl %>% t.test(poscope ~ group, .)

### compare change of poscope
# test normality of d_poscope
# group A
shapiro.test(df.bl.gpA$d_poscope)
df.bl.gpA$d_poscope %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_poscope)
df.bl.gpB$d_poscope %>% EDA()
# t-test of poscope_change
df.bl %>% wilcox.test(d_poscope ~ group, .)

############### Chunk 3 N-COPE##############
### compare baseline negcope
# test normality of negcope of group A, B
# group A
shapiro.test(df.bl.gpA$negcope)
df.bl.gpA$negcope %>% EDA()
# group B
shapiro.test(df.bl.gpB$negcope)
df.bl.gpB$negcope %>% EDA()
# baseline t-test
df.bl %>% wilcox.test(negcope ~ group, .)

### compare change of negcope
# test normality of d_negcope
# group A
shapiro.test(df.bl.gpA$d_negcope)
df.bl.gpA$d_negcope %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_negcope)
df.bl.gpB$d_negcope %>% EDA()
# t-test of negcope_change
df.bl %>% t.test(d_negcope ~ group, .)

############### Chunk 4 CESD##############
### compare baseline cesd
# test normality of cesd of group A, B
# group A
shapiro.test(df.bl.gpA$cesdscore1)
df.bl.gpA$cesdscore1 %>% EDA()
# group B
shapiro.test(df.bl.gpB$cesdscore1)
df.bl.gpB$cesdscore1 %>% EDA()
# baseline t-test
df.bl %>% wilcox.test(cesdscore1 ~ group, .)

### compare change of cesd
# test normality of d_cesd
# group A
shapiro.test(df.bl.gpA$d_cesd)
df.bl.gpA$d_cesd %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_cesd)
df.bl.gpB$d_cesd %>% EDA()
# t-test of cesd_change
df.bl %>% wilcox.test(d_cesd ~ group, .)

des_md('d_cesd')

############### Chunk 5 PA ##############
### compare baseline 
# test normality of PA of group A, B
# group A
shapiro.test(df.bl.gpA$MET)
df.bl.gpA$MET %>% EDA()
# group B
shapiro.test(df.bl.gpB$MET)
df.bl.gpB$MET %>% EDA()
# baseline t-test
df.bl %>% wilcox.test(MET ~ group, .)

### compare change of MET
# test normality of d_MET
# group A
shapiro.test(df.bl.gpA$d_MET)
df.bl.gpA$d_MET %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_MET)
df.bl.gpB$d_MET %>% EDA()
# t-test of MET change
df.bl %>% wilcox.test(d_MET ~ group, .)

############### Chunk 6 GSES ##############
### compare baseline 
# test normality of GSES of group A, B
# group A
shapiro.test(df.bl.gpA$gsesscore)
df.bl.gpA$gsesscore %>% EDA()
# group B
shapiro.test(df.bl.gpB$gsesscore)
df.bl.gpB$gsesscore %>% EDA()
# baseline t-test
df.bl %>% t.test(gsesscore ~ group, .)

### compare change of gsesscore
# test normality of d_gsesscore
# group A
shapiro.test(df.bl.gpA$d_gsesscore)
df.bl.gpA$d_gsesscore %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_gsesscore)
df.bl.gpB$d_gsesscore %>% EDA()
# t-test of gses change
df.bl %>% wilcox.test(d_gsesscore ~ group, .)

############### Chunk 6 QOL ##############
### compare baseline 
# test normality of QOL of group A, B
# group A
shapiro.test(df.bl.gpA$QOL.TOTAL)
df.bl.gpA$QOL.TOTAL %>% EDA()
# group B
shapiro.test(df.bl.gpB$QOL.TOTAL)
df.bl.gpB$QOL.TOTAL %>% EDA()
# baseline t-test
df.bl %>% t.test(QOL.TOTAL ~ group, .)

### compare change of QOL
# test normality of d_qolscore
# group A
shapiro.test(df.bl.gpA$d_qol.total)
df.bl.gpA$d_qol.total %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_qol.total)
df.bl.gpB$d_qol.total %>% EDA()
# t-test of qol change
df.bl %>% t.test(d_qol.total ~ group, .)

des_md('d_qol.total')

# qol in 3-month follow-up
df.fl %$% by(QOL.TOTAL, group, mydes)
df.fl %>% t.test(QOL.TOTAL ~ group, .)

############### Chunk 7 QOL.PHYS ##############
### compare baseline 
# test normality of QOL.PHYS of group A, B
# group A
shapiro.test(df.bl.gpA$QOL.PHYS)
df.bl.gpA$QOL.PHYS %>% EDA()
# group B
shapiro.test(df.bl.gpB$QOL.PHYS)
df.bl.gpB$QOL.PHYS %>% EDA()
# baseline wilcox-test
df.bl %>% wilcox.test(QOL.PHYS ~ group, .)

### compare change of QOL.PHYS
# test normality of d_qol.phys
# group A
shapiro.test(df.bl.gpA$d_qol.phys)
df.bl.gpA$d_qol.phys %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_qol.phys)
df.bl.gpB$d_qol.phys %>% EDA()

# t-test of qol.phys change
df.bl %>% t.test(d_qol.phys ~ group, .)

# mean-change and standard deviation chagne
des_md('d_qol.phys')

############### Chunk 8 QOL.PSYCH ##############
### compare baseline 
# test normality of QOL.PSYCH of group A, B
# group A
shapiro.test(df.bl.gpA$QOL.PSYCH)
df.bl.gpA$QOL.PSYCH %>% EDA()
# group B
shapiro.test(df.bl.gpB$QOL.PSYCH)
df.bl.gpB$QOL.PSYCH %>% EDA()
# baseline wilcox-test
df.bl %>% wilcox.test(QOL.PSYCH ~ group, .)

### compare change of QOL.PSYCH
# test normality of d_qol.psych
# group A
shapiro.test(df.bl.gpA$d_qol.psych)
df.bl.gpA$d_qol.psych %>% EDA()

# group B
shapiro.test(df.bl.gpB$d_qol.psych)
df.bl.gpB$d_qol.psych %>% EDA()
# t-test of qol.psych change
df.bl %>% t.test(d_qol.psych ~ group, .)

des_md('d_qol.psych')

############### Chunk 9 QOL.IND ##############
# t-test of qol.ind change
df.bl %>% t.test(d_qol.ind ~ group, .)

des_md('d_qol.ind')

############### Chunk 10 QOL.SOCIL ##############
# t-test of qol.socil change
df.bl %>% t.test(d_qol.socil ~ group, .)

des_md('d_qol.socil')

############### Chunk 11 QOL.ENVIR ##############
# t-test of qol.envir change
df.bl %>% t.test(d_qol.envir ~ group, .)

des_md('d_qol.envir')

############### Chunk 12 QOL.SPIRIT ##############
# t-test of qol.spirit change
df.bl %>% t.test(d_qol.spirit ~ group, .)

des_md('d_qol.spirit')

