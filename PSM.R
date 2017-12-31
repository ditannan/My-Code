total <- read.csv('total.csv')

library(dplyr)

table(is.na(total))
summary(total)

table(total$N.period, total$is.td.new)
chisq.test(total$is.td.new, total$N.period)


chisq.test(total$T.period, total$is.td.new)

total['tumor.size.gp'] <- ifelse(total$tumor.size<5, 0, 1)
total$tumor.size.gp <- as.factor(total$tumor.size.gp)


t.test(total$age ~ total$is.td.new)

table(total$is.td.new)
# 样本匹配
library(MatchIt)
match.it <- matchit(is.td.new ~ T.period + N.period + age
                    + tumor.loc + dif.level + is.trans + tumor.size.gp, 
                    data = total, method="nearest", ratio=1, discard = 'both')
a <- summary(match.it)
# 匹配情况
library(knitr)
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1 : ncol(total)]
chisq.test(df.match$is.td.new, df.match$tumor.loc)
chisq.test(df.match$is.td.new, df.match$gender)
chisq.test(df.match$is.td.new, df.match$dif.level)
chisq.test(df.match$is.td.new, df.match$T.period)
chisq.test(df.match$is.td.new, df.match$N.period)
chisq.test(df.match$is.td.new, df.match$tumor.size.gp)
chisq.test(df.match$is.td.new, df.match$is.trans)
t.test(df.match$age ~ df.match$is.td.new)

id <- c(df.match$ptid)
is.vector(id)

write.csv(df.match, 'match.csv')

df2006 <- read.csv('df2006.csv')
df2007 <- read.csv('df2007.csv', skip = 1)
df2008 <- read.csv('df2008.csv', skip = 1)
df2009 <- read.csv('df2009.csv', skip = 1)

str(df2006)

df2006.new <- df2006[df2006$院号 %in% id, ]
df2007.new <- df2007[df2007$院号 %in% id, ]
df2008.new <- df2008[df2008$院号 %in% id, ]
df2009.new <- df2009[df2009$院号 %in% id, ]

write.csv(df2006.new, 'df2006new.csv')
write.csv(df2007.new, 'df2007new.csv')
write.csv(df2008.new, 'df2008new.csv')
write.csv(df2009.new, 'df2009new.csv')

mystat <- function(x){
  m.sd = c()
  mean <- mean(x)
  std <- sd(x)
  m.sd <- c(m.sd, mean, std)
  m.sd
}
mystat(total$age)
