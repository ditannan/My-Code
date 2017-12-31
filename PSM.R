total <- read.csv('total.csv')

library(dplyr)

table(is.na(total))
summary(total)
table(total$有无转移)
table(total$部位)
table(total$性别)
table(total$TD有无,total$N分期)

chisq.test(total$T分期, total$TD有无)
chisq.test(total$TD有无, total$N分期)

total['肿瘤长'] <- ifelse(total$瘤长<5, 0, 1)
total$肿瘤长 <- as.factor(total$肿瘤长)


t.test(total$年龄, total$TD有无)

# 样本匹配
library(MatchIt)
match.it <- matchit(TD有无 ~ T分期 + N分期 + 年龄
                    + 部位 + 分化程度 + 有无转移 + 肿瘤长, 
                    data = total, method="nearest", ratio=1, discard = 'both')
a <- summary(match.it)
# 匹配情况
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1 : ncol(total)]
chisq.test(df.match$TD有无, df.match$部位)

