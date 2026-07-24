####################################################
#### WEEK 9 Solutions
####################################################

rm(list=ls())

library(tidyverse)

# 1
setwd()

ks2 <- read.csv("ks2.csv")

ks2 <- ks2[complete.cases(ks2),]
# 12 observations lost.

# 2
with(ks2, c(mean(reading), sd(reading)))

with(ks2, c(mean(maths), sd(maths)))

with(ks2, c(mean(gps), sd(gps)))

# The mean mark of GPS (105.76) is higher than reading (103.56) and maths (104.70).
# This indicates that on average students performed better in GPS than reading or maths.
# The standard deviation of reading (5.42) is higher than maths (5.10) and GPS (4.61).
# This indicates that marks are more spread out from the mean for reading than
# they are for maths and GPS.

# 3
t.test(ks2$reading, alternative = "two.sided", mu = 100)

t.test(ks2$maths, alternative = "two.sided", mu = 100)

t.test(ks2$gps, alternative = "two.sided", mu = 100)

# P < 0.05; CI does not contain 100; 2 < t (2 t rule of thumb for 95% confidence level).
# For all three tests, the means are statistically different from 100.

# 4
t.test(ks2$avg_all, alternative = "less", mu = 105, conf.level = 0.99)
# Mean is statistically less than 100 at 99% confidence level.

# 5.A
ks2$english <- with(ks2, (reading + gps) / 2)

# 5.B
t.test(ks2$english, alternative = "less", mu = 105, conf.level = 0.999)
# Mean is statistically less than 105.

# 5.C
t.test(ks2$english, alternative = "two.sided", mu = 105, conf.level = 0.999)
# Mean is not statistically different from 105.

# 5.D
# The first t test is statistically significant but the second one is not,
# despite having the same mu value and the same values in the variable.
# The second t test loses its statistical significance due to being a
# two-sided t test whilst the first t test is a one-sided t test. This means
# that the first test has a statistically significant tail consisting of
# 0.001 of the total area under the normal probability distribution,
# whilst the second test has two statistically significant tails consisting
# a total of 0.001 of the total area under the distribution, or 0.0005 under
# the distribution for each individual tail. Because of this, the confidence
# interval of the two-sided t test is wider whilst the one for the one-sided
# t test is smaller, meaning it is harder for the two-sided t test to
# be statistically significant, showing why it is not significant when
# the one-sided t test is significant. This is illustrated in the below
# one-sided t test, which has a confidence level of 99.95%:
t.test(ks2$english, alternative = "less", mu = 105, conf.level = 0.9995)
# Note the statistical insignificance and the same confidence interval
# as the two-sided t test.

# 6.A
ks2 %>%
  mutate(maths_pass = cut(maths,
                          breaks = c(80, 100, 120),
                          labels = c("Fail", "Pass"),
                          right = FALSE,
                          include.lowest = TRUE)) -> ks2


##########################################################################################
# BASE R
##########################################################################################
# ks2$maths_pass <- as.factor(ifelse(ks2$maths < 100, 'Fail', 'Pass'))
##########################################################################################


# 6.B
prop.test(table(ks2$maths_pass), alternative = "less", p = 0.10, conf.level = 0.95)
# Statistically insignificant, indicating that the proportion of students who
# fail maths is not statistically less than 10%.

# 6.C
t.test(ks2$english[ks2$maths_pass == "Fail"], alternative = "less", mu = 100, conf.level = 0.95)
# Statistically significant, indicating that students who fail maths achieve
# statistically less than 100 in English on average, meaning they also fail English.

# 6.D
t.test(ks2$gps[ks2$maths_pass == "Pass"], alternative = "greater", mu = 105, conf.level = 0.95)
# Statistically significant, indicating that students who pass maths achieve
# statistically greater than 105 in GPS on average.

# 7.a
ks2 %>%
  mutate(pass_fail = ifelse(reading < 100 | maths < 100 | gps < 100,
                            "Fail", "Pass")) -> ks2


##########################################################################################
# BASE R
##########################################################################################
# ks2$pass_fail <- as.factor(ifelse(ks2$reading < 100 | ks2$maths < 100 | ks2$gps < 100, 'Fail', 'Pass'))
##########################################################################################

# 7.b
t.test(ks2$avg_all[ks2$pass_fail == "Fail"], alternative = "less", mu = 100, conf.level = 0.95)
# Statistically significant, indicating that students who fail one or more test
# have an overall average mark less than 100, on average. This is expected. However,
# the practical significance is worth drawing attention to; the mean of this group
# is 98.88% and the upper CI is 99.23%, indicating that the mean mark of this
# group is only just below the pass mark, suggesting that whilst the difference
# is statistically significant, there is not much practical significance as
# students who fail only seem to fail relatively narrowly on average.

# 8.a
ks2$normal <- with(ks2, ((avg_all - 80) / (120 - 80)) * 100)
# This scale is useful since it is now percentage based, with 0 representing
# a 0% score (excluding ungraded marks), 50 representing a 50% score,
# and 100 representing a 100% score. This makes it more easy to interpret than
# the previous 80-120 scale.

# 8.b
ks2 %>%
  mutate(grade = cut(normal, breaks = c(0, 50, 60, 70, 80, 100),
                     labels = c("Fail", "Pass", "Merit", "Distinction", "Distinction+"),
                     right = FALSE,
                     include.lowest = TRUE)) -> ks2


##########################################################################################
# BASE R
##########################################################################################
# ks2$grade <- as.factor(ifelse(ks2$normal < 50, 'Fail',
#                               ifelse(ks2$normal < 60, 'Pass',
#                                      ifelse(ks2$normal < 70, 'Merit',
#                                             ifelse(ks2$normal < 80, 'Distinction', 'Distinction+')))))
##########################################################################################


# 8.c.i
t.test(ks2$normal[ks2$grade == "Pass"], alternative = "two.sided", 
       mu = 55, conf.level = 0.95)
# No, the average mark is approximately 55%.

t.test(ks2$normal[ks2$grade == "Merit"], alternative = "two.sided", 
       mu = 65, conf.level = 0.95)
# Yes, the average mark is approximately 64%, lower than 65%.

t.test(ks2$normal[ks2$grade == "Distinction"], alternative = "two.sided", 
       mu = 75, conf.level = 0.95)
# Yes, the average mark is approximately 73%, lower than 75%.

# 8.c.ii
t.test(ks2$normal[ks2$grade == "Distinction+"], alternative = "less", 
       mu = max(ks2$normal), conf.level = 0.95)
# Yes, the average mark of 83% is lower than the maximum mark of 91%.

# 8.c.iii
t.test(ks2$normal[ks2$grade == "Fail"], alternative = "two.sided", 
       mu = median(ks2$normal[ks2$grade == "Fail"]), conf.level = 0.95)
# No, the mean mark of 41% is lower than the median mark of 45%.
