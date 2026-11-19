#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 9
#########################################################################

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
