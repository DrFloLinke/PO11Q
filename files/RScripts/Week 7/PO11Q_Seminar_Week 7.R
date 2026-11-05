#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 7
#########################################################################


# clear R memory
rm(list=ls())

# Set Working Directory (insert your file path, or choose from the menu 'Session')
setwd()

# >>>> Please run the RScript from Week 5 now <<<<



# PO11Q - Worksheet Week 7
##########################

mean(EU$pop18)

median(EU$pop18)

summary(EU$pop18)

min(EU$pop18)

max(EU$pop18)

range(EU$pop18)

sd(EU$pop18)

var(EU$pop18)

# Graphs

## Basic Graphs

EU$popmio <- EU$pop18/1000000

hist(EU$popmio, breaks=4)

hist(EU$popmio, breaks=4,
xlab="Population in million",
main="Histogram of EU Population (2018)")


boxplot(EU$popmio, horizontal = TRUE)

summary(EU$popmio)

# Advanced Graphs

library(ggplot2)

ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20)


ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20, 
boundary = 0)



ggplot(data=EU) +
geom_histogram(mapping=aes(popmio), binwidth = 20, 
boundary = 0) +
scale_x_continuous(breaks = seq(0, 100, 20)) +
labs(x="Population (in million)", y="Frequency")


####################################################################
# BASE R
####################################################################
# hist(EU$popmio, breaks = 4,
#      xlab = "Population (in million)", ylab = "Frequency") 
####################################################################




  
# Even More Advanced Graphs
  
ggplot(data = EU) + 
  geom_histogram(binwidth = 20, boundary = 0,
                 aes(x= popmio, 
                     y = (..count..)/sum(..count..)*100)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x="Population (in million)", y="percent") 


####################################################################
# BASE R
####################################################################
# Create histogram data without plotting
# h <- hist(EU$popmio, 
#          breaks = seq(0, 100, by = 20), 
#          plot = FALSE)

# Normalize counts to percentages
# h$counts <- h$counts / sum(h$counts) * 100

# Plot histogram
# plot(h, 
#     freq = FALSE, 
#     xlab = "Population (in million)", 
#     ylab = "percent", 
#     main = "")
####################################################################




