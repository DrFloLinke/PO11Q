#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 7 - Solutions
#########################################################################

# set working directory

setwd()

# Load packages
library(tidyverse)
library(readxl)

# load data set
EU <- read_excel("EU.xlsx")

###############
# Exercises
###############

# Calculate GDP per capita in a new variable, and sort countries 
# in descending order (for this it is ok to mix 2015 and 2018 data).
######################################################################

EU$gdppc <- EU$GDP_2015/EU$pop18

#or 

EU$gdppc <- with(EU, GDP_2015/pop18)

EU_arrange <- EU |>
  arrange(desc(EU$gdppc))


# BASE R
# -------------------------------------
# EU_arrange <- EU[order(-EU$gdppc),]



EU_arrange


# Which country is the least and which is the most densely populated?
######################################################################

## Population Density

EU$density <- with(EU, pop18/area)

## Sort by descending density
EU_density <- EU |>
  arrange(desc(density))


# BASE R
# -------------------------------------
# EU_density <- EU[order(-EU$density),]



## print all rows of the data frame
EU_density |> print(n = Inf)

## or more elegantly, only showing the two observations we are interested in

EU_density$density[c(1,length(EU_density$density))]

## or even more elegantly

with(EU_density, density[c(1,length(density))])

## which countries are these?

with(EU_density, country[c(which.max(density), which.min(density))])



# BASE R
# ----------------------------------------
# print(EU_density, n = nrow(EU_density))







# Recode the pop18 variable into a categorical variable with low, 
# medium, and high population (cut-off points are your choice).
#####################################################################

summary(EU$pop18)

EU <- EU |>
  mutate(popcat=
           ordered(
             cut(pop18, breaks=c(0, 3781345, 17766718, 83000000),
                 labels=c("low","medium","high"))))



# BASE R
# -----------------------------------------------------------
# EU$popcat <- as.factor(ifelse(EU$pop18 <= 3781345, 'low',
# ifelse(EU$pop18 <= 17766718, 'medium', 'high')))



table(EU$popcat)


#
# EOF
#