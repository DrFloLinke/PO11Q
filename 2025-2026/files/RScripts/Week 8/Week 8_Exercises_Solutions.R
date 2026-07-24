###########################
# Week 8 Exercises
###########################

rm(list=ls())

setwd()

library(haven)
library(tidyverse)

fearon <- read_dta("fearon.dta")
fearonfull <- read_dta("fearonfull.dta")

#1. 	Produce a new data frame of df1, df2, df3, df4. For each dataset, arrange them according to countries with:
#a.	Lowest Ethnic Fractionalisation
#b.	Highest Population
#c.	Highest mountainous terrain
#d.	Lowest elevation

df1<-arrange(fearon, ef)

df2<-arrange(fearon, desc(pop))

df3<-arrange(fearon, desc(mtnest))

df4<-arrange(fearon, elevdiff)

####################################################################
# BASE R
####################################################################
# df1 <- fearon[order(fearon$ef),]
# df2 <- fearon[order(-fearon$pop),]
# df3 <- fearon[order(-fearon$mtnest),]
# df4 <- fearon[order(fearon$elevdiff),]
####################################################################


#2. Create a dataframe which looks upon countries that used to be British colonies. Transform the British colony variable into a binary dummy of “No” and “Yes”. 


fearon$brit = factor(fearon$colbrit)

fearon <- fearon %>%
   mutate(british = recode(brit, '0'="No", 
                                 '1'= "Yes"))

####################################################################
# BASE R
####################################################################
# fearon$british <- as.factor(ifelse(fearon$brit == 0, 'No', 'Yes'))
####################################################################


table(fearon$british)


#3.If we order all countries by population size, what is the population size at the 40th percentile?

quantile(fearon$pop, probs = 0.4)



#4. Make a dummy variable which shows whether or not a country is majority Muslim, given that the requirement of a majority Muslim country is more than 50% of its population. Do not forget to order the factors.
#a. Use cut()
#b. Use ifelse()

#a
fearon2 <- fearon %>% 
   mutate(muslimcoun=ordered(
      cut(muslim, breaks=c(-0.1, 50, 101),labels=c("No","Yes"))))

fearon2$muslimcoun <-factor(fearon2$muslimcoun ,levels =c("Yes", "No"))

table(fearon2$muslimcoun)

#b
fearon3 <- fearon %>% 
   mutate(muslimcoun = factor(ifelse(muslim>50, "Yes", "No"), 
                              levels =c("Yes", "No")))


###########################################################################
# BASE R
###########################################################################
# fearon$muslimcoun <- as.factor(ifelse(fearon$muslim <= 50, 'No', 'Yes'))

# fearon$muslimcoun <-factor(fearon$muslimcoun ,levels =c("Yes", "No"))
###########################################################################


table(fearon3$muslimcoun)


#5. The Polity IV score measures democracy on a scale from -10 to +10 where -10 is equal to perfect autocracy and +10 equal to perfect democracy. Often, the democratisation literature distinguishes between autocracies, anocracies and democracies. We can achieve this differentiation in the Polity IV score as follows...
# a. Into an ordered factor
# b. Into a binary dummy variable – Democracy/Non-Democracy

fearon_polity <- fearon%>%mutate(polity_order=
                                    ordered(cut
                                            (polity2, breaks=c(-10.5, -5.5, 5.5, 10.5),
                                               labels=c("Autocracies","Anocracies","Democracies"))))


table(fearon_polity$polity_order)

##################################################################################
# BASE R
##################################################################################
# fearon$polity_order <- as.factor(ifelse(fearon$polity2 <= -5.5, 'Autocracies',
# ifelse(fearon$polity2 <= 5.5, 'Anocracies', 'Democracies')))

# table(fearon$polity_order)
##################################################################################


fearon_polity1 <- fearon_polity %>%
   mutate(democracy = recode(polity_order, "Democracies"="Yes",
                                          "Autocracies" = "No",
                                          "Anocracies" = "No"))
table(fearon_polity1$democracy)


#################################################################################################
# BASE R
#################################################################################################
# fearon$democracy <- as.factor(ifelse(fearon$polity_order == 'Democracies', 'Yes', 'No'))

# table(fearon$democracy)
#################################################################################################



#6.	Find the mean difference between: 
# a. the share of largest ethnic group and second largest ethnic group
fearon$diffshare <- fearon$plural-fearon$second
mean(fearon$diffshare, na.rm=TRUE)

# b. the share of largest ethnic group and second largest ethnic group within former British colonies (using previous exercise no. 4)
fearon_brit <- filter(fearon, british=="Yes")
fearon_brit$diffeth <- fearon_brit$plural-fearon_brit$second
mean(fearon_brit$diffeth, na.rm=TRUE)

#7. a. Create an age-group dataframe like the following; 
#   Age Group
#   **********
#    NA
#    0-18
#    18-35
#    35-50
#    50-70
#b. Transform the values into numeric values, and separate into a lower and upper category
#c. Create a column - Find the mid of each levels
#d. Create a column - Find the interval of each levels

agegroup <- data.frame(x = c(NA, "0-18", "18-35", "35-50", "50-70")) 

agegroup %>%
separate(x, c("lower", "upper"), 
           sep="-", remove = FALSE) -> agegroup


###################################################################################
# BASE R
###################################################################################
# Split the "x" column on "-" using strsplit
# splits <- strsplit(as.character(agegroup$x), "-")

# Extract lower and upper bounds safely
# agegroup$lower <- sapply(splits, function(z) if (length(z) >= 1) z[1] else NA)
# agegroup$upper <- sapply(splits, function(z) if (length(z) >= 2) z[2] else NA)
###################################################################################


agegroup$lower <- as.numeric(as.character(agegroup$lower))
agegroup$upper <- as.numeric(as.character(agegroup$upper))

agegroup$mid <- (agegroup$lower + agegroup$upper)/2


#8. For this exercise use the data frame `fearonfull` which contains data for all years between 1945 and 1999. Population is defined in terms of 1000s. 
#a.  Create a new data which consists of countries with more than 10000000 people.  (large population)
#b.  Find the average of population of each large population country from 1945-1999. Hint: group according to country, then find the mean

fearonpop <- filter(fearonfull, pop > 10000)
fearonpop1 <- fearonpop %>%
   group_by(country) %>%
   summarise(avgpop=mean(pop))

###########################################################################
# BASE R
###########################################################################
# fearonpop <- fearonfull[fearonfull$pop > 10000,]
# fearonpop1 = aggregate(fearonpop$pop, list(fearonpop$country), mean)
###########################################################################

fearonpop1


#9. Sub-setting data, using the 'fearonfull' data frame:
#a. Extract the necessary variables to compare the social fractionalization between countries
#b. Retain the last row of the dataset
#c. Filter the dataset with only countries in an ongoing war (variable=ended)
#d. Find the country with ongoing war in 1999.

a <- select(fearonfull, c(ef, plural, second, relfrac, plurrel, minrelpc))

b <- slice(fearonfull,n())

c <- filter(fearonfull, ended==0)
c

d <- fearonfull %>% filter(year==1999 & ended==0) %>% select(country)
d

############################################################################################
# BASE R
############################################################################################
# a <- subset(fearonfull, select = c(ef, plural, second, relfrac, plurrel, minrelpc))

# b <- fearonfull[c(nrow(fearonfull)),]

# c <- fearonfull[fearonfull$ended == 0,]

# d1 <- fearonfull[fearonfull$year == 1999 & fearonfull$ended == 0,]
# d <- subset(d1, select = c(country))
############################################################################################


#10. Find all countries with gdp per capita greater than the world's mean in 1999 (you need to use `fearonfull` again).

meangdp <- filter(fearonfull, year==1999)
meangdp1 <- meangdp %>% filter (gdpen>mean(meangdp$gdpen,  na.rm=TRUE)) %>% select(country)


###########################################################################
# BASE R
###########################################################################
# Subset rows where year is 1999
# meangdp <- fearonfull[fearonfull$year == 1999, ]

# Calculate mean GDP (excluding NA)
# mean_gdpen <- mean(meangdp$gdpen, na.rm = TRUE)

# Filter countries with gdpen > mean, then select only 'country' column
# meangdp1 <- meangdp[meangdp$gdpen > mean_gdpen, "country", drop = FALSE]
# drop = FALSE ensures meangdp1 stays a data frame (like select(country)).
###########################################################################




#11. Generate a dataframe that only consist of data from 1998 and 1999. Change the data format into a wide data using spread for the observations of gdp per capita (you need to use `fearonfull` again).

fearon4 <- filter(fearonfull, year == 1998| year==1999)
fearon4_spread <- spread (fearon4, year, gdpen)


##########################################################################################
# BASE R
##########################################################################################
# Step 1: Subset rows where year is 1998 or 1999
# fearon4 <- subset(fearonfull, year == 1998 | year == 1999)

# Step 2: Reshape to wide format — spread 'year' to columns, with 'gdpen' values
# You must specify id variables (what defines each country uniquely)
# fearon4_spread <- reshape(fearon4[, c("country", "year", "gdpen")],
#                           timevar = "year",
#                           idvar = "country",
#                           direction = "wide")
##########################################################################################




#12. Generate a data consisting of list of countries and its population in 1945 and 1995
#Find the mean of population differences between 1945 and 1995 (you need to use `fearonfull` again).

fearon5 <- filter(fearonfull, year == 1945| year == 1995)

fearon5_a <- fearon5 %>%
      group_by(country) %>%
    summarise(difference=diff(pop))


##########################################################################################
# BASE R
##########################################################################################
# fearon5 <- fearonfull[fearonfull$year == 1945 | fearonfull$year == 1995,]

# First, ensure the data is sorted properly (e.g., by country and year)
# fearon5 <- fearon5[order(fearon5$country, fearon5$year), ]

# Use reshape to wide format
# wide_data <- reshape(fearon5[, c("country", "pop")],
#                      idvar = "country", timevar = "time", direction = "wide")

# Then calculate the difference directly
# fearon5_a <- data.frame(
#   country = wide_data$country,
#   difference = wide_data[, 3] - wide_data[, 2]
# )
##########################################################################################


#13. Find the oil-producing country (Oil=1) with the highest mean of gdp per capita over the years (you need to use `fearonfull` again).

fearon6 <- filter(fearonfull, Oil==1)

fearon_gdp <- fearon6 %>% 
   group_by(country) %>% 
   summarize (meangdpen=mean(gdpen)) %>% 
   arrange(desc(meangdpen))

##########################################################################################
# BASE R
##########################################################################################
# fearon6 <- fearonfull[fearonfull$Oil == 1,]
# meangdpen1 = aggregate(fearonfull$gdpen, list(fearonfull$country), mean)
##########################################################################################

fearon_gdp



#14. For this exercise use the `fearonfull` data set.
#a. Check how many missing values are in the dataset
#b. Omit rows with missing values in gdp per capita and population using filter

sum(is.na(fearonfull))
sum(is.na(fearonfull$polity2))

fearon8 <- filter(fearonfull, !is.na(gdpen), !is.na(pop))

##########################################################################################
# BASE R
##########################################################################################
# MISSING <- is.na(fearonfull$gdpen) | is.na(fearonfull$pop)
# fearon8 <- subset(fearonfull, subset = !MISSING)
##########################################################################################


#15. Use the data in exercise number 13.
#a. Summarize the mean of gdp per capita and population per country over the years. Find the mean gdp per country
#b. Filter the top 10 highest gdp countries

fearon8_b <- fearon8 %>% 
   group_by(country) %>% 
   summarize (meangdp=mean(gdpen), meanpop=mean(pop), gdp=meangdp*meanpop)

fearon_border <- fearon8_b %>% 
   arrange(desc(gdp)) %>% 
   slice(1:10)

##########################################################################################
# BASE R
##########################################################################################
# Step 1: Calculate group-wise means for gdpen and pop
# meangdp <- aggregate(gdpen ~ country, data = fearon8, FUN = mean)
# meanpop <- aggregate(pop ~ country, data = fearon8, FUN = mean)

# Step 2: Merge the two summaries
# fearon8_b <- merge(meangdp, meanpop, by = "country")

# Step 3: Calculate gdp = meangdp * meanpop
# fearon8_b$gdp <- fearon8_b$gdpen * fearon8_b$pop

# Step 4: Sort in descending order of gdp
# fearon_border <- fearon8_b[order(-fearon8_b$gdp), ]

# Step 5: Select top 10 rows
# fearon_border <- head(fearon_border, 10)
##########################################################################################


fearon_border




#16. Find the difference between each country's largest ethnic group and second largest ethnic group in 1994. Arrange the countries in ascending order based on the difference

fearon$diffshare <- fearon$plural-fearon$second

fearon_diff <-  fearon %>% 
   arrange(diffshare)

##########################################################################################
# BASE R
##########################################################################################
# fearon_diff <- fearon[order(fearon$diffshare),]
##########################################################################################

fearon_diff

#17. Using the data in exercise 16, save the dataframes as:
#a. CSV to be read in excel
#b. DTA file 
#c. R Data

write.csv(fearon_diff, file = "fearon")
write_dta(fearon_diff, "fearon.dta")
save(fearon_diff, file = "fearon.RData")



