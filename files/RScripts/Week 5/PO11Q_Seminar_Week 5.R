#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 5
#########################################################################


############################
# RStudio themes (optional)
############################

# Install the 'rsthemes' package to get additional RStudio themes
install.packages(
  "rsthemes",
  repos = c(gadenbuie = "https://gadenbuie.r-universe.dev", getOption("repos"))
)

# Install the rsthemes theme files into RStudio
rsthemes::install_rsthemes()


##########################################
# First steps: R as a calculator
##########################################

# Use R as a calculator: add 5 and 3
5 + 3

# Assign the result of 5 + 3 to an object called 'result'
result <- 5 + 3

# Display the value stored in 'result'
result


############################
# Working directory
############################

# Set the working directory (folder where R will look for and save files)
# NOTE: Change the path below to the folder on your own computer
setwd("~/Warwick/Modules/PO11Q/Seminars/Week 5/R Week 5")


##########################################
# Packages: installing and loading
##########################################

# Install the 'readxl' package (needed to read Excel files)
install.packages("readxl")

# Load the 'readxl' package so its functions are available in this session
library(readxl)

# Load the 'tidyverse' collection of packages (dplyr, ggplot2, etc.)
library(tidyverse)


############################
# Reading in the data
############################

# Read the Excel file 'EU.xlsx' (Sheet1) from the working directory into an object called 'EU'
EU <- read_excel("EU.xlsx", sheet = "Sheet1")


############################
# Inspecting the data
############################

# Open a spreadsheet-style view of the data frame 'EU'
View(EU)

# Show the first 6 rows (observations) of 'EU'
head(EU)

# List the variable (column) names in 'EU'
names(EU)

# Show the structure of 'EU' (types of variables and a preview of their values)
str(EU)


########################################################
# Variable types: factors and ordered factors
########################################################

# Treat the variable 'country' as a factor (categorical / nominal variable)
EU$country <- factor(EU$country)

# Create an ordered factor 'access_fac' from the numeric variable 'access'
# (used to represent the order of EU accession)
EU$access_fac <- factor(EU$access, ordered = TRUE)


##########################################
# Recode accession years into named waves
##########################################

# Recode 'access_fac' into an ordered factor 'wave'
# giving each accession wave a descriptive label (e.g. "Founding", "First", etc.)
EU <- EU %>%
  mutate(
    wave = recode_factor(
      access_fac,
      "1951" = "Founding",
      "1973" = "First",
      "1981" = "Mediterranean",
      "1986" = "Mediterranean",
      "1995" = "Cold War",
      "2004" = "Eastern",
      "2007" = "Eastern",
      "2013" = "Balkans",
      .ordered = TRUE   # ensure that 'wave' is treated as an ordered factor
    )
  )

# Inspect the levels (categories) of the factor variable 'wave'
levels(EU$wave)


########################################################
# Recode accession years into waves using cut()
########################################################

# Create a new factor variable 'wave1' using cut() on the numeric 'access' variable
# and assign descriptive labels to each interval
EU <- EU %>%
  mutate(
    wave1 = cut(
      access,
      breaks = c(1950, 1951, 1973, 1986, 1995, 2007, 2013),
      labels = c(
        "Founding",
        "First",
        "Mediterranean",
        "Cold War",
        "Eastern",
        "Balkans"
      )
    )
  )


######################################################################
# Creating a binary (dummy) variable: founding member or not
######################################################################

# Tidyverse approach: recode accession year into "Yes"/"No" for founding members
EU <- EU %>%
  mutate(
    founding = recode(
      access_fac,
      "1951" = "Yes",
      "1973" = "No",
      "1981" = "No",
      "1986" = "No",
      "1995" = "No",
      "2004" = "No",
      "2007" = "No",
      "2013" = "No"
    )
  )

# Shorter tidyverse approach: create 'founding' with ifelse() and make it a factor
EU <- EU %>%
  mutate(
    founding = factor(
      ifelse(access_fac == "1951", "Yes", "No"),
      levels = c("Yes", "No")
    )
  )



########################################################
# Dropping and selecting variables (columns)
########################################################

# Permanently drop the variable 'area' from the data frame 'EU'
EU$area <- NULL

# Keep only selected variables in a new data frame 'EU_pop'
# (country, population in 2018, accession factor, and founding dummy)
EU_pop <- select(EU, country, pop18, access_fac, founding)


# Create 'EU_pop1' by dropping the variables 'access' and 'GDP_2015' from 'EU'
EU_pop1 <- select(EU, -access, -GDP_2015)


########################################################
# Subsetting observations (rows) by position
########################################################

# Create 'EU_nobenelux' by dropping rows 1, 16, and 19 (Benelux countries)
EU_nobenelux <- slice(EU, -1, -16, -19)

# Create 'EU_benelux' by keeping only rows 1, 16, and 19
EU_benelux <- slice(EU, 1, 16, 19)


########################################################
# Subsetting observations based on a condition
########################################################

# Keep only countries with population > 10,000,000 in 2018
EU_pop_large <- filter(EU, pop18 > 10000000)



############################
# Ordering (sorting) data
############################

# Create a subset of the EU data with only selected variables
EU_subset <- select(EU, country, pop18, access)

# Order the data by population (ascending)
eu_order <- arrange(EU_subset, pop18)

# Display the first 10 rows of the ordered data
eu_order[1:10, ]

# Order the data by population (descending)
eu_order <- arrange(EU_subset, desc(pop18))

# Display the first 10 rows of the descending order data
eu_order[1:10, ]



##########################################
# Ordering by multiple variables
##########################################

# Order first by accession year 'access', then by population 'pop18' (both ascending)
eu_order <- arrange(EU_subset, access, pop18)

# Display the first 10 rows after ordering by two variables
eu_order[1:10, ]


########################################################
# Grouping data and calculating group summaries
########################################################

# Group the data by accession year 'access'
eu_access <- group_by(EU_subset, access)

# IMPORTANT: ungroup data when you are done with grouped operations
ungroup(EU_subset)

# Calculate average population per accession wave (grouped by 'access')
eu_popaccess <- EU_subset %>%
  group_by(access) %>%
  summarise(avg = mean(pop18))

# Inspect the resulting data frame with average population by accession year
eu_popaccess



##########################################
# Combining grouping and ordering
##########################################

# Order the grouped summary 'eu_popaccess' by average population (descending)
eu_popaccess_order <- arrange(eu_popaccess, desc(avg))

# Inspect the ordered summary table
eu_popaccess_order

