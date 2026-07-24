#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 5, Annotated
#########################################################################


############################
# RStudio themes (optional)
############################

install.packages(
  "rsthemes",
  repos = c(gadenbuie = "https://gadenbuie.r-universe.dev", getOption("repos"))
)
# This installs the package "rsthemes" from both the special gadenbuie repository
# and your usual CRAN repositories so you can use extra RStudio themes.

rsthemes::install_rsthemes()
# This installs the downloaded rsthemes themes into RStudio so they appear in the
# Appearance settings and can be chosen as editor colour themes.


########################################################
# First steps: R as a calculator
########################################################

5 + 3
# This asks R to calculate the sum of 5 and 3 and print the result (8) to the console.

result <- 5 + 3
# This creates a new object called "result" and stores the number 8 (5 + 3) in it.

result
# This prints the value stored in "result" to the console so you can see its current value.


############################
# Working directory
############################

setwd("~/Warwick/Modules/PO11Q/Seminars/Week 5/R Week 5")
# This tells R to use the specified folder as the working directory, i.e. the default
# location where R will look for files (like data sets) and where it will save output.


############################
# Packages: installing and loading
############################

install.packages("readxl")
# This downloads and installs the "readxl" package from CRAN, which provides functions
# for reading Excel files into R.

library(readxl)
# This loads the "readxl" package into the current R session so its functions (like read_excel)
# can be used without needing to prefix them with the package name.

library(tidyverse)
# This loads the "tidyverse" collection of packages (including dplyr, ggplot2, readr, etc.)
# which provide convenient tools for data manipulation and visualisation.


############################
# Reading in the data
############################

EU <- read_excel("EU.xlsx", sheet = "Sheet1")
# This reads the Excel file "EU.xlsx" from the working directory, specifically sheet "Sheet1",
# and stores the resulting data frame in an object called "EU" so you can work with the data in R.


############################
# Inspecting the data
############################

View(EU)
# This opens a spreadsheet-style viewer window in RStudio so you can visually inspect
# all rows and columns of the data frame "EU".

head(EU)
# This prints the first 6 rows of the data frame "EU" to the console, giving you a quick
# preview of how the data look.

names(EU)
# This returns a vector with the names of all variables (columns) in the "EU" data frame
# so you can see how each column is labelled.

str(EU)
# This displays the structure of the "EU" data frame, showing each variable’s name, type
# (numeric, factor, character, etc.), and a sample of its values.


############################
# Variable types: factors and ordered factors
############################

EU$country <- factor(EU$country)
# This converts the variable "country" in the data frame "EU" into a factor, i.e. a
# categorical variable that stores different country names as labelled categories.

EU$access_fac <- factor(EU$access, ordered = TRUE)
# This takes the numeric variable "access" (accession year) and converts it into an
# ordered factor called "access_fac", preserving the natural order of the years so that
# R knows the categories have an inherent ordering.


########################################################
# Recode accession years into named waves (tidyverse)
########################################################

EU <- EU %>%
  mutate(
    wave = recode_factor(
      access_fac,
      "1951"  = "Founding",
      "1973"  = "First",
      "1981"  = "Mediterranean",
      "1986"  = "Mediterranean",
      "1995"  = "Cold War",
      "2004"  = "Eastern",
      "2007"  = "Eastern",
      "2013"  = "Balkans",
      .ordered = TRUE
    )
  )
# This pipeline updates the "EU" data frame:
# 1) "EU %>%" starts with the EU data and passes it into the next function.
# 2) mutate(...) adds a new variable called "wave" to EU (or overwrites it if it exists).
# 3) recode_factor(access_fac, ...) replaces each accession year in "access_fac" with a
#    descriptive label (e.g. "1951" becomes "Founding", "1973" becomes "First", etc.).
# 4) .ordered = TRUE ensures that "wave" is an ordered factor, keeping the order implied
#    by the original "access_fac" variable.


levels(EU$wave)
# This prints the distinct levels (categories) of the factor variable "wave", in the
# order R is using internally for that factor.



############################
# Recode accession years into waves using cut()
############################

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
# This pipeline updates the data frame "EU" by adding a new variable "wave1":
# 1) The EU data is passed into mutate().
# 2) cut(access, breaks = ...) divides the accession year into the specified intervals.
# 3) labels = ... assigns descriptive labels to each interval ("Founding", "First", etc.).
# 4) The result is stored as a factor variable "wave1" that encodes the accession wave
#    categories derived from the numeric "access" variable.



########################################################
# Creating a binary (dummy) variable: founding member or not
########################################################

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
# This pipeline adds a new variable "founding" to "EU":
# 1) The EU data is passed into mutate().
# 2) recode(access_fac, ...) turns each accession year category into "Yes" if the year
#    is 1951 (founding member) and "No" otherwise.
# 3) The resulting character vector ("Yes"/"No") is stored as the new variable "founding".


EU <- EU %>%
  mutate(
    founding = factor(
      ifelse(access_fac == "1951", "Yes", "No"),
      levels = c("Yes", "No")
    )
  )
# This pipeline creates (or replaces) the "founding" variable in a more concise way:
# 1) ifelse(access_fac == "1951", "Yes", "No") checks each row:
#    - If the accession year is "1951", it returns "Yes".
#    - Otherwise, it returns "No".
# 2) factor(..., levels = c("Yes", "No")) converts the "Yes"/"No" values into a factor,
#    explicitly setting "Yes" as the first level and "No" as the second.
# 3) mutate() assigns this factor as the new "founding" variable in the data frame "EU".



########################################################
# Dropping and selecting variables (columns)
########################################################

EU$area <- NULL
# This deletes the variable (column) "area" from the data frame "EU" completely
# (no undo within R unless you reload the data).

EU_pop <- select(EU, country, pop18, access_fac, founding)
# This creates a new data frame "EU_pop" that contains only the four specified variables:
# "country", "pop18", "access_fac", and "founding", dropping all other columns.

EU_pop1 <- select(EU, -access, -GDP_2015)
# This creates "EU_pop1" by starting from the full "EU" data and dropping the variables
# "access" and "GDP_2015", keeping all remaining columns.


########################################################
# Subsetting observations (rows) by position
########################################################

EU_nobenelux <- slice(EU, -1, -16, -19)
# This creates "EU_nobenelux" by taking the "EU" data and removing rows 1, 16, and 19.
# All other rows are kept; this is a tidyverse-style row deletion.

EU_benelux <- slice(EU, 1, 16, 19)
# This creates "EU_benelux" by keeping only rows 1, 16, and 19 from "EU" and dropping
# all other rows (tidyverse-style row selection).


########################################################
# Subsetting observations based on a condition
########################################################

EU_pop_large <- filter(EU, pop18 > 10000000)
# This keeps only those rows from "EU" where the variable "pop18" (population in 2018)
# is greater than 10,000,000, and stores them in a new data frame "EU_pop_large".


########################################################
# Operators table (used for documentation in the worksheet)
########################################################

geometry <- read.csv("files/Week 5/operators.csv")
# This reads the CSV file "operators.csv" from the specified folder into an object
# called "geometry" as a data frame.

knitr::kable(
  geometry,
  format = "pipe",
  escape = FALSE
)
# This formats the "geometry" data frame as a markdown table (pipe format),
# suitable for inclusion in a Quarto/knitr document, and allows LaTeX-style expressions
# by setting escape = FALSE.


############################
# Ordering (sorting) data
############################

EU_subset <- select(EU, country, pop18, access)
# This creates a new data frame "EU_subset" containing only the variables "country",
# "pop18", and "access" from "EU", dropping all other columns.

eu_order <- arrange(EU_subset, pop18)
# This sorts "EU_subset" by "pop18" in ascending order (from smallest to largest),
# and stores the ordered result in a new data frame "eu_order".

eu_order[1:10, ]
# This prints only the first 10 rows of "eu_order" to the console, allowing you to
# view the 10 countries with the smallest population (after sorting).

eu_order <- arrange(EU_subset, desc(pop18))
# This sorts "EU_subset" by "pop18" in descending order (from largest to smallest)
# and stores the result in "eu_order".

eu_order[1:10, ]
# This prints the first 10 rows of the descendingly sorted "eu_order", showing the
# 10 countries with the largest populations.


########################################################
# Ordering by multiple variables
########################################################

eu_order <- arrange(EU_subset, access, pop18)
# This sorts "EU_subset" first by "access" (accession year) in ascending order, and
# within each accession year, by "pop18" in ascending order, then stores the result
# in "eu_order".

eu_order[1:10, ]
# This prints the first 10 rows of "eu_order" after the two-level sorting so you can
# inspect the ordering by accession year and population.


########################################################
# Grouping data and calculating group summaries
########################################################

eu_access <- group_by(EU_subset, access)
# This groups the rows of "EU_subset" by the variable "access", creating a grouped
# data frame "eu_access" where rows with the same accession year are treated as a group
# for further summarising operations.

ungroup(EU_subset)
# This removes any grouping structure from "EU_subset" (if present), returning it to
# a regular, ungrouped data frame. It is good practice to ungroup once grouped
# calculations are finished.

eu_popaccess <- EU_subset %>%
  group_by(access) %>%
  summarise(avg = mean(pop18))
# This pipeline creates a summary data frame "eu_popaccess":
# 1) EU_subset %>% passes the subset data into group_by(access), forming groups by accession year.
# 2) summarise(avg = mean(pop18)) calculates, for each group, the mean of "pop18" and
#    stores it in a new variable "avg".
# 3) The result is a data frame with one row per accession year and a column "avg"
#    showing the average population of countries in that accession wave.

eu_popaccess
# This prints the "eu_popaccess" summary table to the console so you can see the
# average population for each accession year group.


########################################################
# Combining grouping and ordering
########################################################

eu_popaccess_order <- arrange(eu_popaccess, desc(avg))
# This sorts the summary data frame "eu_popaccess" in descending order of "avg"
# so that accession years with the highest average population appear first, and
# stores the result in "eu_popaccess_order".

eu_popaccess_order
# This prints the ordered summary "eu_popaccess_order" so you can quickly see which
# accession wave had the largest average population.
