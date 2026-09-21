#########################################################################
# PO11Q - Quantitative Political Analysis: From Measurement to Inference
# Dr Flo Linke
# WEEK 8
#########################################################################

rm(list=ls())

setwd()

# *****************************************************************
# SETUP AND PACKAGES
# *****************************************************************
library(tidyverse)
library(haven)

# *****************************************************************
# GRAPH FORMATTING
# *****************************************************************

# Graph theme
theme_iqmss <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t=9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 24),
        axis.ticks.length=unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA)
  )
# Graph theme (with math notation)
theme_iqmss_math <- theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(margin = margin(b = 10, t=9)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 22),
        axis.ticks.length=unit(.1, "cm")) +
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.box.background = element_rect(fill = 'transparent', color = NA)
  )


# *****************************************************************
# LOAD DATA
# *****************************************************************

# The data are supplied as an SPSS file. The haven package reads .sav
# files. SPSS variables are often imported as labelled variables, so
# we use as.numeric() when we need ordinary numbers for calculations.

afro <- read_sav(file.path(RAW, "Afrobarometer.sav"))

# Look at the data before beginning.
head(afro)
nrow(afro)
ncol(afro)

# *****************************************************************
# EXERCISE 1
# *****************************************************************

# Q4A records the present economic condition of the country:
# 1 = Very bad
# 2 = Fairly bad
# 3 = Neither good nor bad
# 4 = Fairly good
# 5 = Very good
#
# Values outside 1-5 are missing or non-substantive responses.

# --- (a) Filter the data for Kenya -------------------------------

kenya <- afro |>
  filter(COUNTRY == 16)

kenya <- kenya |>
  mutate(Q4A = as.numeric(Q4A))

kenya <- kenya |>
  filter(Q4A >= 1 & Q4A <= 5)

head(kenya)
table(kenya$Q4A)

# --- (b) Recode to factor variable ------------------------------------------

# Give the five response categories readable labels.
q4a_labels <- c(
  "Very bad", "Fairly bad", "Neither",
  "Fairly good", "Very good"
)

# Convert Q4A into a factor with all five categories.
# .drop = FALSE tells count() to keep categories with zero responses.
kenya <- kenya |>
  mutate(
    Q4A = factor(
      Q4A,
      levels = 1:5,
      labels = q4a_labels
    )
  )

# --- (c) Frequency table ------------------------------------------

# i. Count the responses and calculate the relative frequency.
freq_kenya <- kenya |>
  count(Q4A, .drop = FALSE)

freq_kenya

# ii. Calculate the total number of valid responses.
total_kenya <- sum(freq_kenya$n)
total_kenya

# iii. Calculate the probability of each response category.
freq_kenya <- freq_kenya |>
  mutate(probability = n / total_kenya)

freq_kenya

# iv. Check that the probabilities add up to 1.
sum(freq_kenya$probability)

# --- (d) Bar chart -------------------------------------------------

kenya_plot <- ggplot(
  freq_kenya,
  aes(x = Q4A, y = probability)
) +
  geom_col(fill = "#e57726", width = 0.6) +
  labs(
    x = "Economic Condition",
    y = "Probability",
    title = "Economic Condition of the Country (Kenya)"
  ) +
  theme_iqmss

kenya_plot

# Very bad is the talles bar, but if you want to find this with R:
highest_kenya_probability <- max(freq_kenya$probability)
highest_kenya_probability

# Find the category or categories with that probability.
highest_kenya_category <- freq_kenya |>
  filter(probability == highest_kenya_probability)

highest_kenya_category

# --- (e) Very bad OR Fairly bad -------------------------------

# add the relative frequencies together from 

freq_kenya
0.627 + 0.238 

# Or if you want to use R: Select the two relevant rows.
bad_categories <- freq_kenya |>
  filter(Q4A %in% c("Very bad", "Fairly bad"))

bad_categories

# Add their probabilities together.
sum(bad_categories$probability)


# --- (f) Repeat the frequency table for Nigeria -------------------

nigeria <- afro |>
  filter(COUNTRY == 28)

nigeria <- nigeria |>
  mutate(Q4A = as.numeric(Q4A))

nigeria <- nigeria |>
  filter(Q4A >= 1 & Q4A <= 5)

# Use the same labels and factor levels as for Kenya.
nigeria <- nigeria |>
  mutate(
    Q4A = factor(
      Q4A,
      levels = 1:5,
      labels = q4a_labels
    )
  )

freq_nigeria <- nigeria |>
  count(Q4A, .drop = FALSE)

freq_nigeria

total_nigeria <- sum(freq_nigeria$n)
total_nigeria

freq_nigeria <- freq_nigeria |>
  mutate(probability = n / total_nigeria)

freq_nigeria
sum(freq_nigeria$probability)

# Compare the two probability distributions.
# Either call these separately
freq_nigeria
freq_kenya

# Or join these together for a more elegant solution
comparison_q4a <- freq_kenya |>
  select(Q4A, probability_kenya = probability) |>
  left_join(
    freq_nigeria |>
      select(Q4A, probability_nigeria = probability),
    by = "Q4A"
  )

comparison_q4a

# Even though these are probability distributions for two different countries, they are very similar. 


# *****************************************************************
# EXERCISE 2
# *****************************************************************

# Q1 records respondent age. For this exercise, age is treated as a
# continuous variable.

# --- (a) Select valid ages -----------------------------------------

age <- afro |>
  mutate(Q1 = as.numeric(Q1))

# Valid ages are between 18 and 112. Other values are removed.
age <- age |>
  filter(Q1 >= 18 & Q1 <= 112)

head(age)
summary(age$Q1)

# --- (b) Mean and standard deviation -------------------------------

mu_age <- mean(age$Q1)
sd_age <- sd(age$Q1)

mu_age
sd_age

# --- (c) Histogram and density curve -------------------------------

ggplot(age, aes(x = Q1)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    colour = "#e57726",
    fill = "#e57726",
    alpha = 0.7
  ) +
  geom_density(colour = "#8a1e00", linewidth = 0.8) +
  labs(
    x = "Age",
    y = "Density",
    title = "Distribution of Respondent Age (All Countries)"
  ) +
  theme_iqmss


# The distribution is heavily right-skewed as a large proportion of the population is young, and there are only few very old people.

# --- (d) Probability of age between 25 and 45 ----------------------

# pnorm() gives the area to the LEFT of a value.
prob_age_at_most_45 <- pnorm(
  45,
  mean = mu_age,
  sd = sd_age
)

prob_age_at_most_25 <- pnorm(
  25,
  mean = mu_age,
  sd = sd_age
)

prob_age_at_most_45
prob_age_at_most_25

# Subtract the two areas to get the area between 25 and 45.
p_25_45 <- prob_age_at_most_45 - prob_age_at_most_25
p_25_45

# The probability of finding a respondent between 25 and 45 years old is 48.59%.


# *****************************************************************
# EXERCISE 3
# *****************************************************************

# We continue using the mean and standard deviation of age.

# --- (a) Probability of being older than 60 ------------------------

prob_age_at_most_60 <- pnorm(
  60,
  mean = mu_age,
  sd = sd_age
)

prob_age_at_most_60

# The probability above 60 is the area remaining after the area below 60.
p_gt_60 <- 1 - prob_age_at_most_60
p_gt_60

# This probability is small, at 7.2%.

# --- (b) Confirm using a z-score -----------------------------------

z_60 <- (60 - mu_age) / sd_age
z_60

# The z-table gives a right-tail probability of 0.0721 which is slightly higher than the value we obtained from R, because R uses the precise values. The values in the table are rounded. 

# --- (c) Younger than 25 -------------------------------------------

p_lt_25 <- pnorm(
  25,
  mean = mu_age,
  sd = sd_age
)

p_lt_25

# The probability that a randomly selected respondent is younger than 25 is 19.27%.

# --- (d) Between 30 and 50 ----------------------------------------

prob_age_at_most_50 <- pnorm(
  50,
  mean = mu_age,
  sd = sd_age
)

prob_age_at_most_30 <- pnorm(
  30,
  mean = mu_age,
  sd = sd_age
)

prob_age_at_most_50
prob_age_at_most_30

p_30_50 <- prob_age_at_most_50 - prob_age_at_most_30
p_30_50

# The probability that a randomly selected respondent is between 30 and 50 years old is 49.09%. 

# --- (e) Oldest 5% -------------------------------------------------

# The oldest 5% begin at the 95th percentile.
age_95 <- qnorm(
  0.95,
  mean = mu_age,
  sd = sd_age
)

age_95

# The oldest 5% of the population are older than 62.74 years.


# *****************************************************************
# EXERCISE 4
# *****************************************************************

# Q4B records respondents' own present living conditions:
# 1 = Very bad and 5 = Very good.
# We use South Africa, COUNTRY == 33.

# --- (a) South African Q4B data -----------------------------------

sa <- afro |>
  filter(COUNTRY == 33)

sa <- sa |>
  mutate(Q4B = as.numeric(Q4B))

sa <- sa |>
  filter(Q4B >= 1 & Q4B <= 5)

mu_sa <- mean(sa$Q4B)
sd_sa <- sd(sa$Q4B)

mu_sa
sd_sa

# --- (b) z-score for a rating of 5 -------------------------------

rating_5 <- 5

z_5 <- (rating_5 - mu_sa) / sd_sa
z_5

# The z-score means that the rating of 5 is 1.58 standard deviations above the mean of South Africa.


# --- (c) z-score for a rating of 1 -------------------------------

rating_1 <- 1

z_1 <- (rating_1 - mu_sa) / sd_sa
z_1

# The z-score means that the rating of 1 is 1.27 standard deviations below the mean of South Africa.

# --- (d) Right-tail probabilities ---------------------------------

# pnorm() with a z-score uses the standard normal distribution.
prob_below_5 <- pnorm(z_5)
prob_below_5

prob_above_5 <- 1 - prob_below_5
prob_above_5

# Only 5.7% of South Africans enjoy very good living conditions.  

prob_below_1 <- pnorm(z_1)
prob_below_1

prob_above_1 <- 1 - prob_below_1
prob_above_1

# 89.83% of South Africans have better than "very bad" living conditions.  

# *****************************************************************
# EXERCISE 5
# *****************************************************************

# Q6E records how often respondents have gone without a cash income:
# 0 = Never, 1 = Just once or twice, 2 = Several times,
# 3 = Many times, 4 = Always.
# We use Ghana, COUNTRY == 14.

# --- (a) Ghana Q6E data --------------------------------------------

ghana <- afro |>
  filter(COUNTRY == 14)

ghana <- ghana |>
  mutate(Q6E = as.numeric(Q6E))

ghana <- ghana |>
  filter(Q6E >= 0 & Q6E <= 4)

mu_ghana <- mean(ghana$Q6E)
sd_ghana <- sd(ghana$Q6E)

mu_ghana
sd_ghana

# --- (b) z-score for a value of 3 -------------------------------

value_3 <- 3

z_3 <- (value_3 - mu_ghana) / sd_ghana
z_3

# --- (c) Probability of a score higher than 3 --------------------

prob_at_most_3 <- pnorm(z_3)
prob_at_most_3

p_gt_3 <- 1 - prob_at_most_3
p_gt_3

# The probability that a randomly selected Ghanaian respondent has a 
# score higher than 3 on this variable is 16.75%.


# --- (d) Probability of a score between 1 and 3 ------------------

value_1 <- 1

z_1_ghana <- (value_1 - mu_ghana) / sd_ghana
z_1_ghana

prob_at_most_z3 <- pnorm(z_3)
prob_at_most_z1 <- pnorm(z_1_ghana)

prob_at_most_z3
prob_at_most_z1

p_1_to_3 <- prob_at_most_z3 - prob_at_most_z1
p_1_to_3

# The probability that a randomly selected Ghanaian respondent has a 
# score between 1 and 3 on this variable is 56.81%.

# --- (e) Histogram -----------------------------------------------

ggplot(ghana, aes(x = Q6E)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 5,
    colour = "#e57726",
    fill = "#e57726",
    alpha = 0.7,
    boundary = -0.5
  ) +
  labs(
    x = "Gone without cash income",
    y = "Density",
    title = "Distribution of Q6E (Ghana)"
  ) +
  theme_iqmss

# Q6E is discrete and has only five possible values. A normal model is
# therefore not a particularly reasonable description of this variable.
# A discrete probability distribution as in Exercise 1 would be more 
# appropriate here.


# *****************************************************************
# EXERCISE 6
# *****************************************************************

# --- (a) Calculate the country-level means ------------------------

q4b_valid <- afro |>
  mutate(Q4B = as.numeric(Q4B))

q4b_valid <- q4b_valid |>
  filter(Q4B >= 1 & Q4B <= 5)

country_means <- q4b_valid |>
  group_by(COUNTRY)

country_means <- country_means |>
  summarise(
    mean_Q4B = mean(Q4B),
    .groups = "drop"
  )

country_means

# --- (b) Number of observations -----------------------------------

number_of_countries <- nrow(country_means)
number_of_countries

# --- (c) Mean and standard deviation ------------------------------

mu_pop <- mean(country_means$mean_Q4B)
mu_pop

# sd() uses the sample formula and divides by N - 1.
sd_country_means_sample_formula <- sd(country_means$mean_Q4B)
sd_country_means_sample_formula

# Here we treat the 39 countries as the complete population. Therefore,
# calculate sigma using N in the denominator.

deviations_from_mean <- country_means$mean_Q4B - mu_pop
squared_deviations <- deviations_from_mean^2
sum_squared_deviations <- sum(squared_deviations)

sig_pop <- sqrt(
  sum_squared_deviations / number_of_countries
)

sig_pop

# --- (d) Plot the country means ------------------------------------

ggplot(country_means, aes(x = mean_Q4B)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 10,
    colour = "#e57726",
    fill = "#e57726",
    alpha = 0.7
  ) +
  geom_density(colour = "#8a1e00", linewidth = 0.8) +
  labs(
    x = "Mean Living Conditions Score",
    y = "Density",
    title = "Distribution of Country-Level Means (Q4B)"
  ) +
  theme_iqmss

# With a little goodwill, this can be described as approximately normal. 
# There is a notable right-skew, however. 

# --- (e) z-score and probability above 3.5 ------------------------

hypothetical_mean <- 3.5

z_35 <- (hypothetical_mean - mu_pop) / sig_pop
z_35

prob_country_at_most_35 <- pnorm(
  hypothetical_mean,
  mean = mu_pop,
  sd = sig_pop
)

prob_country_at_most_35

p_gt_35 <- 1 - prob_country_at_most_35
p_gt_35

# The probability of observing such a country is very small at only 0.6%.

# *****************************************************************
# EXERCISE 7
# *****************************************************************

# --- (a) Population parameters ------------------------------------

# You should still have these results from Exercise 2b. If not, then this is the code:

age <- afro |>
  mutate(Q1 = as.numeric(Q1))

# Valid ages are between 18 and 112. Other values are removed.
age <- age |>
  filter(Q1 >= 18 & Q1 <= 112)

mu_age <- mean(age$Q1)
sd_age <- sd(age$Q1)

mu_age
sd_age


# --- (b) 5,000 samples of size 50 -------------------------------

set.seed(42)

n_sims <- 5000
n_size <- 50

# Create an empty vector to store the sample means.
sample_means_50 <- numeric(n_sims)

# Repeat the following steps 5,000 times:
# 1. Draw a random sample of 50 ages.
# 2. Calculate its mean.
# 3. Store the mean in sample_means_50.

for (i in 1:n_sims) {      #1.
  one_sample <- sample(
    pop_age,
    size = n_size,
    replace = TRUE
  )
  one_sample_mean <- mean(one_sample)     #2.
  sample_means_50[i] <- one_sample_mean   #3.
}

head(sample_means_50)
length(sample_means_50) # this needs to be 5000, as we drew 5000 samples

# --- (c) Plot the sample means ------------------------------------

# Store the sample means in their own data frame for ggplot
sample_means_50_data <- data.frame(
  sample_mean = sample_means_50
)

ggplot(
  sample_means_50_data,
  aes(x = sample_mean)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    colour = "#e57726",
    fill = "#e57726",
    alpha = 0.7
  ) +
  geom_density(colour = "#8a1e00", linewidth = 0.8) +
  labs(
    x = "Sample Mean Age",
    y = "Density",
    title = "Sampling Distribution of the Mean (n = 50)"
  ) +
  theme_iqmss

# This is a normal distribution, as per the central limit theorem, the sample
# size of each sample exceeds 30


# --- (d) Mean and standard deviation ------------------------------

mean_sampling_distribution_50 <- mean(sample_means_50)
sd_sampling_distribution_50 <- sd(sample_means_50)

mean_sampling_distribution_50
mu_age

# These are very close to one another, but not identical, this would only happen 
# if we drew an infinite number of samples.

# The theoretical standard error is sigma divided by the square root
# of the sample size.
se_theoretical_50 <- sd_age / sqrt(n_size)

se_theoretical_50
sd_sampling_distribution_50

# Again, these are very close to one another, but not identical, this would only 
# happen if we drew an infinite number of samples.


# --- (e) Repeat for n = 10 and n = 200 ----------------------------

set.seed(42)

sample_means_10 <- numeric(n_sims)

for (i in 1:n_sims) {
  one_sample <- sample(
    pop_age,
    size = 10,
    replace = TRUE
  )
  sample_means_10[i] <- mean(one_sample)
}

set.seed(42)

sample_means_200 <- numeric(n_sims)

for (i in 1:n_sims) {
  one_sample <- sample(
    pop_age,
    size = 200,
    replace = TRUE
  )
  sample_means_200[i] <- mean(one_sample)
}

sd_10 <- sd(sample_means_10)
sd_50 <- sd(sample_means_50)
sd_200 <- sd(sample_means_200)

sd_10
sd_50
sd_200

se_10_theoretical <- sd_age / sqrt(10)
se_50_theoretical <- sd_age / sqrt(50)
se_200_theoretical <- sd_age / sqrt(200)

se_10_theoretical
se_50_theoretical
se_200_theoretical

# The spread becomes smaller as the sample size increases.
# This reflects the decreasing uncertainty, as the sample size increases.


# *****************************************************************
# EXERCISE 8
# *****************************************************************

# --- (a) Sample nine countries ------------------------------------

set.seed(123)

n <- 9

samp <- country_means |>
  slice_sample(n = n)

samp

# pull the country values into a vector for analysis
sample_values <- samp$mean_Q4B
sample_values

y_bar <- mean(sample_values)
s <- sd(sample_values)

y_bar
s

# --- (b) Exact standard error -------------------------------------

se_exact <- sig_pop / sqrt(n)
se_exact

# --- (c) Estimated standard error ---------------------------------

se_est <- s / sqrt(n)
se_est

# --- (d) Compare ---------------------------------------------------

# You could just call these separately:
se_exact
se_est

# or if you want this super-elegant:
se_comparison <- tibble(
  type = c("Exact", "Estimated"),
  standard_error = c(se_exact, se_est)
)

se_comparison

# The values differ because s is calculated from only nine countries.
# In practice, sigma is usually unknown, so we use the estimated se.

# *****************************************************************
# EXERCISE 9
# *****************************************************************

# --- (a) Degrees of freedom ---------------------------------------

df <- n - 1
df

# 8 degrees of freedom

# --- (b) Critical t-value -----------------------------------------

t_crit <- qt(0.975, df = df)
t_crit

# --- (c) Compare with z = 1.96 -----------------------------------

z_crit <- qnorm(0.975)
z_crit

difference_t_z <- t_crit - z_crit
difference_t_z

# The t-value is larger because the t-distribution has heavier tails.
# This takes into account the uncertainty arising from the small sample size.

# --- (d) n = 35 ----------------------------------------------------

df_35 <- 35 - 1
df_35

t_crit_35 <- qt(0.975, df = df_35)
t_crit_35

# This value is closer to 1.96 than the result of part c), because the sample
# size is larger and the uncertainty lower.

# --- (e) Plot normal and t distributions --------------------------

x <- seq(-4, 4, length.out = 300)

normal_density <- dnorm(x)
t_density <- dt(x, df = df)

plot(
  x,
  normal_density,
  type = "l",
  lwd = 2,
  ylab = "Density",
  xlab = "x",
  main = "Normal Distribution vs t-Distribution"
)

lines(x, t_density, col = "#e57726", lwd = 2)

legend(
  "topright",
  legend = c("Normal", "t (df = 8)"),
  col = c("black", "#e57726"),
  lwd = 2
)

# The t-distribution is flatter and has heavier tails, just as 
# anticipated on the basis of the preceeding exercises. 


# *****************************************************************
# EXERCISE 10
# *****************************************************************

# --- (a) Normal interval with known sigma -------------------------

margin_of_error_normal <- z_crit * se_exact
margin_of_error_normal

ci_normal_lower <- y_bar - margin_of_error_normal
ci_normal_upper <- y_bar + margin_of_error_normal

ci_normal_lower
ci_normal_upper

# --- (b) t interval with estimated sigma --------------------------

margin_of_error_t <- t_crit * se_est
margin_of_error_t

ci_t_lower <- y_bar - margin_of_error_t
ci_t_upper <- y_bar + margin_of_error_t

ci_t_lower
ci_t_upper

# --- (c) Comparison table ------------------------------------------

normal_width <- ci_normal_upper - ci_normal_lower
t_width <- ci_t_upper - ci_t_lower

ci_table <- tibble(
  distribution = c("Normal", "t"),
  lower = c(ci_normal_lower, ci_t_lower),
  upper = c(ci_normal_upper, ci_t_upper),
  width = c(normal_width, t_width)
)

ci_table

# The confidence interval using the estimated standard error is wider,
# reflecting the uncertainty arising from using the sample standard 
# deviation to estimate the true sigma.

# --- (d) Does the population mean fall inside? --------------------

mu_pop

mu_in_normal_ci <- mu_pop >= ci_normal_lower &
  mu_pop <= ci_normal_upper

mu_in_t_ci <- mu_pop >= ci_t_lower &
  mu_pop <= ci_t_upper

mu_in_normal_ci
mu_in_t_ci

# In this case, mu is contained in both confidence intervals.
# But remember that a particular 95% interval may or may not contain mu. 
# The 95% refers to the long-run performance of the interval-producing procedure.

# *****************************************************************
# EXERCISE 11
# *****************************************************************

# (a) The statement is incorrect, because the population mean is 
# treated as fixed. After the interval is calculated, it either 
# is inside or outside it.

# (b) Correct interpretation:
# If we repeated the same sampling process many times and calculated a
# 95% confidence interval each time, approximately 95% of those intervals
# would contain the true population mean.

# (c) A 99% interval would be wider because it uses a larger critical
# value and therefore has a larger margin of error.

# (d) Increasing n from 25 to 39 would generally make the interval
# narrower because the standard error decreases as n increases.

#
# EOF
#
