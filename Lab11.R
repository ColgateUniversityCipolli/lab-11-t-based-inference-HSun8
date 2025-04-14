# Henry Sun 
# Lab 11
library(tidyverse)
################################################################################
# Task 1
library(pwr)
alpha <- 0.05
pwr.analysis <- pwr.t.test(d=0.65, sig.level = alpha, power = 0.80, type = "one.sample", 
           alternative = "two.sided")
num.obs <- pwr.analysis$n

################################################################################
# Task 2
# load and clean the data
# how to collect data 
# go to paper 
# click on source data figure 2 
# download and open excel file
# save desired data as a .csv file
# load in
library(xtable)

# closer vals
closer.data <- read_csv("closer_vals.csv")
# farther vals
farther.data <- read_csv("farther_vals.csv")

# consolidate all data
figG.data <- tibble(closer_vals = closer.data$closer_vals,
                    farther_vals = farther.data$farther_vals) |>
  mutate(val_diffs = closer_vals - farther_vals)
# create table for sweave
xtable(figG.data)
################################################################################
# Task 3
# Summarize data
library(e1071)
# part A
closer.summary <- summarize(figG.data,
                            mean = mean(closer.vals),
                            variance = var(closer.vals),
                            median = median(closer.vals),
                            IQR = IQR(closer.vals),
                            skewness = skewness(closer.vals),
                            e.kurtosis = kurtosis(closer.vals))
xtable(closer.summary)

# part B
farther.summary <- summarize(figG.data,
                            mean = mean(farther.vals),
                            variance = var(farther.vals),
                            median = median(farther.vals),
                            IQR = IQR(farther.vals),
                            skewness = skewness(farther.vals),
                            e.kurtosis = kurtosis(farther.vals))
xtable(farther.summary)
# part C
diffs.summary <- closer.summary - farther.summary
xtable(diffs.summary)

# plot
ggplot(figG.data)+
  geom_boxplot(aes(x="Closer", y=closer.vals))+
  geom_boxplot(aes(x="Farther", y=farther.vals))+
  geom_boxplot(aes(x="Difference", y=val.diffs))+
  ylab("delta F/F (%)")

################################################################################
# Task 4
# differ from 0

# closer
# t statistic
mu0 <- 0
x <- figG.data$closer.vals
(xbar <- mean(x))

# hedge's g + CI
library(effectsize)
hedges_g(x = x, mu = mu0, alternative = "greater")
interpret_hedges_g(1.61)

# p value 
#(p.val <- pt(q=t.stat, df = n-1))

# t.test
t.test(x=x, mu = mu0, alternative = "greater")


# farther
# t statistic
mu0 <- 0
x <- figG.data$farther.vals

# hedge's g + CI
library(effectsize)
hedges_g(x = x, mu = mu0, alternative = "greater")
interpret_hedges_g(-1.51)

# t.test
t.test(x=x, mu = mu0, alternative = "greater")

# differences
# t statistic
mu0 <- 0
x <- figG.data$val.diffs

# hedge's g + CI
hedges_g(x = x, mu = mu0, alternative = "two.sided")
interpret_hedges_g(1.65)

# t.test
t.test(x=x, mu = mu0, alternative = "two.sided")

# QUESTION: When to use two tail, when to use one tail?
