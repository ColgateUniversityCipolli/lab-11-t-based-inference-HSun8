# Henry Sun 
# Lab 11
################################################################################
# Task 1
library(pwr)
suicide <- pwr.t.test(d=0.65, sig.level = 0.05, power = 0.80, type = "one.sample", 
           alternative = "two.sided")
num.obs <- suicide$n

################################################################################
# Task 2
# load and clean the data
# how to collect data 
# go to paper 
# click on source data figure 2 
# download and open excel file
# save desired data as a .csv file
# load in
closer.vals <- read_csv("closer_vals.csv")
farther.vals <- read_csv("farther_vals.csv")

# consolidate all data
figG.data <- tibble(closer.vals = closer.vals$closer_vals,
                    farther.vals = farther.vals$farther_vals) |>
  mutate(val.diffs = closer.vals - farther.vals)

################################################################################
# Task 3
# Summarize data
library(e1071)
# part A
closer.summary <- summarize(figG.data,
                            mean = mean(closer.vals),
                            variance = var(closer.vals),
                            skewness = skewness(closer.vals),
                            e.kurtosis = kurtosis(closer.vals))
# good enough for now
ggplot(figG.data)+
  geom_boxplot(aes(x="Closer", y=closer.vals))+
  geom_boxplot(aes(x="Farther", y=farther.vals))+
  geom_boxplot(aes(x="Difference", y=val.diffs))+
  ylab("delta F/F (%)")
  

# part B
farther.summary <- summarize(figG.data,
                            mean = mean(farther.vals),
                            variance = var(farther.vals),
                            skewness = skewness(farther.vals),
                            e.kurtosis = kurtosis(farther.vals))
# part C
diffs.summary <- closer.summary - farther.summary

# part D 

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
(p.val <- pt(q=t.stat, df = n-1))

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
