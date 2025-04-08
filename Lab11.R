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

