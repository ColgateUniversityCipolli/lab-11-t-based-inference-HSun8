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
# further vals
further.data <- read_csv("further_vals.csv")

# consolidate all data
figG.data <- tibble(closer_vals = closer.data$closer_vals,
                    further_vals = further.data$further_vals) |>
  mutate(val_diffs = closer_vals - further_vals)
# create table for sweave
xtable(figG.data)
################################################################################
# Task 3
# Summarize data
library(e1071)
# part A
closer.summary <- summarize(figG.data,
                            mean = mean(closer_vals),
                            variance = var(closer_vals),
                            median = median(closer_vals),
                            IQR = IQR(closer_vals),
                            skewness = skewness(closer_vals),
                            e.kurtosis = kurtosis(closer_vals))
xtable(closer.summary)

# part B
further.summary <- summarize(figG.data,
                            mean = mean(further_vals),
                            variance = var(further_vals),
                            median = median(further_vals),
                            IQR = IQR(further_vals),
                            skewness = skewness(further_vals),
                            e.kurtosis = kurtosis(further_vals))
xtable(further.summary)
# part C
diffs.summary <- closer.summary - further.summary
xtable(diffs.summary)

# plot
ggplot(figG.data)+
  geom_boxplot(aes(x="Closer", y=closer_vals))+
  geom_boxplot(aes(x="Further", y=further_vals))+
  geom_boxplot(aes(x="Difference", y=val_diffs))+
  ylab("delta F/F (%)")

################################################################################
# Task 4

library(effectsize)
# part A (closer)
mu0 <- 0
x.closer <- figG.data$closer_vals

# hedge's g + CI
hedges_g(x = x.closer, mu = mu0, alternative = "greater")
interpret_hedges_g(1.61)

# t.test
t.test(x=x.closer, mu = mu0, alternative = "greater")


# part B (further)
mu0 <- 0
x.further <- figG.data$further_vals

# hedge's g + CI
hedges_g(x = x.further, mu = mu0, alternative = "less")
interpret_hedges_g(-1.51)

# t.test
t.test(x=x.further, mu = mu0, alternative = "less")

# part C (differences)
mu0 <- 0
x.diff <- figG.data$val_diffs

# hedge's g + CI
hedges_g(x = x.diff, mu = mu0, alternative = "two.sided")
interpret_hedges_g(1.65)

# t.test
t.test(x=x.diff, mu = mu0, alternative = "two.sided")

################################################################################
# Task 5
n <- 25
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))

# closer data
s.closer <- sd(x.closer)
xbar.closer <- mean(x.closer)
t.stat.closer <- (xbar.closer - mu0)/(s.closer/sqrt(n))

# For plotting the observed point
ggdat.obs.closer <- tibble(t    = t.stat.closer, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x.closer,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks.closer <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.stat.closer)                  # t-statistic observed
xbar.breaks.closer <- t.breaks.closer * s.closer/(sqrt(n)) + mu0

# Create Plot
t5a <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  # geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs.closer, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks.closer,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks.closer,
                                         labels = round(xbar.breaks.closer,2)))+
  ylab("Density")+
  ggtitle("T-Test for Dopamine Change in a Closer Response for Young Zebra Finches",
          subtitle=bquote(H[0]==0*";"~H[a] > 0))


# further data
s.further <- sd(x.further)
xbar.further <- mean(x.further)
t.stat.further <- (xbar.further - mu0)/(s.further/sqrt(n))

# For plotting the observed point
ggdat.obs.further <- tibble(t    = t.stat.further, 
                           y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x.further,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks.further <- c(-5, qt(0.025, df = n-1), # rejection region (left)
                     0, 
                     qt(0.975, df = n-1), 5,  # rejection region (right)
                     t.stat.further)                  # t-statistic observed
xbar.breaks.further <- t.breaks.further * s.further/(sqrt(n)) + mu0

# Create Plot
t5b <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs.further, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks.further,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks.further,
                                         labels = round(xbar.breaks.further,2)))+
  ylab("Density")+
  ggtitle("T-Test for Dopamine Change in a Further Response for Young Zebra Finches",
          subtitle=bquote(H[0]==0*";"~H[a] < 0))


# diff data
s.diff <- sd(x.diff)
xbar.diff <- mean(x.diff)
t.stat.diff <- (xbar.diff - mu0)/(s.diff/sqrt(n))

# For plotting the observed point
ggdat.obs.diff <- tibble(t    = t.stat.diff, 
                            y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x.diff,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks.diff <- c(-5, qt(0.025, df = n-1), # rejection region (left)
                      0, 
                      qt(0.975, df = n-1), 5,  # rejection region (right)
                      t.stat.diff)                  # t-statistic observed
xbar.breaks.diff <- t.breaks.diff * s.diff/(sqrt(n)) + mu0

# Create Plot
t5c <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
               aes(x=t, ymin=0, ymax=pdf.null),
               fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs.diff, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks.diff,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks.diff,
                                         labels = round(xbar.breaks.diff,2)))+
  ylab("Density")+
  ggtitle("T-Test for Dopamine Change Between Populations (Close and Far Responses)",
          subtitle=bquote(H[0]==0*";"~H[a] != 0))

library(patchwork)
t5a + t5b + t5c
