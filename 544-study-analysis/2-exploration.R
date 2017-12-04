library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
library(reshape2)
library(knitr)
library(xtable)
library(lubridate)

dat <- read.csv("data/data-ready.text", sep = "|", check.names = F)

# Split quant. data into groups for time, satisfaction, and preference
dat.time <- dat[c("participant-number", "house-type", "t1", "t2", "t3", "t4", "t5")]
names(dat.time) <- c("p", "h", "shelf", "list", "fb", "m_return", "fb_return")
dat.satisfaction <- dat[c("participant-number", "t1q", "t2q", "t3q", "t4q", "t5q")]
names(dat.satisfaction) <- c("p", "shelf", "list", "fb", "m_return", "fb_return")
dat.preference <- dat[c("participant-number", "q1", "q2", "q3", "q4")]
names(dat.preference) <- c("p", "machine_usefulness", "fb_usefulness", "shelf_vs_list", "app_vs_fb")

# Plots for most important measures
ggplot(melt(dat.time[2:5]),aes(x=variable,y=value, color=h)) +
  geom_point() +
  stat_summary(aes(y = value,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  theme_classic()

ggplot(melt(dat.time[5:6]),aes(x=variable,y=value, color=variable)) +
  geom_point() +
  theme_classic()

ggplot(melt(dat.satisfaction[2:6]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  theme_classic()

ggplot(melt(dat.preference[2:3]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  theme_classic()


# summary stats
mean(dat.satisfaction$shelf)
mean(dat.satisfaction$list)
mean(dat.satisfaction$fb)
mean(dat.satisfaction$fb_return)
mean(dat.satisfaction$m_return)

median(dat.satisfaction$shelf)
median(dat.satisfaction$list)
median(dat.satisfaction$fb)
median(dat.satisfaction$fb_return)
median(dat.satisfaction$m_return)

range(dat.satisfaction$shelf)
range(dat.satisfaction$list)
range(dat.satisfaction$fb)
range(dat.satisfaction$fb_return)
range(dat.satisfaction$m_return)

# Some tests
t.test(dat.time$shelf, dat.time$list)

# chisq.test(dat.satisfaction$fb, dat.satisfaction$list, correct = FALSE)

# wilcox.test(dat.satisfaction$list, dat.satisfaction$fb)

# Prepare for ANOVA
dat.aov <- melt(dat.time[2:5])

fit <- lm(formula = value ~ variable * h, data = dat.aov)
anova(fit)

aov.fit <- aov(formula = value ~ variable * h, data = dat.aov)
summary(aov.fit)
TukeyHSD(aov.fit, "variable", ordered = TRUE)
# Significant effect of interface, but no interaction effect of housing type and interface

# Prepare data to run friedman test of satisfaction
dat.fr <- as.matrix(dat.satisfaction[2:4])
friedman.test(dat.fr)

wilcox.test(dat.satisfaction$list, dat.satisfaction$fb)
wilcox.test(dat.satisfaction$shelf, dat.satisfaction$fb)
wilcox.test(dat.satisfaction$list, dat.satisfaction$shelf)

wilcox.test(dat.preference$machine_usefulness, dat.preference$fb_usefulness)
