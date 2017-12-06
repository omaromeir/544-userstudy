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
dat.satisfaction <- dat[c("participant-number", "t1q", "t2q", "t3q", "t4q", "t5q", "house-type")]
names(dat.satisfaction) <- c("p", "shelf", "list", "fb", "m_return", "fb_return", "h")
dat.preference <- dat[c("participant-number", "q1", "q2", "q3", "q4", "house-type")]
names(dat.preference) <- c("p", "machine_usefulness", "fb_usefulness", "shelf_vs_list", "app_vs_fb", "h")
dat.error <- dat[c("participant-number", "house-type")]
error <- data.frame(c(8245, 2589, 466, 5115, 9893, 1557, 46, 5511, 2757, 7994, 2105, 9500, 3682, 7019, 5665, 2747),
                    c(0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2), 
                    c(2, 2, 2, 0, 3, 0, 2, 0, 2, 1, 2, 2, 2, 2, 0, 0))
names(error) <- c("p", "t1e", "t2e")
names(dat.error) <- c("p", "h")
dat.er <- merge(dat.error, error)

mean(dat.er$t1e)
mean(dat.er$t2e)

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
bartlett.test(dat.time[2:5])
fligner.test(dat.time[2:5])
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

wilcox.test(dat.satisfaction$list, dat.satisfaction$fb, paired = T)
wilcox.test(dat.satisfaction$list, dat.satisfaction$fb, paired = T, alternative = "less")
wilcox.test(dat.satisfaction$shelf, dat.satisfaction$fb, paired = T)
wilcox.test(dat.satisfaction$list, dat.satisfaction$shelf, paired = T)

# Test usefulness, alternative hypothesis is machine usefulness is less (better) than fb usefulness
wilcox.test(dat.preference$machine_usefulness, dat.preference$fb_usefulness, paired = T)
wilcox.test(dat.preference$machine_usefulness, dat.preference$fb_usefulness, paired = T, alternative = "less")

# Error rate, not normally distributed, variance is not homogenuos
bartlett.test(dat.er[3:4])
fligner.test(dat.er[3:4])
wilcox.test(dat.er$t1e, dat.er$t2e, paired = T)
wilcox.test(dat.er$t1e, dat.er$t2e, paired = T, alternative = "less")

# Idea: run tests comparing student housing for shelf with private residence for shelf, 
# compare error, usefulness and satisfaction with wilcox
wilcox.test(subset(dat.satisfaction, h == "Student residence")$shelf, subset(dat.satisfaction, h == "Private house")$shelf)
wilcox.test(subset(dat.satisfaction, h == "Student residence")$list, subset(dat.satisfaction, h == "Private house")$list)
wilcox.test(subset(dat.satisfaction, h == "Student residence")$fb, subset(dat.satisfaction, h == "Private house")$fb)

wilcox.test(subset(dat.preference, h == "Student residence")$machine_usefulness, subset(dat.preference, h == "Private house")$machine_usefulness)

wilcox.test(subset(dat.er, h == "Student residence")$t1e, subset(dat.er, h == "Private house")$t1e)

# No main effect of home type on error, usefulness or satisfaction