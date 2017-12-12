library(ggplot2)
library(ggthemes)
suppressPackageStartupMessages(library(dplyr))
library(stringr)
library(reshape2)
library(knitr)
library(xtable)
suppressWarnings(library(lubridate))

dat <- read.csv("data/data-ready.text", sep = "|", check.names = F)

#' Split quant. data into groups for time, satisfaction, and preference
dat.time <- dat[c("participant-number", "house-type", "t1", "t2", "t3", "t4", "t5")]
names(dat.time) <- c("p", "h", "shelf", "list", "fb", "m_return", "fb_return")
dat.satisfaction <- dat[c("participant-number", "t1q", "t2q", 
                          "t3q", "t4q", "t5q", "house-type")]
names(dat.satisfaction) <- c("p", "shelf", "list", "fb", "m_return", "fb_return", "h")
dat.preference <- dat[c("participant-number", "q1", "q2", "q3", "q4", "house-type")]
names(dat.preference) <- c("p", "machine_usefulness", 
                           "fb_usefulness", "shelf_vs_list", "app_vs_fb", "h")
dat.error <- dat[c("participant-number", "house-type")]

#' Create data.frame for Errors from observed data. t1e is task 1 errors, t2e is task 2 errors.
error <- data.frame(c(8245, 2589, 466, 5115, 9893, 1557, 46, 5511, 
                      2757, 7994, 2105, 9500, 3682, 7019, 5665, 2747),
                    c(0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2), 
                    c(2, 2, 2, 0, 3, 0, 2, 0, 2, 1, 2, 2, 2, 2, 0, 0))
names(error) <- c("p", "t1e", "t2e")
names(dat.error) <- c("p", "h")
dat.er <- merge(dat.error, error)

# Plots for most important measures
means.time <- melt(dat.time[2:5] %>% 
  group_by(h) %>% 
  summarise_all(mean))
means.time.sr <- subset(means.time, h == "Student residence")
means.time.pr <- subset(means.time, h == "Private house")

#' Time for borrow tasks with different interface type
ggplot(melt(dat.time[2:5]),aes(x=variable,y=value, color=h)) +
  geom_point() +
  stat_summary(data = means.time.sr, aes(y = value), colour="red", geom="line", group=1)+
  stat_summary(data = means.time.pr, aes(y = value), colour="blue", geom="line", group=2)+
  ylab("Time (seconds)") + xlab("Interface type")

ggsave("time-borrow.png", scale = 1, path = "plots/")

#' Time for return tasks
ggplot(melt(dat.time[6:7]),aes(x=variable,y=value, color=variable)) +
  geom_point() +
  stat_summary(aes(y = value), fun.y = mean, colour="blue", geom="line", group=1)+
  ylab("Time (seconds)") + xlab("Interface type")

ggsave("time-return.png", scale = 1, path = "plots/")

#' Satisfaction for borrow tasks
ggplot(melt(dat.satisfaction[2:4]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  ylab("Satisfaction (1-5)") + xlab("Interface type")

ggsave("sat-borrow.png", scale = 1, path = "plots/")

#' Satisfaction for return tasks (Facebook task depended heavily on how fast people type)
ggplot(melt(dat.satisfaction[5:6]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  ylab("Satisfaction (1-5)") + xlab("Interface type")

ggsave("sat-return.png", scale = 1, path = "plots/")

#' Usefulness rating
ggplot(melt(dat.preference[2:3]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  ylab("Usefulness (1-5)") + xlab("Interface type")

ggsave("usefulness.png", scale = 1, path = "plots/")

#' Summary stats
kable(dat.er[3:4] %>% 
  summarise_all(funs(mean, median, sd)))

xtable(melt(dat.er[3:4]) %>% 
         group_by(variable) %>% 
        summarise_all(funs(mean, median, sd)))

kable(dat.satisfaction[2:6] %>% 
  summarise_all(funs(mean)))
kable(dat.satisfaction[2:6] %>% 
        summarise_all(funs(median)))
kable(dat.satisfaction[2:6] %>% 
        summarise_all(funs(sd)))

kable(dat.time[3:7] %>% 
  summarise_all(funs(mean)))
kable(dat.time[3:7] %>% 
        summarise_all(funs(median)))
kable(dat.time[3:7] %>% 
        summarise_all(funs(sd)))

xtable(melt(dat.time[3:7]) %>% 
  group_by(variable) %>% 
  summarise_all(funs(mean, median, sd)))

kable(dat.preference[2:3] %>% 
        summarise_all(funs(mean, median)))


xtable(melt(dat.satisfaction[2:6]) %>% 
         group_by(variable) %>% 
         summarise_all(funs(mean, median, sd)))

xtable(melt(dat.preference[2:3]) %>% 
        group_by(variable) %>% 
        summarise_all(funs(mean, median, sd)))

#' To prepare time data before running mixed factorial ANOVA, 
#' run tests to determine if ANOVA assumptions hold. 
#' If data has homogenuos variance (Bartlett) and is normally distributed (Shapiro Wilk).
bartlett.test(dat.time[2:5])

#' Put data in the correct format
dat.aov <- melt(dat.time[2:5])
shapiro.test(dat.aov$value)

#' Plot quantile-quantile to see how far data deviates from normal. 
#' Normal assumption seems to hold so we can run our ANOVA.
qqnorm(subset(dat.aov, variable == "shelf")$value);qqline(subset(dat.aov, variable == "shelf")$value)
qqnorm(subset(dat.aov, variable == "list")$value);qqline(subset(dat.aov, variable == "list")$value)
qqnorm(subset(dat.aov, variable == "fb")$value);qqline(subset(dat.aov, variable == "fb")$value)

#' Fit the linear model (value ~ variable * h) value is the measurement (time), 
#' variable is the UI type, and h is the home type.
fit <- lm(formula = value ~ variable * h, data = dat.aov)
anova(fit)

#' This is the same as above but using the ANOVA native function of R
aov.fit <- aov(formula = value ~ variable * h, data = dat.aov)
summary(aov.fit)

#' Tukey Honest Significant Differences is a post-hoc test 
#' to determine the difference in means in terms of the UI type factor (variable).
TukeyHSD(aov.fit, "variable", ordered = TRUE)
#' Significant effect of interface, but no interaction effect of housing type and interface
#' Difference between FB and shelf and FB and list is significant.

dat.ret <- melt(dat.time[dat.time$p != 2589, 6:7])
t.test(subset(dat.ret, variable == "m_return")$value, subset(dat.ret, variable == "fb_return")$value, paired = T)

#' No significant effect of return task time!
#' 
#' Prepare data to run friedman test on satisfaction (3 factor)
dat.fr <- as.matrix(dat.satisfaction[2:4])
friedman.test(dat.fr)
#' Significant difference detected, to see what it is we conduct 
#' Wilcoxon tests with "less" hypotheses"

wilcox.test(dat.satisfaction$list, dat.satisfaction$fb, paired = T, alternative = "less")
wilcox.test(dat.satisfaction$shelf, dat.satisfaction$fb, paired = T, alternative = "less")
#' All FB tests show significant difference from FB. 

wilcox.test(dat.satisfaction$list, dat.satisfaction$shelf, paired = T, alternative = "less")
#' No significant difference between list and shelf though.

#' Test usefulness. Significant results. 
wilcox.test(dat.preference$machine_usefulness, dat.preference$fb_usefulness, paired = T)

#' Alternative hypothesis is machine usefulness is less (better) than fb usefulness
wilcox.test(dat.preference$machine_usefulness, dat.preference$fb_usefulness, paired = T, alternative = "less")

#' Error rate is simply counts so we assume it's not normally distributed
wilcox.test(dat.er$t1e, dat.er$t2e, paired = T)
wilcox.test(dat.er$t1e, dat.er$t2e, paired = T, alternative = "less")

#' Instead of testing for interaction with non-parametric tests, 
#' we run tests comparing student housing for shelf with private residence for shelf, 
#' compare error, usefulness and satisfaction with wilcox
wilcox.test(subset(dat.satisfaction, h == "Student residence")$shelf, subset(dat.satisfaction, h == "Private house")$shelf)
wilcox.test(subset(dat.satisfaction, h == "Student residence")$list, subset(dat.satisfaction, h == "Private house")$list)
wilcox.test(subset(dat.satisfaction, h == "Student residence")$fb, subset(dat.satisfaction, h == "Private house")$fb)

wilcox.test(subset(dat.preference, h == "Student residence")$machine_usefulness, subset(dat.preference, h == "Private house")$machine_usefulness)

wilcox.test(subset(dat.er, h == "Student residence")$t1e, subset(dat.er, h == "Private house")$t1e)

#' No main effect of home type on error, usefulness or satisfaction. 
#' No real need to test interaction.

#' When we asked people which one they prefer, the answers look like this:
kable(dat.preference[4:5])

kable(dat.preference[4:5] %>% 
        summarise_all(funs(mean, median)))

kable(melt(dat.preference[4:5]) %>% 
        group_by(variable) %>% 
        summarise_all(funs(mean, median, sd)))

#' There's no need to analyze this. A plot summarizes the results.
ggplot(melt(dat.preference[4:5]),aes(x=variable,y=value, color=variable)) +
  geom_boxplot() +
  ylab("Preference (1 Much worse to 5 Much better)") + xlab("Comparison")

ggsave("pref-summary.png", scale = 1, path = "plots/")

#' Barcharts with CIs
std.err <- function(x) sd(x)/sqrt(length(x))

timedat <- melt(dat.time[2:7]) %>% 
  group_by(variable, h) %>% 
  summarise_all(funs(mean, std.err))

ggplot(timedat,aes(x=variable,y=mean, fill=h)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Time") + xlab("Interface type")

ggsave("bar-time.png", scale = 1, path = "plots/")

satdat <- melt(dat.satisfaction[2:7]) %>% 
  group_by(variable, h) %>% 
  summarise_all(funs(mean, std.err))

ggplot(satdat,aes(x=variable,y=mean, fill=h)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Satisfaction (1-5)") + xlab("Interface type")

ggsave("bar-satisfaction.png", scale = 1, path = "plots/")

prefdat <- melt(dat.preference[c(2, 3, 6)]) %>% 
  group_by(variable, h) %>% 
  summarise_all(funs(mean, std.err))

ggplot(prefdat,aes(x=variable,y=mean, fill=h)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Usefulness (1-5)") + xlab("Interface type")

ggsave("bar-usefulness.png", scale = 1, path = "plots/")

errdat <- melt(dat.er[2:4]) %>% 
  group_by(variable, h) %>% 
  summarise_all(funs(mean, std.err))

ggplot(errdat,aes(x=variable,y=mean, fill=h)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Error count") + xlab("Interface type")

ggsave("bar-error.png", scale = 1, path = "plots/")

errdat1 <- melt(dat.er[3:4]) %>% 
  group_by(variable) %>% 
  summarise_all(funs(mean, std.err))

ggplot(errdat1,aes(x=variable,y=mean)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Error count") + xlab("Interface type")

ggsave("1bar-error.png", scale = 1, path = "plots/")

errpref <- melt(dat.preference[4:6]) %>% 
  group_by(variable, h) %>% 
  summarise_all(funs(mean, std.err))

ggplot(errpref,aes(x=variable,y=mean, fill=h)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=mean-std.err, ymax=mean+std.err), width=.2, position=position_dodge(.9)) +
  ylab("Preference (1 much worse to 5 much better)") + xlab("Interface type")

ggsave("pref-bar.png", scale = 1, path = "plots/")

#' Adjusted p-values for pairwise tests
#' The values are of the following tests: 1) Satisfaction list vs Facebook 
#' 2) satisfaction shelf vs Facebook 3) satisfaction list vs shelf 
#' 4) usefulness machine vs FB 5) Errors shelf vs list
tests <- c("sat list vs FB", "sat shelf vs FB", "sat shelf vs list", "use lendy vs FB", "error shelf vs list")
p_val <- p.adjust(c(0.008271, 0.04284,0.1201, 0.01178, 0.009371), method = "bonferroni")
adj.tests <- data.frame(tests, p_val)
kable(adj.tests)
