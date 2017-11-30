library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
library(reshape2)
library(knitr)
library(xtable)
library(lubridate)

dat.raw <- read.csv("data/user-study-data.csv", sep = "|", check.names = F)
dat.nopilot <- dat.raw[3:18,]

# change all non-residence house-types to private
dat.nopilot$`house-type`[dat.nopilot$`house-type` != "Student residence"] <- "Private house"
dat.nopilot$`house-type` <- factor(dat.nopilot$`house-type`)

# count users in demographics
user.count <- dat.nopilot %>%
  group_by(`house-type`, age, gender) %>%
  summarise(count = n())

xtable(user.count)

# Prep analysis data
p.dat <- dat.nopilot

# Remove columns (not used by any user meaningfully)
drop <- c("house-type_other", "used-lending-app", "used-lending-app_exp")
p.dat <- p.dat[,!names(p.dat) %in% drop] 

# Reshuffle groups to align all similar tasks in the same column

# Split groups
groups <- split(p.dat, p.dat$group)
A <- groups[[1]]
B <- groups[[2]]
C <- groups[[3]]
D <- groups[[4]]

# reorder columns to match A
col_order <- c("participant-number", "group", "age", "gender", "house-type", 
               "t2", "t2q", "t1", "t1q")
col_full <- c(col_order, setdiff(names(B), col_order))
B <- B[col_full]
names(B) <- names(A)

col_order_c <- c("participant-number", "group", "age", "gender", "house-type", 
               "t2", "t2q", "t3", "t3q", "t1", "t1q", "t5", "t5q", "t4", "t4q")
col_full_c <- c(col_order_c, setdiff(names(C), col_order_c))
C <- C[col_full_c]
names(C) <- names(A)

col_order_d <- c("participant-number", "group", "age", "gender", "house-type", 
                 "t3", "t3q", "t2", "t2q", "t1", "t1q", "t5", "t5q", "t4", "t4q")
col_full_d <- c(col_order_d, setdiff(names(D), col_order_d))
D <- D[col_full_d]
names(D) <- names(A)

# Combine all into one data frame
pdat <- rbind(A, B, C, D)

# Convert time to number of seconds
time <- as.character(A[1,10])
time <- strsplit(time, ":")
time <- unlist(time)
time <- paste0(c(time[1],time[2], time[3]), collapse = ":")
period_to_seconds(hms(time))

# Do it for all time fields in pdat
to_seconds <- function(st){
  s <- unlist(strsplit(as.character(st), ":"))
  x <- as.numeric(s[4]) * .001
  s <- paste0(c(s[1], s[2], s[3]), collapse = ":")
  return(period_to_seconds(hms(s)) + x)
}

pdat$t1 <- sapply(pdat$t1, FUN = to_seconds)
pdat$t2 <- sapply(pdat$t2, FUN = to_seconds)
pdat$t3 <- sapply(pdat$t3, FUN = to_seconds)
pdat$t4 <- sapply(pdat$t4, FUN = to_seconds)
pdat$t5 <- sapply(pdat$t5, FUN = to_seconds)

write.table(file = "data/data-ready.text", x = pdat, quote = FALSE, row.names = FALSE, sep = "|")

# Prepare data for report
names(pdat)
colnames.report <- c("Participant Number", "Participant group", "Age group", "Gender", "House type",         
                     "Task 1-time", "Task 1-satisfaction",               
                     "Task 2-time", "Task 2-satisfaction",                
                     "Task 3-time", "Task 3-satisfaction",                
                     "Task 4-time", "Task 4-satisfaction",            
                     "Task 5-time", "Task 5-satisfaction",
                     "Q1","Q2","Q3","Q3b","Q4","Q4b","Q5","Q5b","Q6","Q6b","Q7")

report.dat <- pdat
names(report.dat) <- colnames.report

write.table(file = "data/data-report.tsv", x = report.dat, quote = FALSE, row.names = FALSE, sep = "\t")



# 
# user.reasons <- melt(user.reasons)
# 
# ggplot(user.reasons,aes(x=variable,y=value)) + 
#   geom_bar(stat="identity",position="dodge") +
#   ggtitle("Reasons to borrow items") + 
#   geom_text(aes(label = value), hjust=1.5, size=10, color="white") +
#   coord_flip() + ylab("Count") + xlab("Reason") +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size=24),
#         axis.text =  element_text(size = 18),
#         axis.title = element_text(size = 20),
#         plot.margin = unit(c(1,1,1,1), "cm"))
# 
# # how comfortable borrowing?
# colnames(survey.data.raw)[21] <- "How comfortable"
# user.comfort <- data.frame(table(survey.data.raw[21]))
# 
# xtable(user.comfort)
# 
# # knowing the lender
# colnames(survey.data.raw)[22] <- "How well"
# user.know <- data.frame(table(survey.data.raw[22]))
# 
# xtable(user.know)
# 
# # rank according to importance when borrowing
# colnames(survey.data.raw)[23:28]
# newcols <- str_extract_all(colnames(survey.data.raw)[23:28],"\\| [A-Za-z ]+")
# newcols <- str_extract_all(newcols,"[a-zA-Z ,/]+")
# colnames(survey.data.raw)[23:28] <- newcols
# 
# rank.data <- melt(survey.data.raw[23:28])
# 
# user.rank <- rank.data %>% 
#   group_by(variable) %>% 
#   summarise_all(funs(mean, median, min, max))
# 
# ggplot(user.rank,aes(x=variable,y=median)) + 
#   geom_bar(stat="identity",position="dodge") +
#   ggtitle("Important factors to borrow items ranked") +
#   theme_bw() +
#   coord_flip() +
#   theme(plot.title = element_text(hjust = 0.5, face="bold"), plot.margin = unit(c(1,1,1,1), "cm"))
# 
# ggplot(user.rank,aes(x=variable,y=mean)) + 
#   geom_bar(stat="identity",position="dodge") +
#   ggtitle("Important factors to borrow items ranked") +
#   theme_bw() +
#   coord_flip() +
#   theme(plot.title = element_text(hjust = 0.5, face="bold"), plot.margin = unit(c(1,1,1,1), "cm"))
# 
# # provide the lender a return date?
# colnames(survey.data.raw)[29] <- "Provide date"
# user.date <- data.frame(table(survey.data.raw[29]))
# 
# kable(user.date)
# 
# # lending questions
# # how do people approach to borrow?
# newcols <- str_extract_all(colnames(survey.data.raw)[33:38],"\\[(.*?)\\]")
# newcols <- str_extract_all(newcols,"[a-zA-Z ,/]+")
# colnames(survey.data.raw)[33:38] <- newcols
# lend.user.approach <- survey.data.raw[33:38] %>% 
#   summarise_all(funs(sum))
# 
# lend.user.approach <- melt(lend.user.approach)
# 
# ggplot(lend.user.approach,aes(x=variable,y=value)) + 
#   geom_bar(stat="identity",position="dodge") +
#   ggtitle("How lenders are approached for item requests") +
#   geom_text(aes(label = value), hjust=1.5, size=10, color="white") +
#   scale_fill_manual(values=c("blue", "red", "green", "yellow", "pink", "brown")) +
#   coord_flip() + ylab("Count") + xlab("Medium") +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size=24),
#         axis.text =  element_text(size = 18),
#         axis.title = element_text(size = 20),
#         plot.margin = unit(c(1,1,1,1), "cm"))
# 
# # knowing the borrower
# colnames(survey.data.raw)[40] <- "How well lender"
# lend.user.know <- data.frame(table(survey.data.raw[40]))
# 
# xtable(lend.user.know)
# 
# # lender comfort
# colnames(survey.data.raw)[41] <- "Lender comfort"
# lend.user.comfort <- data.frame(table(survey.data.raw[41]))
# 
# xtable(lend.user.comfort)
# 
# # Factors for lending
# newcols <- str_extract_all(colnames(survey.data.raw)[42:50],"\\[(.*?)\\]")
# newcols <- str_extract_all(newcols,"[a-zA-Z ,/]+")
# colnames(survey.data.raw)[42:50] <- newcols
# lend.user.reasons <- survey.data.raw[42:50] %>% 
#   summarise_all(funs(sum))
# 
# lend.user.reasons <- melt(lend.user.reasons)
# 
# ggplot(lend.user.reasons,aes(x=variable,y=value)) + 
#   geom_bar(stat="identity",position="dodge") +
#   ggtitle("Reasons to lend items") +
#   geom_text(aes(label = value), hjust=1.5, size=10, color="white") +
#   coord_flip() + ylab("Count") + xlab("Reason") +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size=24),
#         axis.text =  element_text(size = 18),
#         axis.title = element_text(size = 20),
#         plot.margin = unit(c(1,1,1,1), "cm"))
# 
# # did you as a lender know the return date?
# colnames(survey.data.raw)[52] <- "Know date"
# lend.user.date <- data.frame(table(survey.data.raw[52]))
# 
# kable(lend.user.date)
# 
# # How honest were you in survey?
# colnames(survey.data.raw)[58] <- "Honesty"
# user.honesty <- data.frame(table(survey.data.raw[58]))
# 
# kable(lend.user.date)
