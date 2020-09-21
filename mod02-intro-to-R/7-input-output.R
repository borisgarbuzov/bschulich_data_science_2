rm(list = ls())

# ==================== reading csv ===========================
setwd("~/schulich_data_science_2/mod02-intro-to-R/")
getwd()
data <- read.csv("input.csv")
data

# ==================== analyzing the csv file ===========================
is.data.frame(data)
ncol(data)
nrow(data)
rownames(data)
colnames(data)

# ==================== investigate data ===========================
# get the max salary from data frame.
max(data$salary)
# print the whole summary of this column
summary(data$salary)
# get the person detail having max salary.
subset(data, salary == max(salary))
# get all the people working in IT department
subset(data, dept == "IT")
# get the persons in IT department whose salary is greater than 600
subset(data, salary > 600 & dept == "IT")
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))

# ==================== writing into a csv file ===========================

# write filtered data into a new file.
write.csv(retval, "output.csv")
newdata <- read.csv("output.csv")
newdata
