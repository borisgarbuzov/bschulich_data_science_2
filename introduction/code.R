rm(list = ls())

# start working with R----
print("Hello World")

# more basic stuff----
2 + 2 # addition
5 * 5 + 2 # multiplication and addition
5 / 5 - 3 # division and substraction
log(exp(pi)) # log, exponential, pi
sin(pi / 2) # sinusoids
exp(1)^(-2) # power
sqrt(8) # square root
1:5 # sequences
seq(1, 10, by=2) # sequences
rep(2, 3) # repeat 2 three times

# assignments and objects----
x <- 1 + 2 # put 1 + 2 in object x
x = 1 + 2 # same as above with fewer keystrokes
1 + 2 -> x # same
x # view object x
(y = 9 * 3) # put 9 times 3 in y and view the result
(z = rnorm(5)) # put 5 standard normals into z and print z

# vectors----
a <- c(1, 2, 5, 3, 6, -2, 4)
a
b <- c("one", "two", "three")
b
c <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
c

x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)
x + y
x * y
length(x)

# matrices----
y <- matrix(1:15, nrow=5, ncol=3)
y
cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
mymatrix

# arrays----
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))
z

# data frames----
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type2")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

# factors----
status <- c("Poor", "Improved", "Excellent", "Poor")
status <- factor(status)
status

# lists----
list_data <- list(color_green = "Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
list_data

# control flow example----
category <- 'A'
price <- 10
if (category == 'A'){
  cat('A vat rate of 8% is applied.', 'The total price is', price * 1.08)  
} else if (category == 'B'){
  cat('A vat rate of 10% is applied.', 'The total price is', price * 1.10)  
} else {
  cat('A vat rate of 20% is applied.', 'The total price is', price * 1.20)  
}

# for loops----
for (i in 1:10) {
  cat(i, "\n")
}

for (i in 1:10) {
  if (i %% 2 == 0) {
    cat(i, "\n")
  }
}

# nested for loop----
for (i in 1:5) {
  for (j in 1:2) {
    cat(i * j, "\n")
  }
}

# while loops----
i <- 1
while (i <= 10) {
  cat(i, "\n")
  i = i + 1
}

cube <- function(x, n) {
  x^3
}
x <- cube(4)

# user defined functions----
# Create a function to print squares of numbers in sequence.
my_function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}

# Call the function my_function supplying 6 as an argument.
my_function(6)

# We can call the function with arguments value by name
my_function(a = 6)

# function arguments as default----
my_power <- function(n, x = 2) {
  return(n^x)
}

my_power(5)
my_power(5, 3)

# built-in functions----
s <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mean(s) # calculates the arithmetic mean
var(s) # compute the variance
sd(s) # computes the standard deviation

# pie charts----
# Create data for the graph.
x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

# Plot the chart.
pie(x, labels)

# bar charts----
# Create the data for the chart
H <- c(7,12,28,3,41)
M <- c("Mar","Apr","May","Jun","Jul")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Month",ylab="Revenue",col="blue",
        main="Revenue chart",border="red")

# boxplots----
input <- mtcars[,c('mpg','cyl')]

# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

# histograms----
# Create data for the graph.
v <-  c(9,13,21,8,36,22,12,41,31,33,19)

# Create the histogram.
hist(v, xlab = "Weight", col = "yellow", border = "blue")

# line graphs----
# Create the data for the chart.
v <- c(7,12,28,3,41)

# Plot the bar chart.
plot(v,type = "o",  col = "red",  xlab = "Month",  ylab = "Rain fall",
     main = "Rain fall chart")

# scatterplots----
# Get the input values.
input <- mtcars[,c('wt','mpg')]

# Plot the chart for cars with weight between 2.5 to 5 and mileage between 15 and 30.
plot(x = input$wt,y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),		 
     main = "Weight vs Milage"
)

# reading csv----
data <- read.csv("input.csv")
data

# analyzing the csv file----
is.data.frame(data)
ncol(data)
nrow(data)

# investigate data----
# get the max salary from data frame.
max(data$salary)
# get the person detail having max salary.
subset(data, salary == max(salary))
# get all the people working in IT department
subset(data, dept == "IT")
# get the persons in IT department whose salary is greater than 600
subset(data, salary > 600 & dept == "IT")

# writing into a csv file----
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))

# write filtered data into a new file.
write.csv(retval, "output.csv")
newdata <- read.csv("output.csv")
newdata

