rm(list = ls())

# ==================== pie charts ===========================
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

# ==================== boxplots ===========================
input <- mtcars[,c('mpg','cyl')]

# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

# ==================== histograms ===========================
# Create data for the graph.
v <-  c(9,13,21,8,36,22,12,41,31,33,19)

# Create the histogram.
hist(v, xlab = "Weight", col = "yellow", border = "blue")

# ==================== scatterplots ===========================
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


# ==================== line graphs ===========================
# Create the data for the chart.
v <- c(7,12,28,3,41)

# Plot the line graph.
plot(v, type = "o",  col = "red",  xlab = "Month",  ylab = "Rain fall",
     main = "Rain fall chart")
# type="o" means overplotted
