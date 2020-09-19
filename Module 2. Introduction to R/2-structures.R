rm(list = ls())

#================ vectors =================
a <- c(1, 2, 5, 3, 6, -2, 4)
a

class(a)
typeof(a)
is.vector(a)

aMix = c(1, "a")
class(aMix)
typeof(aMix)
is.vector(aMix)

b <- c("one", "two", "three")
b
c <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
c

x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)
x + y
x * y
length(x)

# vector element recycling
v1 <- c(1, 2, 3, 4)
v2 <- c(5, 6)
v1v2Res <- v1 + v2
v1v2Res

# accessing vector elements using position.
t <- c("Sun", "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat")
u <- t[c(2,3,6)]
u

# accessing vector elements using logical indexing.
v <- t[c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
v

# accessing vector elements using negative indexing.
x <- t[c(-2,-5)]
x

#================ matrices =================
y <- matrix(1:15, nrow=5, ncol=3)
y
cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
mymatrix

# access the element at 2nd column and 1st row.
mymatrix[1, 2]

# access the element at 2nd column and 1st row.
mymatrix[2, 1]

# access only the 2nd row.
mymatrix[2,]

# access only the 1rd column.
mymatrix[, 1]

# create two 2x3 matrices.
matrix1 <- matrix(c(3, 9, -1, 4, 2, 6), nrow = 2)
matrix1

matrix2 <- matrix(c(5, 2, 0, 9, 3, 4), nrow = 2)
matrix2

# add the matrices.
result <- matrix1 + matrix2
result

# subtract the matrices
result <- matrix1 - matrix2
result

result <- matrix1 * matrix2
result

# divide the matrices
result <- matrix1 / matrix2
result

#================ arrays =================
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))
z

# print the third row of the second matrix of the array.
z[3,,2]

# print the element in the 1st row and 3rd column of the 1st matrix.
z[1, 3, 1]

# print the 2nd Matrix.
z[,, 2]

# create two vectors of different lengths.
vector1 <- c(5, 9, 3)
vector2 <- c(10, 11, 12, 13, 14, 15)

# take these vectors as input to the array.
array1 <- array(c(vector1, vector2), dim = c(3, 3, 2))

# create two vectors of different lengths.
vector3 <- c(9, 1, 0)
vector4 <- c(6, 0, 11, 3, 14, 1, 2, 6, 9)
array2 <- array(c(vector1, vector2), dim = c(3, 3, 2))

# create matrices from these arrays.
matrix1 <- array1[,, 2]
matrix2 <- array2[,, 2]

# add the matrices.
result <- matrix1+matrix2
result

#================ data frames =================
ID <- c(10, 20, 30, 40)
items <- c("book", "pen", "textbook", "pencil case")
store <- c("Poor", "Improved", "Excellent", "Poor")
price <- c(2.5, 8.0, 10.0, 7.0)
df <- data.frame(ID, items, store, price)
df

# get the structure of the data frame.
str(df)

# print the summary.
summary(df)

# extract specific columns.
result <- data.frame(df$items, df$store)
result

# extract first two rows.
result <- df[1:2,]
result

# extract 2nd and 4th row with 1st and 3rd column.
result <- df[c(2, 4), c(1, 3)]
result

#================ factors =================
status <- c("Poor", "Improved", "Excellent", "Poor")
status <- factor(status)
status
is.vector(status)
is.factor(status)

#================ lists =================
list_data <- list(color_green = "Green", "Red", c(21,32,11), TRUE, 51.23, 119.1)
list_data

# access the first element of the list.
list_data[1]

# access the third element. As it is also a list, all its elements will be printed.
list_data[3]

# access the list element using the name of the element.
list_data$color_green

# add element at the end of the list.
list_data[7] <- "New element"
list_data

# Update the 4th Element.
list_data[4] <- FALSE
list_data[4]
