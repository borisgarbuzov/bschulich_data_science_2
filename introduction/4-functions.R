#================ user defined functions =================
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

#================ function arguments as default =================
my_power <- function(n, x = 2) {
  return(n^x)
}

my_power(5)
my_power(5, 3)
my_power(x = 3)

#================ built-in functions =================
s <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mean(s) # calculates the arithmetic mean
var(s) # compute the variance
sd(s) # computes the standard deviation
