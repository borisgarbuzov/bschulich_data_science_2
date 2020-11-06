rm(list = ls())
x1 = 1
n = 10
a = -1
x = rep(0, n)
x[1] = x1
for (i in 2:n)
{
  x[i] = x[i-1] * a
}
print (x)
plot(x, type = 'b')

# sinusoid
rm(list = ls())
n = 10
x = c(1,2)
for (i in 3:n)
{
  x[i] = -x[i-2] 
}
print(x)
plot(x, type = 'b')



# AR2:
# general
rm(list = ls())
n = 30
# starting conditions
x1 = 1
x2 = 2
mySd = 0.8
myNoise = rnorm(n=n, sd = mySd)
x = c(x1, x2) + myNoise[1:2]
# coefficients
a1 = 0
a2 = -0.8
for (i in 3:n)
{
  deterministicRecursivePart = a1 * x[i-1] + a2 * x[i-2] 
  innovation = myNoise[i]
  x[i] = deterministicRecursivePart + innovation
}
print(x)
plot(x, type = 'b')

# https://www.r-bloggers.com/2010/05/dynamic-modeling-1-linear-difference-equations/


