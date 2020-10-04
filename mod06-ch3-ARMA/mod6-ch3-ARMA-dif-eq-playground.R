rm(list = ls())
x1 = 1
n = 10
a = -2
x = rep(0, n)
x[1] = x1
for (i in 2:n)
{
  x[i] = x[i-1] * a
}
print (x)
plot(x)

# sinusoid

rm(list = ls())
n = 10
x = rep(0, n)
x[1] = 1
x[2] = 2
for (i in 3:n)
{
  x[i] = -x[i-2] 
}
print (x)
plot(x)





