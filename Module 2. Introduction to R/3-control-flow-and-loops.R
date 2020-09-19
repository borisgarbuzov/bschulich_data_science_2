rm(list = ls())

#================ control flow example =================
category <- 'A'
price <- 10
if (category == 'A'){
  cat('A tax rate of 8% is applied.', 'The total price is', price * 1.08)
} else if (category == 'B'){
  cat('A tax rate of 10% is applied.', 'The total price is', price * 1.10)
} else {
  cat('A tax rate of 20% is applied.', 'The total price is', price * 1.20)
}

#================ for loops =================
for (i in 1:10) {
  cat(i, "\n")
}

for (i in 1:10) {
  if (i %% 2 == 0) {
    cat(i, "\n")
  }
}

#================ nested for loop =================
for (i in 1:5) {
  for (j in 1:2) {
    cat(i * j, "\n")
  }
}

#================ while loops =================
i <- 1
while (i <= 10) {
  cat(i, "\n")
  i = i + 1
}
