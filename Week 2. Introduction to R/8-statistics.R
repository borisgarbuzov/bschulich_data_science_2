#================ LLT example =================
LawOfLargeNumbers <- function(num.tries)
{
  TrueMean = 3.5
  DiceMeansArray <- array(data = NA, dim = num.tries)
  for (n in 1:num.tries) {
    DiceSamples <- sample(1:6, size = n, replace = TRUE)
    DiceMeansArray[n] <- mean(DiceSamples)
  }
  plot(DiceMeansArray, type = "l", col = "#54aa68", xlab = "number of trials",
       ylab = "average", main = "Average dice rolls by number of rolls")
  abline(h = TrueMean, col = "#4b73b1", lwd = 3)
  legend("topright", legend = c("theoretical mean", "observed mean"),
         col = c("#4b73b1", "#54aa68"), bg = "transparent", lty = 1:1, lwd = 2,
         box.lty = 0, cex = 0.8)
}

LawOfLargeNumbers(num.tries = 1000)

#================ CLT example =================
CentralLimitTheorem <- function(n, r, min, max)
{
  Samples <- runif(n * r, min = min, max = max)
  Means <- rowMeans(matrix(Samples, nrow = r, ncol = n))
  hist(Samples, xlab = "sample values", 
       main = "Sample of uniform distribution")
  hist(Means, xlab = "means", main = "Sample mean")
}

CentralLimitTheorem(n = 200, r = 1000, min = -10, max = 10)
