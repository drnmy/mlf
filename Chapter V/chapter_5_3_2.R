library(quantmod)
library(mixtools)
library(nortest)
library(stats)
library(moments)
library(plotGMM)
library(EMCluster)

getSymbols(c("^GDAXI", "^GSPC"))

dailyDAX <- dailyReturn(GDAXI$GDAXI.Adjusted)
weeklyDAX <- weeklyReturn(GDAXI$GDAXI.Adjusted)

analyze <- function(input)
{
  print("Unsupervised: ")
  clobj <- simple.init(input, nclass = 3)
  clobj <- emcluster(input, clobj)
  #summary(clobj)
  print("Object:")
  print(clobj)
  print("Pi:")
  print(clobj$pi)
  print("Mu:")
  print(clobj$Mu)
  print("LTSigma:")
  print(sqrt(clobj$LTSigma))
  
  marks <- vector()
  
  for(i in 1:dim(input)[1])
  {
    v <- 0
    dt <- as.Date(index(input[i]))
    if ( (dt > "2007-07-01") && (dt < "2008-12-01") )
      v <- 3
    else
      if ( (dt > "2014-07-01") && (dt < "2015-06-01") )
        v <- 1
    else
      if ( (dt > "2011-07-01") && (dt < "2013-06-01") )
        v <- 2
    marks <- c(marks, v)
  }
  print("Semi-supervised: ")
  clobj <- simple.init(input, nclass = 3)
  clobj <- emcluster(input, clobj, assign.class = TRUE, lab = marks)
  print("Object:")
  print(clobj)
  print("Pi:")
  print(clobj$pi)
  print("Mu:")
  print(clobj$Mu)
  print("LTSigma:")
  print(sqrt(clobj$LTSigma))
  summary(clobj)
}

plot(GDAXI$GDAXI.Adjusted)
res <- analyze(dailyDAX)

rec <- analyze(weeklyDAX)

dailySPC <- dailyReturn(GSPC$GSPC.Adjusted)
weeklySPC <- weeklyReturn(GSPC$GSPC.Adjusted)

res <- analyze(dailySPC)
res <- analyze(weeklySPC)
