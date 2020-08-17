library(quantmod)
library(mixtools)
library(nortest)
library(stats)
library(moments)
library(plotGMM)

getSymbols(c("^GDAXI", "^GSPC"))

dailyDAX <- dailyReturn(GDAXI$GDAXI.Adjusted)
weeklyDAX <- weeklyReturn(GDAXI$GDAXI.Adjusted)

print("Дневна база:")
mean(dailyDAX)
sd(dailyDAX)
skewness(dailyDAX)
kurtosis(dailyDAX)
ad.test(dailyDAX)
shapiro.test(as.vector(dailyDAX))

print("Седмична база:")
mean(weeklyDAX)
sd(weeklyDAX)
skewness(weeklyDAX)
kurtosis(weeklyDAX)
ad.test(weeklyDAX)
shapiro.test(as.vector(weeklyDAX))

qqnorm(dailyDAX)
qqnorm(weeklyDAX)

for(i in 2:20) {
  mx <- normalmixEM(dailyDAX, k=i)
  print(mx$loglik)
}

#Expectation Maximization (EM) algorithm...
mixmdl = normalmixEM(dailyDAX, k=3)

plot_GMM(mixmdl,3)
x <- mixmdl$x
x <- data.frame(x)
ggplot2::ggplot(data.frame(x)) +
  ggplot2::geom_density(ggplot2::aes(x), color="black", fill="black") +
  ggplot2::stat_function(geom = "line", 
                         fun = plot_mix_comps,args = list(mixmdl$mu[1], mixmdl$sigma[1], 
                                                          lam = mixmdl$lambda[1]),colour = "red") +
  ggplot2::stat_function(geom = "line", 
                         fun = plot_mix_comps,args = list(mixmdl$mu[2], mixmdl$sigma[2], 
                                                          lam = mixmdl$lambda[2]),colour = "blue")

hist(mixmdl$x, probability=T, ylim=c(0,100))
lines(density(dailyDAX), lty=2, lwd=2)
curve(dnorm(x, mean=mixmdl$mu[1], sd=mixmdl$sigma[1]), 
      col="red", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[2], sd=mixmdl$sigma[2]), 
      col="green", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[3], sd=mixmdl$sigma[3]), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


plot(mixmdl,which=2)
lines(density(dailyDAX), lty=2, lwd=2)

#Кои са разпределенията?
print(mixmdl$mu)
print(mixmdl$sigma)

#Expectation Maximization (EM) algorithm...
mixmdl = normalmixEM(weeklyDAX, k=3)

hist(mixmdl$x, probability=T, ylim=c(0,100))
lines(density(dailyDAX), lty=2, lwd=2)
curve(dnorm(x, mean=mixmdl$mu[1], sd=mixmdl$sigma[1]), 
      col="red", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[2], sd=mixmdl$sigma[2]), 
      col="green", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[3], sd=mixmdl$sigma[3]), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


plot(mixmdl,which=2)
lines(density(weeklyDAX), lty=2, lwd=2)

#Кои са разпределенията?
print(mixmdl$mu)
print(mixmdl$sigma)



###### 2. S&P500



dailySPC <- dailyReturn(GSPC$GSPC.Adjusted)
weeklySPC <- weeklyReturn(GSPC$GSPC.Adjusted)

print("Дневна база:")
mean(dailySPC)
sd(dailySPC)
skewness(dailySPC)
kurtosis(dailySPC)
ad.test(dailySPC)
shapiro.test(as.vector(dailySPC))

print("Седмична база:")
mean(weeklySPC)
sd(weeklySPC)
skewness(weeklySPC)
kurtosis(weeklySPC)
ad.test(weeklySPC)
shapiro.test(as.vector(weeklySPC))

qqnorm(dailySPC)
qqnorm(weeklySPC)

#Expectation Maximization (EM) algorithm...
mixmdl = normalmixEM(dailySPC, k=3)

hist(mixmdl$x, probability=T, ylim=c(0,100))
lines(density(dailySPC), lty=2, lwd=2)
curve(dnorm(x, mean=mixmdl$mu[1], sd=mixmdl$sigma[1]), 
      col="red", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[2], sd=mixmdl$sigma[2]), 
      col="green", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[3], sd=mixmdl$sigma[3]), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


plot(mixmdl,which=2)
lines(density(dailySPC), lty=2, lwd=2)

#Кои са разпределенията?
print(mixmdl$mu)
print(mixmdl$sigma)


#Expectation Maximization (EM) algorithm...
mixmdl = normalmixEM(weeklySPC, k=3)

hist(mixmdl$x, probability=T, ylim=c(0,100))
lines(density(weeklyDAX), lty=2, lwd=2)
curve(dnorm(x, mean=mixmdl$mu[1], sd=mixmdl$sigma[1]), 
      col="red", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[2], sd=mixmdl$sigma[2]), 
      col="green", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=mixmdl$mu[3], sd=mixmdl$sigma[3]), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


plot(mixmdl,which=2)
lines(density(weeklySPC), lty=2, lwd=2)

#Кои са разпределенията?
print(mixmdl$mu)
print(mixmdl$sigma)



