if (FALSE==is.element("plotly", installed.packages()[,1])) {
  install.packages("plotly")
}

if (FALSE==is.element("dplyr", installed.packages()[,1])) {
  install.packages("dplyr")
}

if (FALSE==is.element("tidyr", installed.packages()[,1])) {
  install.packages("tidyr")
}

if (FALSE==is.element("purrr", installed.packages()[,1])) {
  install.packages("purrr")
}

if (FALSE==is.element("quantmod", installed.packages()[,1])) {
  install.packages("quantmod")
}

if (FALSE==is.element("magrittr", installed.packages()[,1])) {
  install.packages("magrittr")
}

if (FALSE==is.element("YieldCurve", installed.packages()[,1])) {
  install.packages("YieldCurve")
}

if (FALSE==is.element("ustyc", installed.packages()[,1])) {
  install.packages("ustyc")
}


library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(quantmod)
library(magrittr)
library(YieldCurve)

#сваляме данните от St. Louis Fed FRED
#проверка за тикерите на: https://fred.stlouisfed.org/series/DTB3
#за обяснение на %>% моля вижте тук: http://stackoverflow.com/questions/24536154/what-does-mean-in-r
yield_curve <- list("DTB3", "DGS2", "DGS5", "DGS10", "DGS30") %>%
  map(
    ~getSymbols(.x, auto.assign=FALSE, src="FRED")
  ) %>%
  do.call(merge,.)

myData <- yield_curve["1981::"]
myData <- na.omit(myData)
maturity <- c(0.25, 2, 5, 10, 30)


SvenssonParameters <- Svensson(myData, maturity)

sd(SvenssonParameters$beta_0)/mean(SvenssonParameters$beta_0)
sd(SvenssonParameters$beta_1)/mean(SvenssonParameters$beta_1)
sd(SvenssonParameters$beta_2)/mean(SvenssonParameters$beta_2)
sd(SvenssonParameters$beta_3)/mean(SvenssonParameters$beta_3)

sub2008 <- window(SvenssonParameters, start='2008-01-02', end='2009-01-02')

sd(sub2008$beta_0)/mean(sub2008$beta_0)
sd(sub2008$beta_1)/mean(sub2008$beta_1)
sd(sub2008$beta_2)/mean(sub2008$beta_2)
sd(sub2008$beta_3)/mean(sub2008$beta_3)

NSParameters <- Nelson.Siegel( myData, maturity )

sd(NSParameters$beta_0)/mean(NSParameters$beta_0)
sd(NSParameters$beta_1)/mean(NSParameters$beta_1)
sd(NSParameters$beta_2)/mean(NSParameters$beta_2)

subNS2008 <- window(NSParameters, start='2008-01-02', end='2009-01-02')
sd(subNS2008$beta_0)/mean(subNS2008$beta_0)
sd(subNS2008$beta_1)/mean(subNS2008$beta_1)
sd(subNS2008$beta_2)/mean(subNS2008$beta_2)

