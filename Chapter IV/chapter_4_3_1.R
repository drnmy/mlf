library(Quandl)

library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(quantmod)
library(magrittr)
library(YieldCurve)
library(stats)
library(kernlab)
library(factoextra)

yield_curve <- list("DTB3", "DTB6", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30") %>%
  map(
    ~getSymbols(.x, auto.assign=FALSE, src="FRED")
  ) %>%
  do.call(merge,.)

inputData <- yield_curve["1981::"]
inputData <- na.omit(inputData)

plot(inputData, legend.loc="topright")
cr <- cor(inputData)
library(corrplot)
corrplot(cr, type="upper", order="hclust")


library(plyr)
COR <- rollapply(inputData,90,cor,by.column = F)
COR <- na.omit(COR)
tmp <- COR[, 11]
plot(tmp)

cv <- cov(inputData)

prc <- prcomp(inputData, center=TRUE, scale=TRUE)
plot(prc)
summary(prc)

fviz_eig(prc)
tmp <- get_pca(prc)
plot(tmp$coord[,3])

kprc <- kpca(~.,inputData,kernel="rbfdot",
             kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kprc)

#plot the data projection on the components
plot(rotated(kprc),col=as.integer(data),
     xlab="1st Principal Component",ylab="2nd Principal Component")


### LIBOR
library(quantmod)

getSymbols( c("USDONTD156N", "USD1WKD156N", "USD1MTD156N", 
              "USD2MTD156N", "USD3MTD156N", "USD6MTD156N", 
              "USD12MD156N"), src="FRED")

inputData <- merge(USDONTD156N, USD1WKD156N, USD1MTD156N, USD2MTD156N, USD3MTD156N, USD6MTD156N, USD12MD156N)
inputData <- na.omit(inputData)

plot(inputData, legend.loc="topright")
cr <- cor(inputData)
library(corrplot)
corrplot(cr, type="upper", order="hclust")

COR <- rollapply(inputData,90,cor,by.column = F)
COR <- na.omit(COR)
tmp <- COR[, 8]
plot(tmp)

prc <- prcomp(inputData, center=TRUE, scale=TRUE)
plot(prc)
summary(prc)


fviz_eig(prc)
tmp <- get_pca(prc)
plot(tmp$coord[,3])
