library(quantmod)
library(stats)
library(factoextra)

getSymbols(c("ADS.DE", "SIE.DE", "BMW.DE", "BAS.DE", "SAP.DE"))

data <- data.frame(ADS.DE$ADS.DE.Adjusted, BAS.DE$BAS.DE.Adjusted, BMW.DE$BMW.DE.Adjusted, SAP.DE$SAP.DE.Adjusted, SIE.DE$SIE.DE.Adjusted)

data <- ROC(data)

data <- na.omit(data)

cv <- cov(data)

prc <- prcomp(data, center=TRUE, scale=TRUE)
plot(prc)
summary(prc)

fviz_eig(prc)

kprc <- kpca(~.,data,kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kprc)

#plot the data projection on the components
plot(rotated(kprc),col=as.integer(data),
     xlab="1st Principal Component",ylab="2nd Principal Component")
