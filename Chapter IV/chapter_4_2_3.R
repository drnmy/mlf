library(quantmod)
library(stats)
library(tidyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(kohonen)

data <- read.csv("C:/Stanio/MONO_S/Data/StressTest/TRA_CR.csv")
data <- as.data.frame(data)

data <- data[which(data$Country==0),]
data <- data[which(data$Country_rank==0),]
data <- data[which(data$Exposure==0),]
data <- data[which(data$Portfolio==0),]

data2017 <- data[which(data$Period == 201712),]

dataActual <- data[which(data$Scenario==11),]

inputActual <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Bank_name", "183401", "183402", "183403")
colnames(inputActual) <- x

for(i in unique(dataActual$Bank_name)) {
  tmp <- dataActual[which(dataActual$Bank_name==i),]
  tmp <- tmp[, which( names(tmp) %in% c("Bank_name", "Item", "Amount"))]
  tmp <- tidyr::spread(tmp, Item, Amount)
  inputActual <- rbind(inputActual, tmp)
}

inputActual <- na.omit(inputActual)
inputActual <- inputActual[,2:4]
inputActual$`183401` <- as.numeric(inputActual$`183401`)
inputActual$`183402` <- as.numeric(inputActual$`183402`)
inputActual$`183403` <- as.numeric(inputActual$`183403`)
inputActual <- scale(inputActual)
head(inputActual)

distance <- get_dist(inputActual)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(inputActual, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Брой клъстери K",
     ylab="Общо вътрешно-клъстерна SS")

# Elbow method
fviz_nbclust(inputActual, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow метод")

# Silhouette method
fviz_nbclust(inputActual, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette метод")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(inputActual, kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
  labs(subtitle = "Gap метод")


k2 <- kmeans(inputActual, centers = 3, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)


k2 <- kmeans(inputActual, centers = 4, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)

#grid.size <- ceiling(dim(inputActual)[1] ^ (1/2.5))
grid.size <- 6
som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', toroidal = T)
som.model <- som(data.matrix(inputActual), grid = som.grid)

plot(som.model, type = 'mapping', palette.name = terrain.colors)
plot(som.model, type = 'counts')
plot(som.model, type = "dist.neighbours", palette.name = terrain.colors)


### BASELINE

dataBaseline <- data[which(data$Scenario==2),]

dataBaseline <- dataBaseline[which(dataBaseline$Period == 202012),]
inputActual <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Bank_name", "183401", "183402", "183403")
colnames(inputActual) <- x

dataActual <- dataBaseline
dataActual <- na.omit(dataActual)

for(i in unique(dataActual$Bank_name)) {
  print(i)
  tmp <- dataActual[which(dataActual$Bank_name==i),]
  tmp <- tmp[, which( names(tmp) %in% c("Bank_name", "Item", "Amount"))]
  tmp <- tidyr::spread(tmp, Item, Amount)
  inputActual <- rbind(inputActual, tmp)
}

inputActual <- na.omit(inputActual)
inputActual <- inputActual[,3:4]
inputActual$`183401` <- as.numeric(inputActual$`183401`)
inputActual$`183402` <- as.numeric(inputActual$`183402`)
inputActual$`183403` <- as.numeric(inputActual$`183403`)
inputActual <- scale(inputActual)

head(inputActual)

distance <- get_dist(inputActual)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(inputActual, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Брой клъстери K",
     ylab="Общо вътрешно-клъстерна SS")

# Elbow method
fviz_nbclust(inputActual, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow метод")

# Silhouette method
fviz_nbclust(inputActual, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette метод")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(inputActual, kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
  labs(subtitle = "Gap метод")


k2 <- kmeans(inputActual, centers = 3, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)


k2 <- kmeans(inputActual, centers = 4, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)

k2 <- kmeans(inputActual, centers = 5, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)


k2 <- kmeans(inputActual, centers = 6, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)

#### AVERSE

dataAdverse <- data[which(data$Scenario==3),]

dataAdverse <- dataAdverse[which(dataAdverse$Period == 202012),]
inputActual <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Bank_name", "183401", "183402", "183403")
colnames(inputActual) <- x

dataActual <- dataAdverse
dataActual <- na.omit(dataActual)

for(i in unique(dataActual$Bank_name)) {
  print(i)
  tmp <- dataActual[which(dataActual$Bank_name==i),]
  tmp <- tmp[, which( names(tmp) %in% c("Bank_name", "Item", "Amount"))]
  tmp <- tidyr::spread(tmp, Item, Amount)
  inputActual <- rbind(inputActual, tmp)
}

inputActual <- na.omit(inputActual)
inputActual <- inputActual[,3:4]
inputActual$`183401` <- as.numeric(inputActual$`183401`)
inputActual$`183402` <- as.numeric(inputActual$`183402`)
inputActual$`183403` <- as.numeric(inputActual$`183403`)
inputActual <- scale(inputActual)

head(inputActual)

distance <- get_dist(inputActual)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(inputActual, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Брой клъстери K",
     ylab="Общо вътрешно-клъстерна SS")

# Elbow method
fviz_nbclust(inputActual, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow метод")

# Silhouette method
fviz_nbclust(inputActual, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette метод")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(inputActual, kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
  labs(subtitle = "Gap метод")


k2 <- kmeans(inputActual, centers = 3, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)


k2 <- kmeans(inputActual, centers = 4, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)

k2 <- kmeans(inputActual, centers = 5, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)


k2 <- kmeans(inputActual, centers = 6, nstart = 25)
str(k2)

fviz_cluster(k2, data = inputActual)

