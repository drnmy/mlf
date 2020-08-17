library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests

df <- na.omit(df)
df <- scale(df)
head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 3, nstart = 25)
str(k2)

fviz_cluster(k2, data = df)

x <- rbind(matrix(rnorm(1000, sd = 0.3), ncol = 2),
           matrix(rnorm(1000, mean = 0.5, sd = 0.3), ncol = 2))

y <- x[1000,]

x <- rbind(x, c(-0.5, 1.25))
x <- rbind(x, c(-0.6, 1.35))
x <- rbind(x, c(-0.7, 1.45))
x <- rbind(x, c(-0.8, 1.15))
x <- rbind(x, c(-0.9, 1.48))
x <- rbind(x, c(-0.55, 1.47))
x <- rbind(x, c(-0.65, 1.32))
x <- rbind(x, c(-0.76, 1.12))
x <- rbind(x, c(-0.82, 1.05))

colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)

plot(x, col='black')

points(y, col='red')


# Elbow method
fviz_nbclust(x, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(x, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(x, kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
  labs(subtitle = "Gap statistic method")

