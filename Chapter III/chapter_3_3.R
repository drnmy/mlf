# Данните са https://archive.ics.uci.edu/ml/datasets/Polish+companies+bankruptcy+data#

library(foreign)
library(stats)
library(caret)

readData <- function(bAllData = FALSE)
{
  if (TRUE == bAllData) {
    d1 <- read.arff("c:/Stanio/Mono_S/Data/1year.arff")
    d2 <- read.arff("c:/Stanio/Mono_S/Data/2year.arff")
    d3 <- read.arff("c:/Stanio/Mono_S/Data/3year.arff")
    d4 <- read.arff("c:/Stanio/Mono_S/Data/4year.arff")
    d5 <- read.arff("c:/Stanio/Mono_S/Data/5year.arff")
    
    data <- rbind(d1,d2,d3, d4, d5)    
  } else {
    data <- read.arff("c:/Stanio/Mono_S/Data/1year.arff")
    #data <- read.arff("c:/Stanio/Mono_S/Data/5year.arff")    
  }
  data
}


data <- readData(FALSE)


#data <- data[, which(names(data) %in% c("class","Attr3", "Attr6", "Attr7", "Attr8", "Attr9"))]

split <- createDataPartition(y = data$class,p = 0.85,list = FALSE)
train <- data[split,]
train <- na.omit(train)
train[train == -Inf] <- 0
train[train == Inf] <- 0
test <- data[-split,]
test <- na.omit(test)
test[test == -Inf] <- 0
test[test == Inf] <- 0

summary(train$class)
summary(test$class)

cutoffExtremes <- function(data)
{
  #data$Attr3[data$Attr3 < -20] <- -20
  #data$Attr6[data$Attr6 < -20] <- -20
  #data$Attr7[data$Attr7 < -20] <- -20
  #data$Attr7[data$Attr7 > 20] <- 20
  #data$Attr8[data$Attr8 < -20] <- -20
  #data$Attr8[data$Attr8 > 20] <- 20
  #data$Attr9[data$Attr9 < -20] <- -20
  #data$Attr9[data$Attr9 > 20] <- 20  
  data[, !(names(data) %in% c("class"))] <- scale(data[, !(names(data) %in% c("class"))])
  data
}

train <- cutoffExtremes(train)
test <- cutoffExtremes(test)

trCntl <- trainControl(method = "CV",number = 25)
logitModel <- train(class ~ .,data = train,trControl = trCntl,method="glm",family = "binomial")

summary(logitModel)

confusionMatrix(logitModel)

pdata <- predict(logitModel, test)

# generate confusion matrix for hold back data
confusionMatrix(pdata,reference=test$class)

library(ROCR)
library(Metrics)

logitModel <- glm(class ~ ., family=binomial(link='logit'),data=train)
summary(logitModel)

pdata <- predict(logitModel, test)

prX <- pdata
prY <- test$class

pr <- prediction( prX, prY)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf, colorize=TRUE)

# As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

### 2. MDA

library(mda)

mdaModel <- mda(class ~ Attr3 + Attr6 + Attr7 + Attr8 + Attr9, train)
summary(mdaModel)
confusion(mdaModel)
plot(mdaModel, data=train)

pdata <- predict(mdaModel, test)
confusionMatrix(pdata,reference=test$class)
pr <- prediction(pdata, test$class)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf, colorize=TRUE)

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc

### 3. FDA

fdaModel <- fda(class ~ Attr3 + Attr6 + Attr7 + Attr8 + Attr9, data=train)
summary(fdaModel)
confusion(fdaModel)

### 4. LDA

library(MASS)

ldaModel <- lda(class ~ Attr3 + Attr6 + Attr7 + Attr8 + Attr9, train)
summary(ldaModel)
confusion(ldaModel)
plot(ldaModel)

pdata <- predict(ldaModel, test)
pr <- prediction(pdata$posterior[,2], test$class) 
perf <- performance(pr,"tpr","fpr")
plot(perf,colorize=TRUE)

# As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

### 5. Neural network

library(neuralnet)

#scaledtrain <- scale(train)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#normTrain <- as.data.frame(lapply(train, normalize))
#normTest  <- as.data.frame(lapply(test, normalize))
normTrain <- train
normTrain$Attr3 <- scale(normTrain$Attr3)
normTrain$Attr6 <- scale(normTrain$Attr6)
normTrain$Attr7 <- scale(normTrain$Attr7)
normTrain$Attr8 <- scale(normTrain$Attr8)
normTrain$Attr9 <- scale(normTrain$Attr9)
normTest <- test
normTest$Attr3 <- scale(normTest$Attr3)
normTest$Attr6 <- scale(normTest$Attr6)
normTest$Attr7 <- scale(normTest$Attr7)
normTest$Attr8 <- scale(normTest$Attr8)
normTest$Attr9 <- scale(normTest$Attr9)


nn <- neuralnet(class ~ ., 
                data=normTrain,
                hidden=c(0),
                algorithm="sag",
                linear.output=FALSE)
nn$result.matrix
plot(nn)

tempTest <- normTest[, !(names(normTest) %in% c("class"))]
nn.results <- compute(nn, tempTest)

pred.weights <- nn.results$net.result
idx <- apply(pred.weights, 1, which.max) - 1
table(idx, test$class)

cm <- confusionMatrix(as.factor(idx), normTest$class)
cm
cm$table / length(normTest$class)

prob = compute(nn, tempTest )
prob.result <- prob$net.result



train_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(train[,-6], train$class,
                    method = "nnet",
                    trControl= train_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit
                   )

prop.table(table(train$class))   #Baseline Accuracy

# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, train)
table(train$class, nnet_predictions_train)

#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)
table(test$class, nnet_predictions_test)


### 6. NNET

library(nnet)

normTrain <- train
normTrain$Attr3 <- scale(normTrain$Attr3)
normTrain$Attr6 <- scale(normTrain$Attr6)
normTrain$Attr7 <- scale(normTrain$Attr7)
normTrain$Attr8 <- scale(normTrain$Attr8)
normTrain$Attr9 <- scale(normTrain$Attr9)
normTest <- test
normTest$Attr3 <- scale(normTest$Attr3)
normTest$Attr6 <- scale(normTest$Attr6)
normTest$Attr7 <- scale(normTest$Attr7)
normTest$Attr8 <- scale(normTest$Attr8)
normTest$Attr9 <- scale(normTest$Attr9)

nn <- nnet(class ~ ., data=normTrain, size=5, maxit=1000)
pdata <- predict(nn, normTest, type="class")
pdata <- as.factor(pdata)
confusionMatrix(pdata, normTest$class)


### 7. SVM

library(e1071)

normTrain <- train
normTrain$Attr3 <- scale(normTrain$Attr3)
normTrain$Attr6 <- scale(normTrain$Attr6)
normTrain$Attr7 <- scale(normTrain$Attr7)
normTrain$Attr8 <- scale(normTrain$Attr8)
normTrain$Attr9 <- scale(normTrain$Attr9)
normTest <- test
normTest$Attr3 <- scale(normTest$Attr3)
normTest$Attr6 <- scale(normTest$Attr6)
normTest$Attr7 <- scale(normTest$Attr7)
normTest$Attr8 <- scale(normTest$Attr8)
normTest$Attr9 <- scale(normTest$Attr9)

svmModel <- svm(formula = class ~ ., 
                data = normTrain, 
                type = 'C-classification', 
                kernel = 'sigmoid')

tempTest <- normTest[, !(names(normTest) %in% c("class"))]
pdata <- predict(svmModel, newdata = tempTest) 

cm <- confusionMatrix(pdata, normTest$class)
cm
cm$table / length(normTest$class)


graph <- data.frame(matrix(ncol = 5, nrow = 0))

for(i in 1:150) {
   svmModel <- svm(formula = class ~ ., 
                   data = normTrain, 
                   type = 'C-classification', 
                   kernel = 'sigmoid',
                   class.weights= c("0" = 1, "1" = i))

   tempTest <- normTest[, !(names(normTest) %in% c("class"))]
   pdata <- predict(svmModel, newdata = tempTest) 

   cm <- confusionMatrix(pdata, normTest$class)
   pct <- (cm$table / length(normTest$class))*100

   graph <- rbind(graph, c(i, pct[1,1], pct[1,2], pct[2,1], pct[2,2]))
}

x <- c("w", "(0,0)", "(0,1)", "(1,0)", "(1,1)")
colnames(graph) <- x


require(ggplot2)
require(reshape2)

g <- melt(graph, id.vars="w", variable.name='series')

ggplot(g, aes(w,value)) + geom_line(aes(colour = series))
