
library(caret)
library(rpart)
library(rpart.plot)

setwd("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582")

Musk1 <- read_csv("Final/Musk1.csv", col_names = FALSE)
str(Musk1)


d <- dist(Musk1[,-c(1,2)], method = "euclidean") #compute euclidean distance 
Musk1.hc <- hclust(d, method = "complete" )
plot(Musk1.hc)

#choose k = 10, which is the number of lines at the line where the length gets smaller
km <- kmeans(Musk1[,-c(1,2)],10)
km$cluster

totalData <- cbind.data.frame(Musk1[,c(1,2)],km$cluster)
str(totalData)

#since we have created our clusters, now we should use classification approaches to understand the best fit

smpsize <- floor(0.7 * nrow(totalData))
set.seed(123)
train_ind <- sample(seq_len(nrow(totalData)), size = smpsize)
traindata <- totalData[train_ind,]
testdata <- totalData[-train_ind,]

RMSEDT <- data.frame()
for(k in 1:10){
  for(i in c(3,5,10,15)){
    for(j in c(0.1,0.01,0.05)){
      DTree <- rpart(X1~., data = traindata[,-2], method = "anova", minbucket = i, cp = j)    
      predDTree <- predict(DTree, testdata)
      RMSEDT1 <- RMSE(testdata$X1, predDTree)
      RMSEDT <- rbind.data.frame(RMSEDT,c(i,j,RMSEDT1))
    }
  }
}

min(RMSEDT[,3])
RMSEDT
DTree <- rpart(X1~., data = traindata[,-2], method = "anova", minbucket = 5, cp = 0.01)    
rpart.plot(DTree)

#bagCluster = 10, DT: minbucket: 5 cp:0.01   0.4466265

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1), .splitrule = "variance", .min.node.size = 5)
RF <- train(X1~., data=totalData[,-2], method="ranger", trControl = fitControl, tuneGrid = tunegrid)
RF
#0.4611957  

#less clusters

km2 <- kmeans(Musk1[,-c(1,2)],8)
km2$cluster


totalData2 <- cbind.data.frame(Musk1[,c(1,2)],km2$cluster)

#since we have created our clusters, now we should use classification approaches to understand the best fit

smpsize2 <- floor(0.7 * nrow(totalData2))
set.seed(123)
train_ind2 <- sample(seq_len(nrow(totalData2)), size = smpsize2)
traindata2 <- totalData2[train_ind2,]
testdata2 <- totalData2[-train_ind2,]

RMSEDT2 <- data.frame()
for(k in 1:10){
  for(i in c(3,5,10,15)){
    for(j in c(0.1,0.01,0.05)){
      DTree2 <- rpart(X1~., data = traindata2[,-2], method = "anova", minbucket = i, cp = j)    
      predDTree2 <- predict(DTree2, testdata2)
      RMSEDT1 <- RMSE(testdata2$X1, predDTree2)
      RMSEDT2 <- rbind.data.frame(RMSEDT2,c(i,j,RMSEDT1))
    }
  }
}

min(RMSEDT2[,3])
RMSEDT2
#again, our lowest rate is when the tree has minbucket size 5 and cp  = 0.01. However,

DTree2 <- rpart(X1~., data = traindata2[,-2], method = "anova", minbucket = 5, cp = 0.01)    
rpart.plot(DTree2)
#as we look at the tree, it may bu subject to some "overfitting". However, still a good approach for classifying
#bagCluster = 10, DT: minbucket=5,cp=0.1, RMSE = 0.4326827

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1), .splitrule = "variance", .min.node.size = 5)
RF2 <- train(X1~., data=totalData2[,-2], method="ranger", trControl = fitControl, tuneGrid = tunegrid)
RF2
#0.4673731

#more clusters
km3 <- kmeans(Musk1[,-c(1,2)],25)
km3$cluster


totalData3 <- cbind.data.frame(Musk1[,c(1,2)],km3$cluster)

#since we have created our clusters, now we should use classification approaches to understand the best fit

smpsize3 <- floor(0.7 * nrow(totalData3))
set.seed(123)
train_ind3 <- sample(seq_len(nrow(totalData3)), size = smpsize3)
traindata3 <- totalData3[train_ind3,]
testdata3 <- totalData3[-train_ind3,]

RMSEDT3 <- data.frame()
for(k in 1:10){
  for(i in c(3,5,10,15)){
    for(j in c(0.1,0.01,0.05)){
      DTree3 <- rpart(X1~., data = traindata3[,-2], method = "anova", minbucket = i, cp = j)    
      predDTree3 <- predict(DTree3, testdata3)
      RMSEDT1 <- RMSE(testdata3$X1, predDTree3)
      RMSEDT3 <- rbind.data.frame(RMSEDT3,c(i,j,RMSEDT1))
    }
  }
}

min(RMSEDT3[,3])
RMSEDT3
#again, our lowest rate is when the tree has minbucket size 5 and cp  = 0.01. 
#However, the error rate is =  0.3840805

DTree3 <- rpart(X1~., data = traindata3[,-2], method = "anova", minbucket = 5, cp = 0.01)    
rpart.plot(DTree3)
#as we look at the tree, it may bu subject to some "overfitting". However, still a good approach for classifying
#bagCluster = 10, DT: minbucket=5,cp=0.1, RMSE = 0.4012114

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1), .splitrule = "variance", .min.node.size = 5)
RF3 <- train(X1~., data=totalData3[,-2], method="ranger", trControl = fitControl, tuneGrid = tunegrid)
RF3
#0.3779135


