
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(stringr)
library(tidyverse)


setwd("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582/HW4")


#DATA 1 -- Online News
OnlineNewsPopularity <- read_csv("OnlineNewsPopularity/OnlineNewsPopularity/OnlineNewsPopularity.csv")
OnlineNewsPopularity <- OnlineNewsPopularity[,-1]
str(OnlineNewsPopularity)  #all numeric
table(OnlineNewsPopularity$shares)
#random split
smpsize <- floor(0.8 * nrow(OnlineNewsPopularity))
set.seed(123)
train_ind <- sample(seq_len(nrow(OnlineNewsPopularity)), size = smpsize)
traindata <- OnlineNewsPopularity[train_ind,]
testdata <- OnlineNewsPopularity[-train_ind,]

Lasso <- cv.glmnet(as.matrix(traindata), as.matrix(traindata$shares), family = "gaussian")
predLasso <- predict(Lasso,as.matrix(testdata))
RMSELasso <- RMSE(testdata$shares,predLasso)

DTree <- rpart(shares~., data = traindata, method = "anova", minbucket = 5, cp = 0.005)
rpart.plot(DTree)
predDTree <- predict(DTree, testdata)
RMSEDT1 <- RMSE(testdata$shares, predDTree)

DTree <- rpart(shares~., data = traindata, method = "anova", minbucket = 4, cp = 0.005)
rpart.plot(DTree)
predDTree <- predict(DTree, testdata)
RMSEDT2 <-RMSE(testdata$shares, predDTree)

DTree <- rpart(shares~., data = traindata, method = "anova", minbucket = 4, cp = 0.01)
rpart.plot(DTree)
predDTree <- predict(DTree, testdata)
RMSEDT3 <-RMSE(testdata$shares, predDTree)

#Different values for minbucket and cp is tried. The best results (min MSE) is gathered with 
#bucket size 4 and cp 0.005. If we decrease cp too much, that results in very complex trees however
#error rates are high. Hence, the best results are gained with these params.

fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(3,5,10), .splitrule = "variance", .min.node.size = 5)
RF <- train(shares~., data=traindata, method="ranger", trControl = fitControl, tuneGrid = tunegrid)
predRF <- predict(RF, testdata)
RMSERF <-RMSE(testdata$shares, predRF)

tunegrid <-  expand.grid(interaction.depth = c(1, 5, 9),n.trees = (1:5)*50, shrinkage = c(0.05,0.01, 0.1),n.minobsinnode = 10)
SGB <- train(shares~., data=traindata, method="gbm", trControl = fitControl, tuneGrid = tunegrid)
predSGB <- predict(SGB, testdata)
RMSESGB <-RMSE(testdata$shares, predSGB)

TotalRMSE <- c(RMSELasso, RMSEDT1, RMSEDT2, RMSEDT3, RMSERF, RMSESGB)
TotalRMSE
#According to this, Lasso is the best.


#DATA 2 - Mushroom
agaricus_lepiota <- read_delim("Mushrooms/agaricus-lepiota.csv", ";", escape_double = FALSE, col_types = cols(bruises = col_character(),`gill-attachment` = col_character()), trim_ws = TRUE)

newdata <- agaricus_lepiota
str(newdata) #categorical
sum(is.na(newdata))
table(newdata$class) 
character_vars <- lapply(newdata, class) == "character"
newdata[, character_vars] <- lapply(newdata[, character_vars], as.factor)
str(newdata)  
#since veil-type only has 1 factor, we should omit 
newdata <- newdata[,c(-16)]

smpsize2 <- floor(0.7 * nrow(newdata))
set.seed(123)
train_ind2 <- sample(seq_len(nrow(newdata)), size = smpsize2)
traindata2 <- newdata[train_ind2,]
testdata2 <- newdata[-train_ind2,]

fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001))
Lasso2 <- train(class~., data=traindata2, method="glmnet", trControl = fitControl, tuneGrid = tunegrid)
predLasso2 <- predict(Lasso2,as.matrix(testdata2))
RMSELasso2 <- RMSE(as.numeric(testdata2$class),as.numeric(predLasso2))

DTree2 <- rpart(class~., data = traindata2, method = "anova", minbucket = 5, cp = 0.005)
rpart.plot(DTree2)
predDTree2 <- predict(DTree2, testdata2)
RMSEDT21 <- RMSE(as.numeric(testdata2$class),as.numeric(predDTree2))

DTree2 <- rpart(class~., data = traindata2, method = "anova", minbucket = 4, cp = 0.005)
rpart.plot(DTree2)
predDTree2 <- predict(DTree2, testdata2)
RMSEDT22 <-RMSE(as.numeric(testdata2$class),as.numeric(predDTree2))

DTree2 <- rpart(class~., data = traindata2, method = "anova", minbucket = 2, cp = 0.5)
rpart.plot(DTree2)
predDTree2 <- predict(DTree2, testdata2)
RMSEDT23 <-RMSE(as.numeric(testdata2$class),as.numeric(predDTree2))
#as observed, optimal tree is found with 3 decision points and has a error rate of approx. 0.06

tunegrid <- expand.grid(.mtry=c(3,5,10), .splitrule = "gini", .min.node.size = 5)
RF2 <- train(class~., data=traindata2, method="ranger", trControl = fitControl, tuneGrid = tunegrid)
predRF2 <- predict(RF2, testdata2)
RMSERF2 <-RMSE(as.numeric(testdata2$class),as.numeric(predRF2))

tunegrid <-  expand.grid(interaction.depth = c(1, 5, 9),n.trees = (1:5)*50, shrinkage = c(0.05,0.01, 0.1),n.minobsinnode = 10)
SGB2 <- train(class~., data=traindata2, method="gbm", trControl = fitControl, tuneGrid = tunegrid)
predSGB2 <- predict(SGB2, testdata2)
RMSESGB2 <-RMSE(as.numeric(testdata2$class),as.numeric(predSGB2))

TotalRMSE2 <- c(RMSELasso2, RMSEDT21, RMSEDT22, RMSEDT23, RMSERF2, RMSESGB2)
TotalRMSE2

#DATA 3 - Diabetes
diabetic_data <- read_csv("dataset_diabetes/dataset_diabetes/diabetic_data.csv")

idx <- diabetic_data == "?"
is.na(diabetic_data) <- idx
sum(is.na(diabetic_data))
diabetic_data <- na.omit(diabetic_data)
table(diabetic_data$readmitted)

diabetic_data$readmitted <- str_replace(as.character(diabetic_data$readmitted), "<30", "YES")
diabetic_data$readmitted <- str_replace(as.character(diabetic_data$readmitted), ">30", "YES")
table(diabetic_data$readmitted)
newdata <- diabetic_data
character_vars <- lapply(newdata, class) == "character"
newdata[, character_vars] <- lapply(newdata[, character_vars], as.factor)
str(newdata)
newdata <- newdata[, sapply(newdata, nlevels) > 1]

smpsize3 <- floor(0.8 * nrow(newdata))
set.seed(123)
train_ind3 <- sample(seq_len(nrow(newdata)), size = smpsize3)
traindata3 <- newdata[train_ind3,]
testdata3 <- newdata[-train_ind3,]


fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001))
Lasso3 <- train(readmitted~., data=traindata3, method="glmnet", trControl = fitControl, tuneGrid = tunegrid)
predLasso3 <- predict(Lasso3,as.matrix(testdata3))
RMSELasso3 <- RMSE(as.numeric(testdata3$readmitted),as.numeric(predLasso3))

DTree3 <- rpart(readmitted~., data = traindata3, method = "anova", minbucket = 5, cp = 0.001)
rpart.plot(DTree3)
predDTree3 <- predict(DTree3, testdata3)
RMSEDT31 <- RMSE(as.numeric(testdata3$readmitted),as.numeric(predDTree3))

DTree3 <- rpart(readmitted~., data = traindata3, method = "anova", minbucket = 5, cp = 0.01)
rpart.plot(DTree3)
predDTree3 <- predict(DTree3, testdata3)
RMSEDT32 <- RMSE(as.numeric(testdata3$readmitted),as.numeric(predDTree3))

DTree3 <- rpart(readmitted~., data = traindata3, method = "anova", minbucket = 5, cp = 0.1)
rpart.plot(DTree3)
predDTree3 <- predict(DTree3, testdata3)
RMSEDT33 <- RMSE(as.numeric(testdata3$readmitted),as.numeric(predDTree3))
#when we decrease cp, our data fits better. However this sis not a good fit

tunegrid <- expand.grid(.mtry=c(3,5,10), .splitrule = "gini", .min.node.size = 5)
RF3 <- train(readmitted~., data=traindata3, method="ranger", trControl = fitControl, tuneGrid = tunegrid)
predRF3 <- predict(RF3, testdata3)
RMSERF3 <-RMSE(as.numeric(testdata3$readmitted),as.numeric(predRF3))

tunegrid <-  expand.grid(interaction.depth = c(1, 5, 9),n.trees = (1:5)*50, shrinkage = c(0.05,0.01, 0.1),n.minobsinnode = 10)
SGB3 <- train(readmitted~., data=traindata3, method="gbm", trControl = fitControl, tuneGrid = tunegrid)
predSGB3 <- predict(SGB3, testdata3)
RMSESGB3 <-RMSE(as.numeric(testdata3$readmitted),as.numeric(predSGB3))

TotalRMSE3 <- c(RMSELasso3, RMSEDT31, RMSEDT32, RMSEDT33, RMSERF3, RMSESGB3)
TotalRMSE3


#DATA 4 - Default Credit Card

default_of_credit_card_clients <- read_excel("default credit card/default of credit card clients.xls")
table(default_of_credit_card_clients$`default payment next month`) #class imbalance

newdata <- default_of_credit_card_clients 
names(newdata)[names(newdata) == "default payment next month"] <- "DEFAULT_PAY"
str(newdata)

smpsize4 <- floor(0.65 * nrow(newdata))
set.seed(123)
train_ind4 <- sample(seq_len(nrow(newdata)), size = smpsize4)
traindata4 <- newdata[train_ind4,]
testdata4 <- newdata[-train_ind4,]

Lasso4 <- cv.glmnet(as.matrix(traindata4), as.matrix(traindata4$DEFAULT_PAY), family = "gaussian")
predLasso4 <- predict(Lasso4,as.matrix(testdata4))
RMSELasso4 <- RMSE(testdata4$DEFAULT_PAY,predLasso4)

DTree4 <- rpart(DEFAULT_PAY~., data = traindata4, method = "anova", minbucket = 5, cp = 0.005)
rpart.plot(DTree4)
predDTree4 <- predict(DTree4, testdata4)
RMSEDT41 <- RMSE(testdata4$DEFAULT_PAY, predDTree4)

DTree4 <- rpart(DEFAULT_PAY~., data = traindata4, method = "anova", minbucket = 2, cp = 0.05)
rpart.plot(DTree4)
predDTree4 <- predict(DTree4, testdata4)
RMSEDT42 <- RMSE(testdata4$DEFAULT_PAY, predDTree4)

DTree4 <- rpart(DEFAULT_PAY~., data = traindata4, method = "anova", minbucket = 3 , cp = 0.001)
rpart.plot(DTree4)
predDTree4 <- predict(DTree4, testdata4)
RMSEDT43 <- RMSE(testdata4$DEFAULT_PAY, predDTree4)
#as we decrease the cp, our number of nodes(or size of the tree) gets larger. Hence RMSE value decreases

tunegrid <- expand.grid(.mtry=c(3,5,10), .splitrule = "variance", .min.node.size = 5)
RF4 <- train(DEFAULT_PAY~., data=traindata4, method="ranger", trControl = fitControl, tuneGrid = tunegrid)
predRF4 <- predict(RF4, testdata4)
RMSERF4 <-RMSE(testdata4$DEFAULT_PAY, predRF4)

tunegrid <-  expand.grid(interaction.depth = c(1, 5, 9),n.trees = (1:5)*50, shrinkage = c(0.05,0.01, 0.1),n.minobsinnode = 10)
SGB4 <- train(DEFAULT_PAY~., data=traindata4, method="gbm", trControl = fitControl, tuneGrid = tunegrid)
predSGB4 <- predict(SGB4, testdata4)
RMSESGB4 <-RMSE(testdata4$DEFAULT_PAY, predSGB4)

TotalRMSE4 <- c(RMSELasso4, RMSEDT41, RMSEDT42, RMSEDT43, RMSERF4, RMSESGB4)
TotalRMSE4
