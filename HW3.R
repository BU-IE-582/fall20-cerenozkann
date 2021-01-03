data <- read.csv("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582/HW3/GercekZamanliTuketim-01012016-30112020.csv")
names(data)[names(data) == "Tüketim.Miktarı..MWh."] <- "Consumption"

library(data.table)
library(glmnet)

str(data) 
#Since all the columns are in factor values, we should first manipulate the data.
data$Saat=as.integer(data$Saat)
data$Tarih=as.Date(data$Tarih, "%d.%m.%Y")

for (i in 1:43104){
  if(year(data$Tarih[i])==2016){
    data$day[i]=yday(data$Tarih[i])
  }
  else if(year(data$Tarih[i])==2017){
    data$day[i]=yday(data$Tarih[i])+366
  }
  else if(year(data$Tarih[i])==2018){
    data$day[i]=yday(data$Tarih[i])+731
  }
  else if(year(data$Tarih[i])==2019){
    data$day[i]=yday(data$Tarih[i])+1096
  }
  else if(year(data$Tarih[i])==2020){
    data$day[i]=yday(data$Tarih[i])+1461
  }
}

# Now we should turn the factor values in consumption into double values

data$Consumption = gsub("\\.", "", data$Consumption)
data$Consumption = gsub("\\,", ".", data$Consumption)
data$Consumption = as.numeric(data$Consumption) 


#PART A


for (i in 1:43104){
  if(i<=48){
    data$Lag48[i]=0
    data$Lag168[i]=0
  }
  else if(i>=49 && i<=168){
    data$Lag48[i]=data$Consumption[i-48]
    data$Lag168[i]=0
  }
  else{
    data$Lag48[i]=data$Consumption[i-48]
    data$Lag168[i]=data$Consumption[i-168]
  }
}

#for testing we will only need the data from November. We have 30 days to forecast
testdata <- data[42385:43104,]
#we are asked to perform a naive forecasting approach, which is basically the values given in lag48 and lag168 columns.

for(i in 1:720){
  testdata$MAPE2Day[i] = (abs(testdata$Lag48[i] - testdata$Consumption[i])/testdata$Consumption[i])
  testdata$MAPE7Day[i] = (abs(testdata$Lag168[i] - testdata$Consumption[i])/testdata$Consumption[i])
}
summary(testdata$MAPE2Day)
summary(testdata$MAPE7Day)

MAPE2DAY = 0
MAPE7DAY = 0
for(i in 1:720){
  MAPE2DAY = MAPE2DAY + testdata$MAPE2Day[i]
  MAPE7DAY = MAPE7DAY + testdata$MAPE7Day[i]
}
MAPE2DAY = MAPE2DAY/720
MAPE7DAY = MAPE7DAY/720
MAPE2DAY
MAPE7DAY

#mean of summary = MAPE

#As we can see from summary of the columns and also from the real calculations, Mean Average Percentage Error
#is smaller for Lag168. Which means 1-week predictions is more reliable than 2-days predictions

#PART B

traindata <- data[169:42384,]
lr = lm(Consumption~Lag48+Lag168,data=traindata)
lr$coefficients
summary(lr)
prediction = data.frame(predict(lr, newdata = testdata))
for(i in 1:720){
  testdata$MAPEReg[i] = (abs(prediction[i,1] - testdata$Consumption[i])/testdata$Consumption[i])
}
summary(testdata$MAPEReg)
MAPEReg = 0
for(i in 1:720){
  MAPEReg = MAPEReg + testdata$MAPEReg[i]
}
MAPEReg = MAPEReg/720
MAPEReg      


#PART C
hour1 = traindata[traindata$Saat==1,]
hour2 = traindata[traindata$Saat==2,]
hour3 = traindata[traindata$Saat==3,]
hour4 = traindata[traindata$Saat==4,]
hour5 = traindata[traindata$Saat==5,]
hour6 = traindata[traindata$Saat==6,]
hour7 = traindata[traindata$Saat==7,]
hour8 = traindata[traindata$Saat==8,]
hour9 = traindata[traindata$Saat==9,]
hour10 = traindata[traindata$Saat==10,]
hour11 = traindata[traindata$Saat==11,]
hour12 = traindata[traindata$Saat==12,]
hour13 = traindata[traindata$Saat==13,]
hour14 = traindata[traindata$Saat==14,]
hour15 = traindata[traindata$Saat==15,]
hour16 = traindata[traindata$Saat==16,]
hour17 = traindata[traindata$Saat==17,]
hour18 = traindata[traindata$Saat==18,]
hour19 = traindata[traindata$Saat==19,]
hour20 = traindata[traindata$Saat==20,]
hour21 = traindata[traindata$Saat==21,]
hour22 = traindata[traindata$Saat==22,]
hour23 = traindata[traindata$Saat==23,]
hour24 = traindata[traindata$Saat==24,]

lrHour1 = lm(Consumption~Lag48+Lag168,data=hour1)
lrHour2 = lm(Consumption~Lag48+Lag168,data=hour2)
lrHour3 = lm(Consumption~Lag48+Lag168,data=hour3)
lrHour4 = lm(Consumption~Lag48+Lag168,data=hour4)
lrHour5 = lm(Consumption~Lag48+Lag168,data=hour5)
lrHour6 = lm(Consumption~Lag48+Lag168,data=hour6)
lrHour7 = lm(Consumption~Lag48+Lag168,data=hour7)
lrHour8 = lm(Consumption~Lag48+Lag168,data=hour8)
lrHour9 = lm(Consumption~Lag48+Lag168,data=hour9)
lrHour10 = lm(Consumption~Lag48+Lag168,data=hour10)
lrHour11 = lm(Consumption~Lag48+Lag168,data=hour11)
lrHour12 = lm(Consumption~Lag48+Lag168,data=hour12)
lrHour13 = lm(Consumption~Lag48+Lag168,data=hour13)
lrHour14 = lm(Consumption~Lag48+Lag168,data=hour14)
lrHour15 = lm(Consumption~Lag48+Lag168,data=hour15)
lrHour16 = lm(Consumption~Lag48+Lag168,data=hour16)
lrHour17 = lm(Consumption~Lag48+Lag168,data=hour17)
lrHour18 = lm(Consumption~Lag48+Lag168,data=hour18)
lrHour19 = lm(Consumption~Lag48+Lag168,data=hour19)
lrHour20 = lm(Consumption~Lag48+Lag168,data=hour20)
lrHour21 = lm(Consumption~Lag48+Lag168,data=hour21)
lrHour22 = lm(Consumption~Lag48+Lag168,data=hour22)
lrHour23 = lm(Consumption~Lag48+Lag168,data=hour23)
lrHour24 = lm(Consumption~Lag48+Lag168,data=hour24)

testHour1 = testdata[testdata$Saat==1,]
testHour2 = testdata[testdata$Saat==2,]
testHour3 = testdata[testdata$Saat==3,]
testHour4 = testdata[testdata$Saat==4,]
testHour5 = testdata[testdata$Saat==5,]
testHour6 = testdata[testdata$Saat==6,]
testHour7 = testdata[testdata$Saat==7,]
testHour8 = testdata[testdata$Saat==8,]
testHour9 = testdata[testdata$Saat==9,]
testHour10 = testdata[testdata$Saat==10,]
testHour11 = testdata[testdata$Saat==11,]
testHour12 = testdata[testdata$Saat==12,]
testHour13 = testdata[testdata$Saat==13,]
testHour14 = testdata[testdata$Saat==14,]
testHour15 = testdata[testdata$Saat==15,]
testHour16 = testdata[testdata$Saat==16,]
testHour17 = testdata[testdata$Saat==17,]
testHour18 = testdata[testdata$Saat==18,]
testHour19 = testdata[testdata$Saat==19,]
testHour20 = testdata[testdata$Saat==20,]
testHour21 = testdata[testdata$Saat==21,]
testHour22 = testdata[testdata$Saat==22,]
testHour23 = testdata[testdata$Saat==23,]
testHour24 = testdata[testdata$Saat==24,]

prediction1 = data.frame(predict(lrHour1,newdata=testHour1))
prediction2 = data.frame(predict(lrHour2,newdata=testHour2))
prediction3 = data.frame(predict(lrHour3,newdata=testHour3))
prediction4 = data.frame(predict(lrHour4,newdata=testHour4))
prediction5 = data.frame(predict(lrHour5,newdata=testHour5))
prediction6 = data.frame(predict(lrHour6,newdata=testHour6))
prediction7 = data.frame(predict(lrHour7,newdata=testHour7))
prediction8 = data.frame(predict(lrHour8,newdata=testHour8))
prediction9 = data.frame(predict(lrHour9,newdata=testHour9))
prediction10 = data.frame(predict(lrHour10,newdata=testHour10))
prediction11 = data.frame(predict(lrHour11,newdata=testHour11))
prediction12 = data.frame(predict(lrHour12,newdata=testHour12))
prediction13 = data.frame(predict(lrHour13,newdata=testHour13))
prediction14 = data.frame(predict(lrHour14,newdata=testHour14))
prediction15 = data.frame(predict(lrHour15,newdata=testHour15))
prediction16 = data.frame(predict(lrHour16,newdata=testHour16))
prediction17 = data.frame(predict(lrHour17,newdata=testHour17))
prediction18 = data.frame(predict(lrHour18,newdata=testHour18))
prediction19 = data.frame(predict(lrHour19,newdata=testHour19))
prediction20 = data.frame(predict(lrHour20,newdata=testHour20))
prediction21 = data.frame(predict(lrHour21,newdata=testHour21))
prediction22 = data.frame(predict(lrHour22,newdata=testHour22))
prediction23 = data.frame(predict(lrHour23,newdata=testHour23))
prediction24 = data.frame(predict(lrHour24,newdata=testHour24))

for(i in 1:30){testHour1$MAPEHourly[i] = (abs(prediction1[i,1] - testHour1$Consumption[i])/testHour1$Consumption[i])}
for(i in 1:30){testHour2$MAPEHourly[i] = (abs(prediction2[i,1] - testHour2$Consumption[i])/testHour2$Consumption[i])}
for(i in 1:30){testHour3$MAPEHourly[i] = (abs(prediction3[i,1] - testHour3$Consumption[i])/testHour3$Consumption[i])}
for(i in 1:30){testHour4$MAPEHourly[i] = (abs(prediction4[i,1] - testHour4$Consumption[i])/testHour4$Consumption[i])}
for(i in 1:30){testHour5$MAPEHourly[i] = (abs(prediction5[i,1] - testHour5$Consumption[i])/testHour5$Consumption[i])}
for(i in 1:30){testHour6$MAPEHourly[i] = (abs(prediction6[i,1] - testHour6$Consumption[i])/testHour6$Consumption[i])}
for(i in 1:30){testHour7$MAPEHourly[i] = (abs(prediction7[i,1] - testHour7$Consumption[i])/testHour7$Consumption[i])}
for(i in 1:30){testHour8$MAPEHourly[i] = (abs(prediction8[i,1] - testHour8$Consumption[i])/testHour8$Consumption[i])}
for(i in 1:30){testHour9$MAPEHourly[i] = (abs(prediction9[i,1] - testHour9$Consumption[i])/testHour9$Consumption[i])}
for(i in 1:30){testHour10$MAPEHourly[i] = (abs(prediction10[i,1] - testHour10$Consumption[i])/testHour10$Consumption[i])}
for(i in 1:30){testHour11$MAPEHourly[i] = (abs(prediction11[i,1] - testHour11$Consumption[i])/testHour11$Consumption[i])}
for(i in 1:30){testHour12$MAPEHourly[i] = (abs(prediction12[i,1] - testHour12$Consumption[i])/testHour12$Consumption[i])}
for(i in 1:30){testHour13$MAPEHourly[i] = (abs(prediction13[i,1] - testHour13$Consumption[i])/testHour13$Consumption[i])}
for(i in 1:30){testHour14$MAPEHourly[i] = (abs(prediction14[i,1] - testHour14$Consumption[i])/testHour14$Consumption[i])}
for(i in 1:30){testHour15$MAPEHourly[i] = (abs(prediction15[i,1] - testHour15$Consumption[i])/testHour15$Consumption[i])}
for(i in 1:30){testHour16$MAPEHourly[i] = (abs(prediction16[i,1] - testHour16$Consumption[i])/testHour16$Consumption[i])}
for(i in 1:30){testHour17$MAPEHourly[i] = (abs(prediction17[i,1] - testHour17$Consumption[i])/testHour17$Consumption[i])}
for(i in 1:30){testHour18$MAPEHourly[i] = (abs(prediction18[i,1] - testHour18$Consumption[i])/testHour18$Consumption[i])}
for(i in 1:30){testHour19$MAPEHourly[i] = (abs(prediction19[i,1] - testHour19$Consumption[i])/testHour19$Consumption[i])}
for(i in 1:30){testHour20$MAPEHourly[i] = (abs(prediction20[i,1] - testHour20$Consumption[i])/testHour20$Consumption[i])}
for(i in 1:30){testHour21$MAPEHourly[i] = (abs(prediction21[i,1] - testHour21$Consumption[i])/testHour21$Consumption[i])}
for(i in 1:30){testHour22$MAPEHourly[i] = (abs(prediction22[i,1] - testHour22$Consumption[i])/testHour22$Consumption[i])}
for(i in 1:30){testHour23$MAPEHourly[i] = (abs(prediction23[i,1] - testHour23$Consumption[i])/testHour23$Consumption[i])}
for(i in 1:30){testHour24$MAPEHourly[i] = (abs(prediction24[i,1] - testHour24$Consumption[i])/testHour24$Consumption[i])}
summary(testHour1$MAPEHourly)
summary(testHour2$MAPEHourly)
summary(testHour3$MAPEHourly)
summary(testHour4$MAPEHourly)
summary(testHour5$MAPEHourly)
summary(testHour6$MAPEHourly)
summary(testHour7$MAPEHourly)
summary(testHour8$MAPEHourly)
summary(testHour9$MAPEHourly)
summary(testHour10$MAPEHourly)
summary(testHour11$MAPEHourly)
summary(testHour12$MAPEHourly)
summary(testHour13$MAPEHourly)
summary(testHour14$MAPEHourly)
summary(testHour15$MAPEHourly)
summary(testHour16$MAPEHourly)
summary(testHour17$MAPEHourly)
summary(testHour18$MAPEHourly)
summary(testHour19$MAPEHourly)
summary(testHour20$MAPEHourly)
summary(testHour21$MAPEHourly)
summary(testHour22$MAPEHourly)
summary(testHour23$MAPEHourly)
summary(testHour24$MAPEHourly)

MAPEHourlyReg = c(mean(testHour1$MAPEHourly),mean(testHour2$MAPEHourly),mean(testHour3$MAPEHourly),mean(testHour4$MAPEHourly),mean(testHour5$MAPEHourly),mean(testHour6$MAPEHourly),mean(testHour7$MAPEHourly),mean(testHour8$MAPEHourly),mean(testHour9$MAPEHourly),mean(testHour10$MAPEHourly),mean(testHour11$MAPEHourly),mean(testHour12$MAPEHourly),mean(testHour13$MAPEHourly),mean(testHour14$MAPEHourly),mean(testHour15$MAPEHourly),mean(testHour16$MAPEHourly),mean(testHour17$MAPEHourly),mean(testHour18$MAPEHourly),mean(testHour19$MAPEHourly),mean(testHour20$MAPEHourly),mean(testHour21$MAPEHourly),mean(testHour22$MAPEHourly),mean(testHour23$MAPEHourly),mean(testHour24$MAPEHourly))
#Looking at the means, error rate is relatively high between hours 10-16. Except that, especiallt at night time 
#(between 20-8) our estimation seem to fit good.

#TASK 4
#First, wide matrices for train and test data should be constructed

wideMatrixTest = matrix(nrow = 720, ncol=48)
wideMatrixTrain = matrix(nrow = 42216, ncol=48)

for(i in 1:42216){
  dummy = as.integer((i-1)/24)
  for(j in 1:48){
    if(j <= 24){
      wideMatrixTrain[i,j] = traindata$Lag48[(dummy*24)+j]
    }
    else{
      wideMatrixTrain[i,j] = traindata$Lag168[(dummy*24)+j-24]
    }
  }
}

for(i in 1:720){
  dummy = as.integer((i-1)/24)
  for(j in 1:48){
    if(j <= 24){
      wideMatrixTest[i,j] = testdata$Lag48[(dummy*24)+j]
    }
    else{
      wideMatrixTest[i,j] = testdata$Lag168[(dummy*24)+j-24]
    }
  }
}

noisyTestData = cbind(testdata,wideMatrixTest)
noisyTrainData = cbind(traindata,wideMatrixTrain)
noisyTestData$LassoResult = 0

lassoHourlyTest = split(noisyTestData,noisyTestData$Saat)
lassoHourlyTrain = split(noisyTrainData,noisyTrainData$Saat)

lambda = data.table(model = paste0("Lasso-",c(1:24)),lambda1se = numeric(),lambdamin = numeric())
MAPELasso <- vector(length = 24)

for(i in 1:24){
  x=as.matrix(lassoHourlyTrain[[i]][c(7:54)])
  y=lassoHourlyTrain[[i]]$Consumption
  Lasso = cv.glmnet(x,y,family="gaussian", nfolds=10)
  lassoHourlyTest[[i]]$LassoResult = predict(Lasso, newx = as.matrix(lassoHourlyTest[[i]][c(10:57)]))
  lambda[i]$lambda1se = Lasso$lambda.1se
  lambda[i]$lambdamin = Lasso$lambda.min
  MAPELasso[i]= sum(abs(lassoHourlyTest[[i]]$LassoResult - lassoHourlyTest[[i]]$Consumption)/lassoHourlyTest[[i]]$Consumption)*100/30
  
}

plot(MAPELasso,type = "h", xlab="Hours", ylab="MAPE")
MAPELasso

#TASK F

MAPE = data.frame(MAPE2Day = testdata$MAPE2Day*100, MAPE7Day = testdata$MAPE7Day*100, MAPEReg = testdata$MAPEReg*100, MAPEHourlyReg = MAPEHourlyReg*100, MAPELasso = MAPELasso)
summary(MAPE)
boxplot(MAPE)

#Looking at the summary values and boxplots, LASSO regresion seem to have the best results




