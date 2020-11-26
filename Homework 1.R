library(ggplot2)

EPL2021 <- read.csv("C:/Users/HP/Downloads/E0 (1).csv", header=TRUE) #2020-2021 data
EPL1920 <- read.csv("C:/Users/HP/Downloads/E0.csv", header=TRUE)  #2019-2020 data
EPL1819 <- read.csv("C:/Users/HP/Downloads/E0 (2).csv", header=TRUE) #2018-2019 data


hist(EPL1920$FTHG, xlab = 'Home Goals', ylab = 'Number of Games', ylim=c(0,500))

#TASK 1
#1a
homegoals = c(EPL2021$FTHG,EPL1920$FTHG,EPL1819$FTHG);
hist(homegoals, xlab = 'Home Goals', ylab = 'Number of Games', right = F) ;
awaygoals = c(EPL2021$FTAG,EPL1920$FTAG,EPL1819$FTAG)
hist(awaygoals, xlab = 'Away Goals', ylab = 'Number of Games', right = F)
homeaway = c(V1=( EPL2021$F4THG-EPL2021$FTAG), V2= (EPL1920$FTHG-EPL1920$FTAG), V3= (EPL1819$FTHG-EPL1819$FTAG))
hist(homeaway, xlab = 'Home Goals - Away Goals', ylab = 'Number of Games', right = F)

#1b   
x1 = dpois(c(0:5), mean(homegoals))
expectedNoOfMatchesHome= data.frame(0:5, round(838*x1))
table(homegoals)
# homegoals
# 0   1   2   3   4   5   6   7   8 
# 196 264 209 101  46  17   3   1   1 
expectedNoOfMatchesHome
# X0.5 round.838...x1.
# 1    0             180
# 2    1             277
# 3    2             213
# 4    3             109
# 5    4              42
# 6    5              13
hist(homegoals, xlab = 'Home Goals', ylab = 'Number of Games', right = F)
lines(expectedNoOfMatchesHome)

x2 = dpois(c(0:5), mean(awaygoals))
expectedNoOfMatchesAway = data.frame(0:5, round(838*x2))
table(awaygoals)
# awaygoals
# 0   1   2   3   4   5   6   9 
# 258 278 179  87  20  12   3   1 
expectedNoOfMatchesAway
# X0.5 round.838...x2.
# 1    0             235
# 2    1             299
# 3    2             190
# 4    3              80
# 5    4              25
# 6    5               6
hist(awaygoals, xlab = 'Away Goals', ylab = 'Number of Games', right = F)
lines(expectedNoOfMatchesAway)


#Comments for Task 1: From part b of the question, by comparing theoretical dist and outcomes, we can say that they are very similar. 
# As asked in the question, we took lambda = mean of the samples hence the plots are drawn accordingly. We can say that they have very similar distributions
# hence, we can say our sample data's distribution is consistent with Poisson dist. claim.

# TASK 2
#2a
avgHomeOdds = c(EPL2021$B365H, EPL1920$B365H, EPL1819$B365H, EPL2021$BWH, EPL1920$BWH, EPL1819$BWH, EPL2021$IWH, EPL1920$IWH, EPL1819$IWH,EPL2021$PSH, EPL1920$PSH, EPL1819$PSH)
pAvgHome = 1/avgHomeOdds
mean(pAvgHome) #P(Home Win) = 0.4600872
avgTieOdds = c(EPL2021$B365D, EPL1920$B365D, EPL1819$B365D, EPL2021$BWD, EPL1920$BWD, EPL1819$BWD, EPL2021$IWD, EPL1920$IWD, EPL1819$IWD,EPL2021$PSD, EPL1920$PSD, EPL1819$PSD)
pAvgTie = 1/avgTieOdds
mean(pAvgTie)  #P(Tie) =  0.2501311
avgAwayOdds = c(EPL2021$B365A, EPL1920$B365A, EPL1819$B365A, EPL2021$BWA, EPL1920$BWA, EPL1819$BWA, EPL2021$IWA, EPL1920$IWA, EPL1819$IWA,EPL2021$PSA, EPL1920$PSA, EPL1819$PSA)
pAvgAway = 1/avgAwayOdds
mean(pAvgAway)  #P(Away Win) = 0.3328127

#2b
#For this question our pAvgHome, pAvgTie and pAvgAway values are the 1/Odds(i) values. Hence, first we need to sum them and then take the 1/sum

totalInverseOdds = pAvgHome+pAvgTie+pAvgAway
totalOdds = 1/totalInverseOdds

pHomeNormal = pAvgHome*totalOdds
mean(pHomeNormal) #0.4410851
pTieNormal = pAvgTie*totalOdds
mean(pTieNormal) #0.2398378
pAwayNormal = pAvgAway*totalOdds
mean(pAwayNormal) #0.3190771
# As expected, normalized values are very similar to the probabilities found in the first question.

#2c
pHomeAway = pAvgHome - pAvgAway

data = cbind(pHomeAway,pAvgTie)
df=data.frame(data)
ggplot(df, aes(x=pHomeAway, y=pAvgTie)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 
qplot(x=pHomeAway, y=pAvgTie, data = df)

#2d
homeOddsB365 =c(EPL2021$B365H, EPL1920$B365H, EPL1819$B365H)
awayOddsB365 =c(EPL2021$B365A, EPL1920$B365A, EPL1819$B365A)
drawOddsB365 =c(EPL2021$B365D, EPL1920$B365D, EPL1819$B365D)
dfB365 = data.frame(cbind((1/homeOddsB365)-(1/awayOddsB365), 1/drawOddsB365))
ggplot(dfB365, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsBW =c(EPL2021$BWH, EPL1920$BWH, EPL1819$BWH)
awayOddsBW =c(EPL2021$BWA, EPL1920$BWA, EPL1819$BWA)
drawOddsBW =c(EPL2021$BWD, EPL1920$BWD, EPL1819$BWD)
dfBW = data.frame(cbind((1/homeOddsBW)-(1/awayOddsBW), 1/drawOddsBW))
ggplot(dfBW, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsIW =c(EPL2021$IWH, EPL1920$IWH, EPL1819$IWH)
awayOddsIW =c(EPL2021$IWA, EPL1920$IWA, EPL1819$IWA)
drawOddsIW =c(EPL2021$IWD, EPL1920$IWD, EPL1819$IWD)
dfIW = data.frame(cbind((1/homeOddsIW)-(1/awayOddsIW), 1/drawOddsIW))
ggplot(dfIW, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsPS =c(EPL2021$PSH, EPL1920$PSH, EPL1819$PSH)
awayOddsPS =c(EPL2021$PSA, EPL1920$PSA, EPL1819$PSA)
drawOddsPS =c(EPL2021$PSD, EPL1920$PSD, EPL1819$PSD)
dfPS = data.frame(cbind((1/homeOddsPS)-(1/awayOddsPS), 1/drawOddsPS))
ggplot(dfPS, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

#I tried to bin the real results and visually represent them in the solutions but unfortunately I could not perform it. However, by reading the paper
#suggested in the homework, I undertood that an odds bias occurs since the odds probability varies from the real probabilities, hence the trend line
#changes and our data become biased.

#TASK 3

EPL1819NoNoise =EPL1819[!(EPL1819$HR== 1 | EPL1819$HR==2 | EPL1819$AR==1 | EPL1819$AR==2),]
EPL1920NoNoise =EPL1920[!(EPL1920$HR== 1 | EPL1920$HR==2 | EPL1920$AR==1 | EPL1920$AR==2),]
EPL2021NoNoise = EPL1920[!(EPL1920$HR== 1 | EPL1920$HR==2 | EPL1920$AR==1 | EPL1920$AR==2),]

avgHomeOddsNoNoise = c(EPL2021NoNoise$B365H, EPL1920NoNoise$B365H, EPL1819NoNoise$B365H, EPL2021NoNoise$BWH, EPL1920NoNoise$BWH, EPL1819NoNoise$BWH, EPL2021NoNoise$IWH, EPL1920NoNoise$IWH, EPL1819NoNoise$IWH,EPL2021NoNoise$PSH, EPL1920NoNoise$PSH, EPL1819NoNoise$PSH)
pAvgHomeNoNoise = 1/avgHomeOddsNoNoise
mean(pAvgHomeNoNoise) 
avgTieOddsNoNoise = c(EPL2021NoNoise$B365D, EPL1920NoNoise$B365D, EPL1819NoNoise$B365D, EPL2021NoNoise$BWD, EPL1920NoNoise$BWD, EPL1819NoNoise$BWD, EPL2021NoNoise$IWD, EPL1920NoNoise$IWD, EPL1819NoNoise$IWD,EPL2021NoNoise$PSD, EPL1920NoNoise$PSD, EPL1819NoNoise$PSD)
pAvgTieNoNoise = 1/avgTieOddsNoNoise
mean(pAvgTieNoNoise)  
avgAwayOddsNoNoise = c(EPL2021NoNoise$B365A, EPL1920NoNoise$B365A, EPL1819NoNoise$B365A, EPL2021NoNoise$BWA, EPL1920NoNoise$BWA, EPL1819NoNoise$BWA, EPL2021NoNoise$IWA, EPL1920NoNoise$IWA, EPL1819NoNoise$IWA,EPL2021NoNoise$PSA, EPL1920NoNoise$PSA, EPL1819NoNoise$PSA)
pAvgAwayNoNoise = 1/avgAwayOddsNoNoise
mean(pAvgAwayNoNoise)  
pHomeAwayNoNoise = pAvgHomeNoNoise - pAvgAwayNoNoise
dataNoNoise = cbind(pHomeAwayNoNoise,pAvgTieNoNoise)
dfNoNoise=data.frame(dataNoNoise)
ggplot(dfNoNoise, aes(x=pHomeAwayNoNoise, y=pAvgTieNoNoise)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 


homeOddsB365NoNoise =c(EPL2021NoNoise$B365H, EPL1920NoNoise$B365H, EPL1819NoNoise$B365H)
awayOddsB365NoNoise =c(EPL2021NoNoise$B365A, EPL1920NoNoise$B365A, EPL1819NoNoise$B365A)
drawOddsB365NoNoise =c(EPL2021NoNoise$B365D, EPL1920NoNoise$B365D, EPL1819NoNoise$B365D)
dfB365NoNoise = data.frame(cbind((1/homeOddsB365NoNoise)-(1/awayOddsB365NoNoise), 1/drawOddsB365NoNoise))
ggplot(dfB365NoNoise, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsBWNoNoise =c(EPL2021NoNoise$BWH, EPL1920NoNoise$BWH, EPL1819NoNoise$BWH)
awayOddsBWNoNoise =c(EPL2021NoNoise$BWA, EPL1920NoNoise$BWA, EPL1819NoNoise$BWA)
drawOddsBWNoNoise =c(EPL2021NoNoise$BWD, EPL1920NoNoise$BWD, EPL1819NoNoise$BWD)
dfBWNoNoise = data.frame(cbind((1/homeOddsBWNoNoise)-(1/awayOddsBWNoNoise), 1/drawOddsBWNoNoise))
ggplot(dfBWNoNoise, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsIWNoNoise =c(EPL2021NoNoise$IWH, EPL1920NoNoise$IWH, EPL1819NoNoise$IWH)
awayOddsIWNoNoise =c(EPL2021NoNoise$IWA, EPL1920NoNoise$IWA, EPL1819NoNoise$IWA)
drawOddsIWNoNoise =c(EPL2021NoNoise$IWD, EPL1920NoNoise$IWD, EPL1819NoNoise$IWD)
dfIWNoNoise = data.frame(cbind((1/homeOddsIWNoNoise)-(1/awayOddsIWNoNoise), 1/drawOddsIWNoNoise))
ggplot(dfIWNoNoise, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 

homeOddsPSNoNoise =c(EPL1819NoNoise$PSH, EPL1920NoNoise$PSH, EPL2021NoNoise$PSH)
awayOddsPSNoNoise =c(EPL1819NoNoise$PSA, EPL1920NoNoise$PSA, EPL2021NoNoise$PSA)
drawOddsPSNoNoise =c(EPL1819NoNoise$PSD, EPL1920NoNoise$PSD, EPL2021NoNoise$PSD)
dfPSNoNoise = data.frame(cbind((1/homeOddsPSNoNoise)-(1/awayOddsPSNoNoise), 1/drawOddsPSNoNoise))
ggplot(dfPSNoNoise, aes(x=X1, y=X2)) + geom_point() + scale_x_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) 


