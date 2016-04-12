### Final Project Code

setwd('Desktop/School/Spring 2016/Stat 154/Final Project/')

### Load R dataset
load('Data/TrainTest.RData')

Xtrain <- as.matrix(X)

Xtrain[1,]
X[1,]

sum(X != Xtrain)
