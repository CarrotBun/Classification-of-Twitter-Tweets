library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(git2r)
library(FNN)
library(xgboost)
library(MASS)
library(e1071)
library(glmnet)
library(h2o)
load("/Users/itsaj77/Downloads/TrainTest.RData")

#Cleaning Data 
X=as.matrix(X)
Xtest=as.matrix(Xtest)
Xtest=apply(Xtest, FUN=as.numeric, c(1,2))

#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:40000]
test=indices[40001:50000]

#Removing Highly Correlated Features 
corr_mat=cor(Xtest[,1:1000])
high_corr=findCorrelation(corr_mat, cutoff=1)

accuracy_varlog=vector()
for (i in 1:10){
  high_corr=findCorrelation(corr_mat, cutoff=.1*i)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_log=ordered_coef[350:1000]
  repeats=intersect(high_corr, unimportant_log)
  high_corr_new=high_corr[high_corr %in% unimportant_log==FALSE]
  unimportant_newlog=unimportant_log[unimportant_log %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newlog)
  X_log=X[,-c(useless)]
  datalog=data.frame(X_log,y)
  datalog2=as.data.frame(X_log)
  colnames(datalog2)=colnames(datalog)[1:(length(datalog)-1)]
  Log=glm(y~. , family=binomial, data=datalog[train,])
  problog=predict(Log, newdata=datalog2[test,] ,type="response")
  predlog=as.numeric(problog>.5)
  accuracy_varlog[i]=sum(predlog==y[test])/length(predlog)
}


accuracy_corlog=vector()
for (i in 1:10){
  high_corr=findCorrelation(corr_mat, cutoff=.1*i)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_log=ordered_coef[350:1000]
  repeats=intersect(high_corr, unimportant_log)
  high_corr_new=high_corr[high_corr %in% unimportant_log==FALSE]
  unimportant_newlog=unimportant_log[unimportant_log %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newlog)
  X_log=X[,-c(useless)]
  datalog=data.frame(X_log,y)
  datalog2=as.data.frame(X_log)
  colnames(datalog2)=colnames(datalog)[1:(length(datalog)-1)]
  Log=glm(y~. , family=binomial, data=datalog[train,])
  problog=predict(Log, newdata=datalog2[test,] ,type="response")
  predlog=as.numeric(problog>.5)
  accuracy_corlog[i]=sum(predlog==y[test])/length(predlog)
}
