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

#Cleaning Data and putting them into usable types 
X=as.matrix(X)
Xtest=as.matrix(Xtest)
Xtest=apply(Xtest, FUN=as.numeric, c(1,2))

#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:40000]
test=indices[40001:50000]


#Creating a correlation matrix between columns/variables in X 
corr_mat=cor(Xtest[,1:1000])

#Optimizing number of coefficients for GB
accuracy_varb=vector()
for (i in 1:20){
  high_corr=findCorrelation(corr_mat, cutoff=.5)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_GB=ordered_coef[620:1000]
  repeats=intersect(high_corr, unimportant_GB)
  high_corr_new=high_corr[high_corr %in% unimportant_GB==FALSE]
  unimportant_newGB=unimportant_GB[unimportant_GB %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newGB)
  useless=useless[1:(length(useless)-1)]
  X_GB=X[,-c(useless)]
  Xtest_GB=Xtest[,-c(useless)]
  GBsub1=xgboost(X_GB[train,], as.factor(y[train]), nrounds=450, eta=.2)
  probGB1=predict(GBsub1, X_GB[test,])
  probGB1=(probGB1-min(probGB1))/(max(probGB1)-min(probGB1))
  predGB1=as.numeric(probGB1>.5)
  accuracy_varb[i]=sum(predGB1==y[test])/length(predGB1)   
}


#Optimizing correlation threshhold for GB
accuracy_corr=vector()
for (i in 1:20){
  high_corr=findCorrelation(corr_mat, cutoff=.35)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_GB=ordered_coef[620:1000]
  repeats=intersect(high_corr, unimportant_GB)
  high_corr_new=high_corr[high_corr %in% unimportant_GB==FALSE]
  unimportant_newGB=unimportant_GB[unimportant_GB %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newGB)
  useless=useless[1:(length(useless)-1)]
  X_GB=X[,-c(useless)]
  Xtest_GB=Xtest[,-c(useless)]
  GBsub1=xgboost(X_GB[train,], as.factor(y[train]), nrounds=450, eta=.2)
  probGB1=predict(GBsub1, X_GB[test,])
  probGB1=(probGB1-min(probGB1))/(max(probGB1)-min(probGB1))
  predGB1=as.numeric(probGB1>.5)
  accuracy_corr[i]=sum(predGB1==y[test])/length(predGB1)   
}

