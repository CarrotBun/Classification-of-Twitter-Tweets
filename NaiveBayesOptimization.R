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

#John's Code... Removes Unimportant Features 
lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
columns=lasso$x
ordered_coef=order(lasso$x, decreasing=TRUE)

#Optimizing Naive Bayes Number of Variables to Use 
accuracy_vnb=vector()
for (i in 1:20){
  high_corr=findCorrelation(corr_mat, cutoff=.6)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_NB=ordered_coef[i*50:1000]
  repeats=intersect(high_corr, unimportant_NB)
  high_corr_new=high_corr[high_corr %in% unimportant_NB==FALSE]
  unimportant_newNB=unimportant_NB[unimportant_NB %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newNB)
  X_NB=X[,-c(useless)]
  Xtest_NB=Xtest[,-c(useless)]
  NBsub1=naiveBayes(X_NB[train,], as.factor(y[train]))
  predNB1=predict(NBsub1, X_NB[test,])
  accuracy_vnb[i]=sum(predNB1==y[test])/length(predNB1)   
}

#Optimizing Correlation Threshhold 
accuracy_vnb=vector()
for (i in 1:10){
  high_corr=findCorrelation(corr_mat, cutoff=i*.1)
  lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
  columns=lasso$x
  ordered_coef=order(lasso$x, decreasing=TRUE)
  unimportant_NB=ordered_coef[i*50:1000]
  repeats=intersect(high_corr, unimportant_NB)
  high_corr_new=high_corr[high_corr %in% unimportant_NB==FALSE]
  unimportant_newNB=unimportant_NB[unimportant_NB %in% high_corr==FALSE]
  useless=union(repeats, high_corr_new)
  useless=union(useless, unimportant_newNB)
  X_NB=X[,-c(useless)]
  Xtest_NB=Xtest[,-c(useless)]
  NBsub1=naiveBayes(X_NB[train,], as.factor(y[train]))
  predNB1=predict(NBsub1, X_NB[test,])
  accuracy_vnb[i]=sum(predNB1==y[test])/length(predNB1)   
}
