library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(git2r)
load("/Users/itsaj77/Downloads/TrainTest.RData")

#Cleaning Data 
X=as.matrix(X)

#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:2500]
test=indices[2501:3500]

#Removing Highly Correlated Features 
corr_mat=cor(X[,1:1000])
high_corr=findCorrelation(corr_mat, cutoff=.5)
print(high_corr)
X=X[, -c(high_corr)]

#Random Forest
RF=randomForest(X[train,], as.factor(y[train]), ntrees=100, mtry=8)
predRF=predict(RF, X[test,], type="class")
sum(predRF==y[test])/length(predRF)

#74% with 1000 trees and 10000 training


#Naive Bayes

#Estimating Variable Importance 

find_corr=function(matrix, y){
  v=vector()
  for(x in 1:ncol(matrix)){
    v[x]=cor(matrix[,x], y)
  }
  return(v)
}

correlations=find_corr(X,y)
correlations=order(correlations, decreasing=TRUE)
low_corr=correlations[900:973]
X=X[,-c(low_corr)]

