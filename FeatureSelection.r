library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(git2r)
library(FNN)
library(xgboost)
library(MASS)
library(glmnet)
load("/Users/itsaj77/Downloads/TrainTest.RData")

#Cleaning Data 
X=as.matrix(X)
Xtest=as.matrix(Xtest)


#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:40000]
test=indices[40001:50000]


#Removing Highly Correlated Features 
corr_mat=cor(Xtest[,1:1000])
high_corr=findCorrelation(corr_mat, cutoff=.5)
print(high_corr)

#John's Code... Removes Unimportant Features 
lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
columns=lasso$x
ordered_coef=order(lasso$x, decreasing=TRUE)
unimportant_RF=ordered_coef[640:1000]
unimportant_log=ordered_coef[245:1000]




#Finding Intersection of Unimportant and Highly Correlated Features for RF
repeats=intersect(high_corr, unimportant_RF)
high_corr_new=high_corr[high_corr %in% unimportant_RF==FALSE]
unimportant_newRF=unimportant_RF[unimportant_RF %in% high_corr==FALSE]

useless=union(repeats, high_corr)
useless=union(useless, unimportant_newRF)
X_RF=X[,-c(useless)]
Xtest_RF=Xtest[, -c(useless)]



#Finding Intersection of Unimportant and Highly Correlated Features for Logistic
repeats=intersect(high_corr, unimportant_log)
high_corr_new=high_corr[high_corr %in% unimportant_log==FALSE]
unimportant_newlog=unimportant_log[unimportant_log %in% high_corr==FALSE]

useless=union(repeats, high_corr_new)
useless=union(useless, unimportant_newlog)
X_log=X[,-c(useless)]
Xtest_log=Xtest[, -c(useless)]

