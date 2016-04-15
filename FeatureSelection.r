library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(git2r)
library(FNN)
library(xgboost)
load("/Users/itsaj77/Downloads/TrainTest.RData")

#Cleaning Data 
X=as.matrix(X)

#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:10000]
test=indices[10001:50000]


#Removing Highly Correlated Features 
corr_mat=cor(X[,1:1000])
high_corr=findCorrelation(corr_mat, cutoff=.5)
print(high_corr)

#John's Code... Removes Unimportant Features 
lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
columns=lasso$x
ordered_coef=order(lasso$x, decreasing=TRUE)
unimportant=ordered_coef[201:1000]


#Finding Intersection of Unimportant and Highly Correlated Features... Basically, if row 94 is both "highly correlated" and "unimportant", we don't want to remove it twice
repeats=intersect(high_corr, unimportant)
high_corr_new=high_corr[high_corr %in% unimportant==FALSE]
unimportant_new=unimportant[unimportant %in% high_corr==FALSE]

useless=union(repeats, high_corr)
useless=union(useless, unimportant)
X=X[,-c(useless)]
