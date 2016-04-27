load("/Users/itsaj77/Downloads/TrainTest.RData")

#Cleaning Data 
X=as.matrix(X)

#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:40000]
test=indices[40001:50000]


#Removing Highly Correlated Features 
corr_mat=cor(X[,1:1000])
high_corr=findCorrelation(corr_mat, cutoff=.5)
print(high_corr)

#John's Code... Removes Unimportant Features 
lasso=read.csv("/Users/itsaj77/Desktop/Classes/Stats 154/simulation_results.csv")
columns=lasso$x
ordered_coef=order(lasso$x, decreasing=TRUE)
unimportant=ordered_coef[300:1000]

#Finding Best # of Unimportant Features To Use


#Finding Intersection of Unimportant and Highly Correlated Features 
repeats=intersect(high_corr, unimportant)
high_corr_new=high_corr[high_corr %in% unimportant==FALSE]
unimportant_new=unimportant[unimportant %in% high_corr==FALSE]

useless=union(repeats, high_corr)
useless=union(useless, unimportant)
X=X[,-c(useless)]


#Random Forest

RF=xgboost(X[train,], as.factor(y[train]), nrounds=1000, eta=.2)
predRF=predict(RF, X[test,])
predRF=predRF-1
predRF=as.numeric(predRF>.5)
sum(predRF==y[test])/length(predRF)


#For Submission 
RFsub=xgboost(X[train,], as.factor(y[train]), nrounds=1000, eta=.2)
predRF=predict(RFsub, X)
predRF=predRF-1
predRF=as.numeric(predRF>.5)
sum(predRF==y)/length(predRF)

predRF=data.frame(1:50000, predRF)
colnames(predRF)=c("X", "y")

write.table(predRF, "/Users/itsaj77/Desktop/submission.csv", col.names=TRUE, row.names=FALSE, sep=",")

