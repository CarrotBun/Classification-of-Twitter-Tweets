library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(git2r)
load("/Users/itsaj77/Downloads/TrainTest.RData")

################################################################
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


################################################################
#Random Forest
RF=randomForest(X[train,], as.factor(y[train]), ntrees=100, mtry=8)
predRF=predict(RF, X[test,], type="class")
sum(predRF==y[test])/length(predRF)

#74% with 1000 trees and 10000 training

################################################################
#Naive Bayes
library(e1071)
nBayes_model = naiveBayes(x = X[train, c(sim_indices[1:100, 2])], y = as.factor(y[train]))
# prediction
nBayes_prediction <- predict(nBayes_model, 
                             newdata = X[test,c(sim_indices[1:100, 2])]) 
# accuracy
sum(nBayes_prediction == y[test])/length(y[test])
# 57.6%

nBayes_model2 = naiveBayes(x = X[train, ], y = as.factor(y[train]))
# prediction
nBayes_prediction2 <- predict(nBayes_model2, 
                              newdata = X[test, ]) 
# accuracy
sum(nBayes_prediction2 == y[test])/length(y[test])
# 52%



#######################################
# logistic 
library(glmnet)
log_model <- cv.glmnet(X[train, c(sim_indices[1:100, 2])], 
                       y[train], 
                       family = "binomial")

# prediction
log_prediction <- predict(log_model, 
                          newx = X[test,c(sim_indices[1:100, 2])],
                          type="response") 
log_predictions_glm <-(log_prediction > .5)

# accuracy
sum(log_predictions_glm == y[test])/length(y[test])


#######################################
# logistic 2
log_model2 <- cv.glmnet(X[train, c(sim_indices[1:200, 2])], 
                        y[train], 
                        family = "binomial")

# prediction
log_prediction2 <- predict(log_model2, 
                           newx = X[test,c(sim_indices[1:200, 2])],
                           type="response") 
log_predictions_glm2 <-(log_prediction2 > .5)

# accuracy
sum(log_predictions_glm2 == y[test])/length(y[test])

#################################
# function to run logistic models with varying amount of predictors
run_log = function(index){
  # log model
  predictors = c(sim_indices[1:index, 2])
  log_model <- cv.glmnet(X[train, predictors], 
                         y[train], 
                         family = "binomial")
  
  # prediction
  log_prediction <- predict(log_model, 
                            newx = X[test, predictors],
                            type="response") 
  log_predictions_glm <-(log_prediction > .5)
  
  # accuracy
  accuracy = sum(log_predictions_glm == y[test])/length(y[test])
  return(accuracy)
}
###################
# manually CV
run_log(50) #0.596
run_log(100) #0.604
run_log(500) #0.572
run_log(800) #0.572

vec = seq(100,1000,100)
output = vector()
for(i in 1:length(vec)){
  num = vec[i]
  output[i] = run_log(num)
}

plot(vec2, output2, type = "l", 
     main = "# of Predictors vs. Acccuracy Rate for Logistic Regression", 
     xlab = "# of predictors",
     ylab = "accuracy")


vec2 = seq(200,250,1)
output2 = vector()
for(i in 1:length(vec2)){
  num = vec2[i]
  output2[i] = run_log(num)
}


################################################################
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

################################################################
