# data
load("TrainTest.RData")
##########################
# random forest
library(randomForest)

rf_model = randomForest(x = data[train,1:1000], 
                        y = as.factor(data$y[train]),
                        ntree = 20, 
                        do.trace = FALSE)

yhat_rf = predict(rf_model, data[test, ])

sum(yhat_rf == data$y[test])/length(data$y[test])

############################################
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
#function to run log regression #
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

# using functions

run_log(50) #0.596
run_log(100) #0.604
run_log(500) #0.572
run_log(800) #0.572


# plot of logistic predictors
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

##################################################
# Naive Bayes
library(e1071)
nBayes_model = naiveBayes(x = X[train, c(sim_indices[1:100, 2])], y = as.factor(y[train]))
nBayes_prediction <- predict(nBayes_model, 
                           newdata = X[test,c(sim_indices[1:100, 2])]) 
# accuracy
sum(nBayes_prediction == y[test])/length(y[test])

nBayes_model2 = naiveBayes(x = X[train, ], y = as.factor(y[train]))
nBayes_prediction2 <- predict(nBayes_model2, 
                             newdata = X[test, ]) 
# accuracy
sum(nBayes_prediction2 == y[test])/length(y[test])


#######################################
library(caret)
library(ggplot2)
control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
# train the SVM model
SVM_model = train(as.factor(y[train])~X[train, c(sim_indices[1:100, 2])], 
                  data = cbind(X[train, c(sim_indices[1:100, 2])], y[train]),
                  method = "svmRadial",
                  trControl = control)
GBM_model = train(as.factor(y[train])~X[train, c(sim_indices[1:100, 2])], 
                  data = cbind(X[train, c(sim_indices[1:100, 2])], y[train]),
                  method = "gbm",
                  trControl = control,
                  verbose = FALSE)

NB_model = train(as.factor(y[train])~X[train, c(sim_indices[1:100, 2])], 
                  data = cbind(X[train, c(sim_indices[1:100, 2])], y[train]),
                  method = "nbDiscrete",
                  trControl = control)
bwplot(resamples(list(GBM = GBM_model, SVM = SVM_model)))
