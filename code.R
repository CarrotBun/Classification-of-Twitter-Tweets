# data
load("TrainTest.RData")

# random forest
library(randomForest)

rf_model = randomForest(x = data[train,1:1000], 
                        y = as.factor(data$y[train]),
                        ntree = 20, 
                        do.trace = FALSE)

yhat_rf = predict(rf_model, data[test, ])

sum(yhat_rf == data$y[test])/length(data$y[test])

# kaggle data scoring
yhat_rf_xtest = predict(rf_model, Xtest)

### Gradient Boosting ## unfinished
library(gbm)

gbm_model = gbm.fit(x = data[train,1:1000], 
                    y = as.factor(data$y[train]), 
                    n.trees = 20, 
                    distribution = "multinomial", 
                    verbose = TRUE, shrinkage = 0.6)

yhat_gbm = predict(gbm_model, data[test,], 
                   n.trees = 10, type = "response")[,,1]
# xg boost ## unfinished
library(xgboost)
t1 = proc.time()
rf_model2 = xgboost(data = X[train,], label = y[train], nrounds = 1, verbose = 2, 
                    max.depth = 10, num_parallel_tree = 20, 
                    subsample = 1., colsample_bytree = .04, 
                    objective = "multi:softmax", num_class = 10)
t2 = proc.time()
yhat = predict(rf_model2, X[valid,])
accuracy_score(y[valid], yhat)




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