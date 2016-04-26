### Find optimal number of predictors using logistic regression

### Will use ordered predictors from LASSO predictor selection
setwd('Desktop/School/Spring 2016/Stat 154/Final Project/')

load('Data/TrainTest.RData')

### Clean data
X <- as.matrix(X)


### load predictor results (could use AJ's feature selection as well)
pred_lasso <- as.data.frame(read.csv('LASSO_resultSorted.csv'))

predictors <- pred_lasso$Word
rm(pred_lasso)

### Use ordered predictors to reorder X matrix accordingly
X_ordered <- X[,predictors]
Xtest_ordered <- Xtest[,predictors]
rm(X)
rm(predictors)

data_ordered <- data.frame(y,X_ordered)
rm(X_ordered)

### Run simulation that calculated MSE as function of number of pred
### used in regression

### Fucntion for extracting data w/ desired # of columns

sim_data_fun <- function(data) {
  results <- list()
  sequence <- seq(from=10, to=1000, by=10)
  for(i in 1:100) {
    results[[i]] <- data[,1:(sequence[i]+1)]
  }
  return(results)
}


### Create sim data
sample_data <- sample(1:50000, 10000)
training <- sample(10000, 8000)

sim_data <- sim_data_fun(data_ordered[sample_data,])
sim_data_raw <- sim_data_fun(data_ordered)


### Create function to calculate mse on each logistic regression


mse <- function(yhat, y) {
  result_mse <- (sum((yhat-y)^2))/length(y)
  return(result_mse)
}




sim_model_log <- function(data_input, train_index) {
  model = glm(y ~., data=data_input[train_index,], family='binomial')
  pred = as.numeric(predict(model, newdata=data_input[-train_index,], type='response') >= .5)
  actual = data_input[-train_index,1]
  mse_result = mse(pred,actual)
  return(mse_result)
}

### RF function
library(ranger)

sim_model_rf <- function(data_input, train_index) {
  n = ncol(data_input)
  model = ranger(y ~ ., data=data_input[train_index,], num.tree=100, 
                 write.forest=T, classification=T)
  pred = predict(model, data = data_input[-train_index,])
  actual = data_input[-train_index,1]
  mse_result = mse(pred$predictions, actual)
  return(mse_result)
}

sim_model_rf(data_input = sim_data[[4]], train_index = training)




### Form models
sim_models_log <- lapply(sim_data, function(x) sim_model_log(x, train_index=training))
sim_models_rf <-  lapply(sim_data, function(x) sim_model_rf(x, train_index=training))
sim_models
plot(unlist(sim_models_log),x=seq(from=10, to=500, by=10), type='l', main='MSE of Logistic Simulations (Sample Size 10,000)', 
     ylab='MSE', xlab='Number of Predictors Used')

plot(unlist(sim_models_rf),x=seq(from=10, to=1000, by=10), type='l', main='MSE of RF Simulations (Sample Size 10,000)', 
     ylab='MSE', xlab='Number of Predictors Used')

# So far I'm at 246
bigTrain <- sample(1:50000, 40000)

opt_logistic_modelTrain <- glm(y~., data=data_ordered[bigTrain,1:400], family='binomial')
predictions_Big_log <- as.numeric(predict(opt_logistic_modelTrain, newdata=data_ordered[-bigTrain,1:400], type='response')>=.5)
actual_big <- data_ordered[-bigTrain,1]

opt_RF_modelTrain <- ranger(y~., data= data_ordered[bigTrain,1:600], num.tree=100, 
                            write.forest=T, classification=T)
predictions_Big_RF <- (predict(opt_RF_modelTrain, data=data_ordered[-bigTrain,1:600]))$predictions

accuracy_log <- 1 - mse(actual_big, predictions_Big_log)
accuracy_RF <- 1 - mse(actual_big, predictions_Big_RF)

opt_logistic_model <- glm(y~., data=data_ordered[,1:400], family='binomial')
final_predictions_log <- as.numeric(predict(opt_logistic_modelTrain, newdata=data.frame(y, Xtest_ordered), type='response')>=.5)

final_predDF <- data.frame(1:50000, final_predictions_log)
colnames(final_predDF) <- c('id', 'y')
View(final_predDF)











write.csv(final_predDF, file='submission_logistic.csv', row.names=F)

sum(final_predictions_log == 1)/length(final_predictions_log)
















