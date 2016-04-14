# data
load("TrainTest.RData")
data = as.data.frame(cbind(as.matrix(X), y))

index = sample(1:50000, 1000)
train = index[1:500]
test = index[501:1000]

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

### Gradient Boosting 
library(gbm)

gbm_model = gbm.fit(x = data[train,1:1000], 
                    y = as.factor(data$y[train]), 
                    n.trees = 20, 
                    distribution = "multinomial", 
                    verbose = TRUE, shrinkage = 0.6)

yhat_gbm = predict(gbm_model, data[test,], 
                   n.trees = 10, type = "response")[,,1]
# logistic 
# log sparse
library(glmnet)
LogSparse <- cv.glmnet(as.matrix(prostateData[index, 1:6033]),
                               as.matrix(prostateData$y[index]), 
                               family = "binomial")

# predict responsese based on regression
log.sparse_prediction <- predict(prostateLogSparse, as.matrix(prostateData[-index, 1:6033]), lambda = "lambda.1se", type = "response")

# take only predictions with more than .5 probability
log.sparse_prediction2 <-(log.sparse_prediction > .5)

# accuracy through proportion
sum(log.sparse_prediction2 == prostateData$y[-index])/length(prostateData$y[-index])

