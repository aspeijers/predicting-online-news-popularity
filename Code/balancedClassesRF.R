library(caret)
library("pROC")
library("AppliedPredictiveModeling")

## Simulate data sets with a small event rate
set.seed(1)
training <- twoClassSim(500, intercept = -13)
testing <- twoClassSim(5000, intercept = -13)

table(training$Class)

nmin <- sum(training$Class == "Class2")
nmin

ctrl <- trainControl(method = "cv", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

set.seed(2)
rfDownsampled <- train(Class ~ ., data = training,
                       method = "rf",
                       ntree = 1500,
                       tuneLength = 5,
                       metric = "ROC",
                       trControl = ctrl,
                        ## Tell randomForest to sample by strata. Here, 
                        ## that means within each class
                        strata = training$Class,
                        ## Now specify that the number of samples selected
                        ## within each class should be the same
                        sampsize = rep(nmin, 2))

set.seed(2)
rfUnbalanced <- train(Class ~ ., data = training,
                        method = "rf",
                        ntree = 1500,
                        tuneLength = 5,
                        metric = "ROC",
                        trControl = ctrl)

#ROC curves
downProbs <- predict(rfDownsampled, testing, type = "prob")[,1]
downsampledROC <- roc(response = testing$Class, 
                      predictor = downProbs,
                      levels = rev(levels(testing$Class)))

unbalProbs <- predict(rfUnbalanced, testing, type = "prob")[,1]
unbalROC <- roc(response = testing$Class, 
                  predictor = unbalProbs,
                  levels = rev(levels(testing$Class)))

plot(downsampledROC, col = rgb(1, 0, 0, .5), lwd = 2)
plot(unbalROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
legend(.4, .4,
       c("Down-Sampled", "Normal"),
       lwd = rep(2, 1), 
       col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))