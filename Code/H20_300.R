################################################
# 300 features pruning with GBM ################
################################################

training_300 <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/training_300.csv", header = TRUE)
test_300 <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/test_300.csv", header = TRUE)

library(h2o)
h2o.init(nthreads = -1)


# Converting the dataframe to h2o training frames
Complete_gbm_tr <- as.h2o(training_300[,-1], destination_frame = "Complete_gbm_tr")
Complete_gbm_te <- as.h2o(test_300[,-1], destination_frame = "Complete_gbm_te")

# Ensuring that the popularity is a factor
Complete_gbm_tr[,301] <- as.factor(Complete_gbm_tr[,301])
Complete_gbm_te[,301] <- as.factor(Complete_gbm_te[,301])

### GBM
training.gbm <- h2o.gbm(y=301, x=1:300, training_frame= Complete_gbm_tr, ntrees=1000,
                        max_depth=5, min_rows= 20, learn_rate=0.05,
                        distribution="multinomial", nbins=1024)

# Storing the predictions
prediction <- h2o.predict(training.gbm, newdata=Complete_gbm_te)
pred <- as.data.frame(prediction)

# Checking the accuracy
percent_correct <- (sum(pred[,1] == test_300[,302])) / nrow(test_300)
percent_correct


# Storing the variable importance
final_selection <- h2o.varimp(training.gbm)

# Plotting the variable importance
plot(final_selection[,2])
sum(final_selection[,2] > mean(final_selection[,2]))


################################################
# 49 features with GBM optimum parameters ######
################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/xgboost_train.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/xgboost_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete49_gbm_tr <- as.h2o(xgboost_train[,-1], destination_frame = "Complete49_gbm_tr")
Complete49_gbm_te <- as.h2o(xgboost_test[,-1], destination_frame = "Complete49_gbm_te")

# Ensuring that the popularity is a factor
Complete49_gbm_tr[,1] <- as.factor(Complete49_gbm_tr[,1])
Complete49_gbm_te[,1] <- as.factor(Complete49_gbm_te[,1])

### GBM
training49.gbm <- h2o.gbm(y=1, x=2:50, training_frame= Complete49_gbm_tr, ntrees=285,
                        max_depth=6, min_rows= 20, learn_rate=0.01,
                        distribution="multinomial", nbins=1024)

# Storing the predictions
prediction49 <- h2o.predict(training49.gbm, newdata=Complete49_gbm_te)
pred49 <- as.data.frame(prediction49)

# Checking the accuracy
percent_correct <- (sum(pred49[,1] == xgboost_test[,2])) / nrow(xgboost_test)
percent_correct


# Storing the variable importance
final_selection <- h2o.varimp(training49.gbm)

# Plotting the variable importance
plot(final_selection[,2])
sum(final_selection[,2] > mean(final_selection[,2]))
