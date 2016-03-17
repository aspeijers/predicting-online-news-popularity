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
training49.gbm <- h2o.gbm(y=1, x=2:50, training_frame = Complete49_gbm_tr, ntrees=626,
                        max_depth=6, min_rows= 14, learn_rate=0.01,
                        distribution="multinomial")

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


################################################
# 51 features with GBM optimum parameters ######
################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/final_ftrs_train.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/final_ftrs_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete51_gbm_tr <- as.h2o(xgboost_train[,-1], destination_frame = "Complete51_gbm_tr")
Complete51_gbm_te <- as.h2o(xgboost_test[,-1], destination_frame = "Complete51_gbm_te")

# Ensuring that the popularity is a factor
Complete51_gbm_tr[,51] <- as.factor(Complete51_gbm_tr[,51])
Complete51_gbm_te[,51] <- as.factor(Complete51_gbm_te[,51])

### GBM
training51.gbm <- h2o.gbm(y=51, x=1:50, training_frame = Complete51_gbm_tr, ntrees=350,
                          max_depth=5, min_rows= 15, learn_rate=0.001,
                          distribution="multinomial")

# Storing the predictions
prediction51 <- h2o.predict(training51.gbm, newdata=Complete51_gbm_te)
pred51 <- as.data.frame(prediction51)

# Checking the accuracy
percent_correct <- (sum(pred51[,1] == xgboost_test[,52])) / nrow(xgboost_test)
percent_correct


# Storing the variable importance
final_selection <- h2o.varimp(training51.gbm)

# Plotting the variable importance
plot(final_selection[,2])
sum(final_selection[,2] > mean(final_selection[,2]))


###############################################################
# 49 features with GBM optimum parameters for submission ######
###############################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/train49.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/test49.csv", header = TRUE)
test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/news_popularity_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete49_gbm_tr <- as.h2o(xgboost_train[,-1], destination_frame = "Complete49_gbm_tr")
Complete49_gbm_te <- as.h2o(xgboost_test[,-1], destination_frame = "Complete49_gbm_te")

# Ensuring that the popularity is a factor
Complete49_gbm_tr[,50] <- as.factor(Complete49_gbm_tr[,50])


### GBM
training49.gbm <- h2o.gbm(y=50, x=1:49, training_frame = Complete49_gbm_tr, ntrees=626,
                          max_depth=6, min_rows= 14, learn_rate=0.01,
                          distribution="multinomial")

# Storing the predictions
prediction49 <- h2o.predict(training49.gbm, newdata=Complete49_gbm_te)
pred49 <- as.data.frame(prediction49)

pred49 <- cbind(test[,1], pred49$predict)

colnames(pred49) <- c("id", "popularity")

write.csv(pred49, "submission49.csv")

###############################################################
# 50 features with GBM optimum parameters for submission ######
###############################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/train50.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/test50.csv", header = TRUE)
test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/news_popularity_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete50_gbm_tr <- as.h2o(xgboost_train[,-1], destination_frame = "Complete50_gbm_tr")
Complete50_gbm_te <- as.h2o(xgboost_test[,-1], destination_frame = "Complete50_gbm_te")

# Ensuring that the popularity is a factor
Complete50_gbm_tr[,51] <- as.factor(Complete50_gbm_tr[,51])


### GBM
training50.gbm <- h2o.gbm(y=51, x=1:50, training_frame = Complete50_gbm_tr, ntrees=350,
                          max_depth=5, min_rows= 14, learn_rate=0.025,
                          distribution="multinomial")

# Storing the predictions
prediction50 <- h2o.predict(training50.gbm, newdata=Complete50_gbm_te)
pred50 <- as.data.frame(prediction50)

pred50 <- cbind(test[,1], pred50$predict)

colnames(pred50) <- c("id", "popularity")

write.csv(pred50, "submission50tunedparams.csv")


#################################################
# Original features with GBM optimum parameters #
#################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/training70.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/test30.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete60_gbm_tr <- as.h2o(xgboost_train[,c(-1,-2,-3)], destination_frame = "Complete60_gbm_tr")
Complete60_gbm_te <- as.h2o(xgboost_test[,c(-1,-2,-3)], destination_frame = "Complete60_gbm_te")

# Ensuring that the popularity is a factor
Complete60_gbm_tr[,60] <- as.factor(Complete60_gbm_tr[,60])
Complete60_gbm_te[,60] <- as.factor(Complete60_gbm_te[,60])

### GBM
training60.gbm <- h2o.gbm(y=60, x=1:59, training_frame = Complete60_gbm_tr, ntrees=690,
                          max_depth=3, min_rows= 12, learn_rate=0.026,
                          distribution="multinomial")

# Storing the predictions
prediction60 <- h2o.predict(training60.gbm, newdata=Complete60_gbm_te)
pred60 <- as.data.frame(prediction60)

# Checking the accuracy
percent_correct <- (sum(pred60[,1] == xgboost_test[,63])) / nrow(xgboost_test)
percent_correct


# Storing the variable importance
final_selection <- h2o.varimp(training60.gbm)

# Plotting the variable importance
plot(final_selection[,2])
sum(final_selection[,2] > mean(final_selection[,2]))


################################################################
# Original features with GBM optimum parameters for submission #
################################################################

xgboost_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/news_popularity_training.csv", header = TRUE)
xgboost_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/news_popularity_test.csv", header = TRUE)
test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data/news_popularity_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete60_gbm_tr <- as.h2o(xgboost_train[,c(-1,-2)], destination_frame = "Complete60_gbm_tr")
Complete60_gbm_te <- as.h2o(xgboost_test[,c(-1,-2)], destination_frame = "Complete60_gbm_te")

# Ensuring that the popularity is a factor
Complete60_gbm_tr[,60] <- as.factor(Complete60_gbm_tr[,60])


### GBM
training60.gbm <- h2o.gbm(y=60, x=1:59, training_frame = Complete60_gbm_tr, ntrees=690,
                          max_depth=3, min_rows= 12, learn_rate=0.026,
                          distribution="multinomial")

# Storing the predictions
prediction60 <- h2o.predict(training60.gbm, newdata=Complete60_gbm_te)
pred60 <- as.data.frame(prediction60)

pred60 <- cbind(test[,1], pred60$predict)

colnames(pred60) <- c("id", "popularity")

write.csv(pred60, "submissionoriginaltunedparams1.csv")



###########################################################
######## RANDOM FOREST RANDOM FOREST RANDOM FOREST ########
###########################################################


################################################
# 51 features with RF optimum parameters ######
################################################

rf_train <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/final_ftrs_train.csv", header = TRUE)
rf_test <- read.csv("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/final_ftrs_test.csv", header = TRUE)


# Converting the dataframe to h2o training frames
Complete51_rf_tr <- as.h2o(rf_train[,-1], destination_frame = "Complete51_rf_tr")
Complete51_rf_te <- as.h2o(rf_test[,-1], destination_frame = "Complete51_rf_te")

# Ensuring that the popularity is a factor
Complete51_rf_tr[,51] <- as.factor(Complete51_rf_tr[,51])
Complete51_rf_te[,51] <- as.factor(Complete51_rf_te[,51])


# run rf algorithm
RF2 <- h2o.randomForest(y=51, x=1:50, training_frame=Complete51_rf_tr, 
                        ntrees= 1500, 
                        max_depth=5,
                        mtries = 20,
                        seed=22664, 
                        nfolds=5)

# performance
performance <- h2o.performance(RF2, data=Complete51_rf_tr, valid=TRUE)
performance


# predictions and accuracy
prediction <- h2o.predict(RF2, newdata=Complete51_rf_te)
pred <- as.data.frame(prediction)

actual <- as.matrix(Complete51_rf_te)[,51]
percent_correct <- mean(pred[,1] == actual)
percent_correct