#install.packages("randomForest")
library(randomForest)

setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/code")

## Read in training data
train <- read.csv("../data/news_popularity_training.csv", header = TRUE)

## Read in test data
test <- read.csv("../data/news_popularity_test.csv", header = TRUE)

## Cleaning
#combined <- combined[combined$n_unique_tokens<=1, ] 
#combined <- combined[combined$n_non_stop_words<=1, ] 
#combined <- combined[combined$n_non_stop_unique_tokens<=1, ]

# Add new labels to training data to account for ordinality
train$A1 <- ifelse(train$popularity < 2, 0, 1) # split into y={1} (0) and y={2,3,4,5} (1)
train$A2 <- ifelse(train$popularity < 3, 0, 1) # split into y={1,2} (0) and y={3,4,5} (1)
train$A3 <- ifelse(train$popularity < 4, 0, 1) # split into y={1,2,3} (0) and y={4,5} (1)
train$A4 <- ifelse(train$popularity < 5, 0, 1) # split into y={1,2,3,4} (0) and y={5} (1)

# change popularity and new binaries to factors
train$popularity <- as.factor(train$popularity)  #X's are 3:61, Y is 62
train$A1 <- as.factor(train$A1)
train$A2 <- as.factor(train$A2)
train$A3 <- as.factor(train$A3)
train$A4 <- as.factor(train$A4)

# Run a base model to compare against
RFbase1 <- randomForest(x=train[,3:61], y=train[,62], ntree=1000, importance=TRUE, do.trace=TRUE )

# Run 5 binary models
RFbaseA1 <- randomForest(x=train[,3:61], y=train[,63], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA2 <- randomForest(x=train[,3:61], y=train[,64], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA3 <- randomForest(x=train[,3:61], y=train[,65], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA4 <- randomForest(x=train[,3:61], y=train[,66], ntree=1000, importance=TRUE, ?roll=TRUE )

# oob-error on single model
head(RFbase1$err.rate)
tail(RFbase1$err.rate)
plot(x=1:1000, RFbase1$err.rate[,1], type='l')
plot(RFbase1) #what is this plotting


########### Predicting ###############
# single model
if (exists("submission")) rm(submission)
submission <- data.frame( id= test$id )
submission$popularity <- predict(RFbase1, newdata=test[,3:61])
write.csv(submission, file="../Submissions/submission_RFbase1_1.csv", row.names=FALSE)

# combined models - accounting for ordinality in categories
predictA1 <- predict(RFbaseA1, test[,3:61], type="prob")
predictA2 <- predict(RFbaseA2, test[,3:61], type="prob")
predictA3 <- predict(RFbaseA3, test[,3:61], type="prob")
predictA4 <- predict(RFbaseA4, test[,3:61], type="prob")

# create 30000 x 5 matrix for probability of each class.
# P(class 1) = 1 - P(x > class 1)
# P(class 2) = P(x > class 1) - P(x > class 2)
# P(class 3) = P(x > class 2) - P(x > class 3)
# P(class 4) = P(x > class 3) - P(x > class 4)
# P(class 5) = P(x > class 4)
postProb <- matrix(NA, nrow(test), 5)
postProb[,1] <- predictA1[,1]
postProb[,2] <- predictA1[,2] - predictA2[,2]
postProb[,3] <- predictA2[,2] - predictA3[,2]
postProb[,4] <- predictA3[,2] - predictA4[,2]
postProb[,5] <- predictA4[,2]

if (exists("submission")) rm(submission)
submission <- data.frame(id=test$id)
submission$popularity <- apply(postProb, 1, which.max)
write.csv(submission, file="../Submissions/submission_RFbaseOrdinal_1.csv", row.names=FALSE)


#################### Accounting for Imbalanced Classes by over/undersampling ########################
# Run a base model to compare against - account for imbalance between classes 1,2 & 3 (ignore 4 & 5)
# Class 3 has the least observations. Sample half this amount (2856).
RFbase1.1 <- randomForest(x=train[,3:61], y=train[,62], ntree=2000, importance=TRUE, do.trace=TRUE, sampsize=c(100,100,100,100,5))

# test this by doing subsampling first
class1 <- train[train$popularity==1,]
class2 <- train[train$popularity==2,]
subSamplingClass1 <- sample(1:dim(class1)[1], size=5712, replace=FALSE)
subSamplingClass2 <- sample(1:dim(class2)[1], size=5712, replace=FALSE)
subSample <- rbind(train[train$popularity==3 | train$popularity==4 | train$popularity==5, ], class1[subSamplingClass1,], class2[subSamplingClass2,])

# run RF on the subsampled data set
RFbase1.2 <- randomForest(x=subSample[,3:61], y=subSample[,62], ntree=2000, importance=TRUE, do.trace=TRUE, strata=subSample$popularity, sampsize=c(2856,2856,2856,500,20), nodesize = 1)

# single model prediction
if (exists("submission")) rm(submission)
submission <- data.frame( id= test$id )
submission$popularity <- predict(RFbase1.2, newdata=test[,3:61])
write.csv(submission, file="../Submissions/submission_RFbase1.2_1.csv", row.names=FALSE)

# Run 5 binary models
RFbaseA1.1 <- randomForest(x=subSample[,3:61], y=subSample[,63], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA2.1 <- randomForest(x=subSample[,3:61], y=subSample[,64], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA3.1 <- randomForest(x=subSample[,3:61], y=subSample[,65], ntree=1000, importance=TRUE, do.trace=TRUE )
RFbaseA4.1 <- randomForest(x=subSample[,3:61], y=subSample[,66], ntree=1000, importance=TRUE, do.trace=TRUE )

# combined models - accounting for ordinality in categories
predictA1.1 <- predict(RFbaseA1.1, test[,3:61], type="prob")
predictA2.1 <- predict(RFbaseA2.1, test[,3:61], type="prob")
predictA3.1 <- predict(RFbaseA3.1, test[,3:61], type="prob")
predictA4.1 <- predict(RFbaseA4.1, test[,3:61], type="prob")

# create 30000 x 5 matrix for probability of each class.
postProb.1 <- matrix(NA, nrow(test), 5)
postProb.1[,1] <- predictA1.1[,1]
postProb.1[,2] <- predictA1.1[,2] - predictA2.1[,2]
postProb.1[,3] <- predictA2.1[,2] - predictA3.1[,2]
postProb.1[,4] <- predictA3.1[,2] - predictA4.1[,2]
postProb.1[,5] <- predictA4.1[,2]

if (exists("submission")) rm(submission)
submission <- data.frame(id=test$id)
submission$popularity <- apply(postProb.1, 1, which.max)
write.csv(submission, file="../Submissions/submission_RFbaseOrdinal_2.csv", row.names=FALSE)

############################# Next try - don't undersample class 2 so much! ########################
# create subsample that undersamples classes 1 and 2. Take no.obs of class 2 as 1.5*no.obs of class 3, and no.obs of class 1 as no.obs of class 3. 
class1 <- train[train$popularity==1,]
class2 <- train[train$popularity==2,]
subSamplingClass1 <- sample(1:dim(class1)[1], size=8568, replace=FALSE)
subSampling1Class2 <- sample(1:dim(class2)[1], size=5712, replace=FALSE)
subSample <- rbind(train[train$popularity==3 | train$popularity==4 | train$popularity==5, ], class1[subSamplingClass1,], class2[subSamplingClass2,])

# Run 5 binary models
RFbaseA1.2 <- randomForest(x=subSample[,3:61], y=subSample[,63], ntree=700, importance=TRUE, do.trace=TRUE )
RFbaseA2.2 <- randomForest(x=subSample[,3:61], y=subSample[,64], ntree=700, importance=TRUE, do.trace=TRUE )
RFbaseA3.2 <- randomForest(x=subSample[,3:61], y=subSample[,65], ntree=700, importance=TRUE, do.trace=TRUE )
RFbaseA4.2 <- randomForest(x=subSample[,3:61], y=subSample[,66], ntree=700, importance=TRUE, do.trace=TRUE )

# combined models - accounting for ordinality in categories
predictA1.2 <- predict(RFbaseA1.2, test[,3:61], type="prob")
predictA2.2 <- predict(RFbaseA2.2, test[,3:61], type="prob")
predictA3.2 <- predict(RFbaseA3.2, test[,3:61], type="prob")
predictA4.2 <- predict(RFbaseA4.2, test[,3:61], type="prob")

# create 30000 x 5 matrix for probability of each class.
postProb.2 <- matrix(NA, nrow(test), 5)
postProb.2[,1] <- predictA1.2[,1]
postProb.2[,2] <- predictA1.2[,2] - predictA2.2[,2]
postProb.2[,3] <- predictA2.2[,2] - predictA3.2[,2]
postProb.2[,4] <- predictA3.2[,2] - predictA4.2[,2]
postProb.2[,5] <- predictA4.2[,2]

if (exists("submission")) rm(submission)
submission <- data.frame(id=test$id)
submission$popularity <- apply(postProb.1, 1, which.max)
write.csv(submission, file="../Submissions/submission_RFbaseOrdinal_3.csv", row.names=FALSE)




# predictions and accuracy
prediction <- predict(RFbase1, newdata=test_X)
pred <- as.numeric(prediction)
percent_correct <- mean(pred == popularity_test)
percent_correct

varImpPlot(RFbase2)

