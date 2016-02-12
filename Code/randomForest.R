install.packages("randomForest")
library(randomForest)

setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/code")

## Read in training data
data <- read.csv("../data/training70.csv", header = TRUE)
X <- data[,4:62]
popularity <- as.factor(data[,63])

combined <- cbind(X, popularity)
combined$popularity <- as.factor(combined$popularity)

features <- names(X)

## Read in test data
data_test <- read.csv("../data/test30.csv", header = TRUE)
X_test <- data_test[,4:62]
popularity_test <- data_test[,63]

## Cleaning
combined <- combined[combined$n_unique_tokens<=1, ] 
combined <- combined[combined$n_non_stop_words<=1, ] 
combined <- combined[combined$n_non_stop_unique_tokens<=1, ]

RFbase1 <- randomForest(combined[,1:59], as.factor(combined[,60]), ntree=200, importance=TRUE, do.trace=TRUE )
RFbase2 <- randomForest(popularity ~ ., data=combined, ntree=100, importance=TRUE, do.trace=TRUE )

# predictions and accuracy
prediction <- predict(RFbase1, newdata=X_test)
pred <- as.numeric(prediction)
percent_correct <- mean(pred == popularity_test)
percent_correct

varImpPlot(RFbase2)


##### Real test data ######
testData <- read.csv("../data/news_popularity_test.csv")
submission <- data.frame( id= testData$id )
submission$popularity <- predict(RFbase1, newdata=testData[,3:61])
write.csv(submission, file="../Submissions/submission_RFbase1.csv", row.names=FALSE)
