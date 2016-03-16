# setwd("/home/sreeni-hhs/WorkSpace/Local/Kaggle Final /Report")
TRAIN <- read.csv("news_popularity_training.csv") 
TEST <- read.csv("news_popularity_test.csv")

training70 <- read.csv("training70.csv")
test30 <- read.csv("test30.csv")

ratio <- table(TRAIN[,62])
ratio <- as.numeric(ratio)
ratio <- ratio/30000
ratio <- ratio*100

library(glmnet)
train <- TRAIN[,-c(1,2)]
test <- TEST[,-c(1,2)]

train <- as.matrix(train)
test <- as.matrix(test)

training_70 <- training70[-c(1,2,3)]
test_30 <- test30[,-c(1,2,3)]

train[,60] <- as.factor(train[,60])

training_70 <- as.matrix(training_70)
test_30 <- as.matrix(test_30)

training_70[,60] <- as.factor(training_70[,60])
test_30[,60] <- as.factor(test_30[,60])

cvfit=cv.glmnet(training_70[,1:59], training_70[,60], family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(cvfit)
glmnet.base <- as.numeric(predict(cvfit, train[,1:59], s = "lambda.min", type = "class"))

sum(glmnet.base == TRAIN[,62])/30000


####################################################################################
training_final <- read.csv("train50.csv")
test_final <- read.csv("test50.csv")

training_final <- as.matrix(training_final)
test_final <- as.matrix(test_final)

training_final[,52] <- as.factor(training_final[,52])

library(glmnet)
cvfit=cv.glmnet(training_final[,2:51], training_final[,52], family="multinomial", type.multinomial = "grouped", parallel = TRUE)

plot(cvfit)
glmnet.base <- as.numeric(predict(cvfit, training_final[,2:51], s = "lambda.min", type = "class"))

sum(glmnet.base == training_final[,52])/30000

#####################################################################3
training_final2 <- read.csv("train49.csv")
test_final2 <- read.csv("test49.csv")


training_final2 <- as.matrix(training_final2)
test_final2 <- as.matrix(test_final2)

training_final2[,51] <- as.factor(training_final2[,51])


cvfit2=cv.glmnet(training_final2[,2:50], training_final2[,51], family="multinomial", type.multinomial = "grouped", parallel = TRUE)

plot(cvfit)
glmnet.base2 <- as.numeric(predict(cvfit2, training_final2[,2:50], s = "lambda.min", type = "class"))

sum(glmnet.base2 == training_final2[,51])/30000
###############################################################################

TRAIN <- as.matrix(TRAIN)
TRAIN <- as.matrix(TEST)


TRAIN <- TRAIN[,-c(1,2)]

TRAIN[,60] <- as.factor(TRAIN[,60])

test_30[,60] <- as.factor(test_30[,60])

TRAIN <- data.matrix(TRAIN)

cvfit3=cv.glmnet(x = TRAIN[,1:59], y = TRAIN[,60], family="multinomial", type.multinomial = "grouped", parallel = TRUE)

plot(cvfit)

glmnet.base3 <- as.numeric(predict(cvfit3, TRAIN[,1:59], s = "lambda.min", type = "class"))

sum(glmnet.base3 == TRAIN[,60])/30000

## 49 - 0.498033
## 50 - 0.5045333
## ori - 0.5070 

########################################################################
training_final4 <- read.csv("train49.csv")
test_final4 <- read.csv("test49.csv")


training_final2 <- as.matrix(training_final2)
test_final2 <- as.matrix(test_final2)

training_final2[,51] <- as.factor(training_final2[,51])


cvfit2=cv.glmnet(training_final2[,2:50], training_final2[,51], family="multinomial", type.multinomial = "grouped", parallel = TRUE)

plot(cvfit)
glmnet.base2 <- as.numeric(predict(cvfit2, training_final2[,2:50], s = "lambda.min", type = "class"))

sum(glmnet.base2 == training_final2[,51])/30000


###########################################################################

library(h2o)
h2o.init(nthreads = -1)

training_70 <- training70[,-c(1,2,3)]
test_30 <- test30[,-c(1,2,3)]

features_train2 <- as.h2o(training_70, destination_frame = "features_train2")
features_test2 <- as.h2o(test_30, destination_frame = "features_test2")

features_train2[,60] <- as.factor(features_train2[,60])

dpmodel <- h2o.deeplearning( x = 1:59, y = 60, features_train2, distribution = "multinomial",
                             hidden = c(100,100,100,100,100,100,100,100,100,100,100), 
                             stopping_rounds = 5, stopping_metric = "logloss", rate = 0.001, 
                             loss = "CrossEntropy", train_samples_per_iteration = -2,epochs = 10)

(sum(pred == test_30[,60]))/9000





###############################################################################################
train <- read.csv("train50.csv")
test <- read.csv("test50.csv")

train <- train[,-1]
test <- test[,-1]

train.pca <- prcomp(train[,1:50], scale. = TRUE)
test.pca <- prcomp(test, scale. = TRUE)

str(train.pca)

train.pc <- train.pca$x

library(h2o)
h2o.init(nthreads = -1)

?h2o.randomForest

train.pc <- cbind(train.pc, train[,51])
train.pc.rf <- as.h2o(train.pc, destination_frame = "train.pc")
train.pc.rf[,51] <- as.factor(train.pc.rf[,51])

fit <- h2o.randomForest(x = 1:50, y = 51, train.pc.rf, mtries = -1, sample_rate = 0.7, ntrees= 200, max_depth = 7, min_rows = 10, stopping_metric = "misclassification", nfolds = 5) 
summary(fit)


prediction <- h2o.predict(dpmodel, newdata=features_test2)
pred <- as.data.frame(prediction)