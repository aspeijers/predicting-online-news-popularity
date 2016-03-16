## Xgboost
setwd("~/WorkSpace/Local/Kaggle Final ")

TRAIN <- read.csv("news_popularity_training.csv") 
TEST <- read.csv("news_popularity_test.csv")

AnnTrain <- read.csv("XX_anov_train3.csv")
AnnTest <- read.csv("XX_anov_test3.csv")

# Preparing the original features sets 
train_modified <- TRAIN[,-c(1,2,62)]
test_modified <- TEST[,-c(1,2)]

#
scale <- c(1:12,19:30,39:59)

# Creating scaled values and renaming the columns 

scaled_train <- scale(train_modified[,scale])
scaled_test <- scale(test_modified[,scale])
scaled_names <- paste( "scaled", scale)

colnames(scaled_train) <- scaled_names
colnames(scaled_test) <- scaled_names

train_modified <- cbind(train_modified, scaled_train)
test_modified <- cbind(test_modified, scaled_test)

# Log values


log <- abs(train_modified[,1:59])
log <- apply(log, 2, max)

log <- ifelse(log > 1, 1, 0)

log_train <- log(train_modified[,log])
log_test <- log(test_modified[,log])

log_nums <- which(log > 0) 
log_names <- paste("log", log_nums)
log_names

colnames(log_train) <- log_names
colnames(log_test) <- log_names

train_modified <- cbind(train_modified, log_train)
test_modified <- cbind(test_modified, log_test)

# 

train_newf <- read.csv("train_dciweekday.csv")
test_newf <- read.csv("test_dciweekday.csv")

train_modified <- cbind(train_modified, train_newf[2:5])
test_modified <- cbind(test_modified, test_newf[2:5])

#

interact_names <- paste( "interact.", 33:1, sep = "")

colnames(AnnTrain) <- interact_names
colnames(AnnTest) <- interact_names

train_modified <- cbind(train_modified, AnnTrain)
test_modified <- cbind(test_modified, AnnTest)

#

train_modified <- cbind(train_modified, TRAIN[,62])
colnames(train_modified)[ncol(train_modified)] <- "popularity"

write.csv(train_modified, "final_xgb_train.csv")
write.csv(test_modified, "final_xgb_test.csv")

######
#Running the xgboost

library(data.table)
library(xgboost)
library(Matrix)

gentr <- read.csv("final_xgb_train.csv")
gente <- read.csv("final_xgb_test.csv")

train_gentr <- sapply(gentr[, 2:167], as.numeric)
test_gente <- sapply(gente[, 2:166], as.numeric)

gentr_dtrain <- xgb.DMatrix(data = train_gentr[,1:165], label=as.numeric(train_gentr[,166]))
gente_dtest <- xgb.DMatrix(data = test_gente[,1:165])

xg_train_meta <- matrix(0,30000, 3)
xg_test_meta <- matrix(0,nrow(gente), 3)


bst1 <- xgb.train(data=gentr_dtrain, booster = "gblinear", max.depth= 9, colsample_bytree=10, eta=0.05, nthread = 4, nround=3500, early_stop_round = 10, verbose = T, eval.metric = "rmse", lambda = 0.5,subsample = 0.7, lambda_bias = 0.5, objective = "reg:linear")

xg_train_meta[,1] <- predict(bst1, gentr_dtrain)
xg_test_meta[,1] <- predict(bst1, gente_dtest)


bst2 <- xgb.train(data=gentr_dtrain, booster = "gblinear", max.depth= 10, colsample_bytree=15, eta=0.05, nthread = 4, nround=3500, early_stop_round = 10, verbose = T, eval.metric = "rmse", lambda = 0.5,subsample = 0.9, lambda_bias = 0.5, objective = "reg:linear")

xg_train_meta[,2] <- predict(bst2, gentr_dtrain)
xg_test_meta[,2] <- predict(bst2, gente_dtest)


bst3 <- xgb.train(data=gentr_dtrain, booster = "gblinear", max.depth= 10, colsample_bytree=15, eta=0.2, nthread = 4, nround=3500, early_stop_round = 10, verbose = T, eval.metric = "rmse", lambda = 0.5,subsample = 0.85, lambda_bias = 0.5, objective = "reg:linear")

xg_train_meta[,3] <- predict(bst3, gentr_dtrain)
xg_test_meta[,3] <- predict(bst3, gente_dtest)

boost_names <- paste("xgboost", 1:3, sep=".")

colnames(xg_train_meta) <- boost_names
colnames(xg_test_meta) <- boost_names

write.csv(xg_train_meta, "completion_train.csv")
write.csv(xg_test_meta, "completion_test.csv")