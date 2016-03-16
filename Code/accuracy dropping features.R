# Developing a function to update whether dropping features will help or not
train_baseline <- read.csv("finalstretch_train.csv")
test_baseline <- read.csv("finalstretch_test.csv")

# Finding an initial accuracy for comparison 
accuracy1 <- as.data.frame(matrix(0, ncol(train_baseline),2))
colnames(accuracy1) <- c("accuracy", "drop/baseline")

# 
library(nnet)
baseline_model <- multinom(popularity ~ ., data = train_baseline)
baseline_results <- predict(baseline_model, test_baseline, "probs")
baseline_classes <- apply(baseline_results, 1, which.max)
baseline_accuracy <- (sum(baseline_classes == actualLabels))/9000

accuracy1[ncol(train_baseline),1] <- baseline_accuracy
accuracy1[ncol(train_baseline),2] <- "baseline"

accuracydrop <- function( X, Y, a, z = NULL){
	X1 <- X[,-z]
	Y1 <- Y[,-z]

	model <- multinom(popularity ~ ., data = X1)
	results <- predict(model, Y1, "probs")
	classes <- apply(results, 1, which.max)
	accuracy <- (sum(classes == actualLabels))/9000

	if(accuracy > a) {
		return(accuracy)
	}
	else { return(0)}
}

for (i in 1:(ncol(train_baseline) - 1)){
	accuracy1[i,1] <- accuracydrop(train, test, accuracy1[ncol(train_baseline),1], i)
	if(accuracy1[i,1] > 0){
		accuracy1[i,2] <- "drop"
	}
	else { accuracy1[i,2] <- "baseline"}
}

for (i in 1:2){
	accuracy1[i,1] <- accuracydrop(train, test, accuracy1[51,1], i)
	if(accuracy1[i,1] > 0){
		accuracy1[i,2] <- "drop"
	}
	else{ accuracy1[i,2] <- "baseline"}
}