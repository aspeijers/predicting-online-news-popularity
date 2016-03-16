setwd("~/WorkSpace/predicting-online-news-popularity/Data")

train <- read.csv("train_accuracyv2.csv")
test <- read.csv("test_accuracyv2.csv")
train <- train[,-1]
test <- test[,-1]
#Creating a separate lables vector for final predicted labels comparison
actualLabels <- test[,51]

#The multinom function is part of the nnet package
library(nnet)

# Creating an empty row at the bottom of the train dataset in order to store
# the action performed on the column
train <- as.data.frame(train)
test <- as.data.frame(test)

col <- colnames(train)


accuracy1 <- function(X, Y, z = NULL){

	#This is the empty accuracy vector that stores all the compared accuracies
	accuracy_measure <- rep(0,11)

	# This is a vector with the strings so that we can just use this as a reference 
	# to read the transformation that we should apply 

	actions <- c("scale", "log", "square", 
		"square - mean", "square - x", "cube", "cube - x", "cube - mean", "cube - square", 
		"sqrt_abs", "baseline")

	# Creating the baseline model so that a comparison can be made for whether a 
	# transformation is worth it or not 

	X1 <- X[,-ncol(X)]
	Y1 <- Y[,-ncol(Y)]

	model_base <- multinom(popularity ~ ., data = X1, maxit = 100, MaxNWts = 10000)
	results_base <- predict(model_base, Y1, "probs")
	classes_base <- apply(results_base, 1, which.max)
	accuracy_base <- (sum(classes_base == actualLabels))/9000

	accuracy_measure[11] <- accuracy_base

	train2 <- X1
	train3 <- X1
	train5 <- X1
	train6 <- X1
	train7 <- X1
	train8 <- X1
	train9 <- X1
	train10 <- X1
	train11 <- X1
	train12 <- X1

	test2 <- Y1
	test3 <- Y1
	test5 <- Y1
	test6 <- Y1
	test7 <- Y1
	test8 <- Y1
	test9 <- Y1
	test10 <- Y1
	test11 <- Y1
	test12 <- Y1

	train2[,z] <- as.numeric(scale(train2[,z]))
	test2[,z] <- as.numeric(scale(test2[,z]))
	model_scale <- multinom(popularity~., data = train2, maxit = 100, MaxNWts = 10000)
	results_scale <- predict(model_scale, test2, "probs")
	classes_scale <- apply(results_scale, 1, which.max)
	accuracy_scale <- (sum(classes_scale == actualLabels))/9000

	mn <- min(train[,z])
	off <- 1 + abs(mn)
	train3[,z] <- log(train3[,z] + off)
	test3[,z] <- log(test3[,z] + off)
	model_log <- multinom(popularity~., data = train3, maxit = 100, MaxNWts = 10000)
	results_log <- predict(model_log, test3, "probs")
	classes_log <- apply(results_log, 1, which.max)
	accuracy_log <- (sum(classes_log == actualLabels))/9000

	train5[,z] <- train5[,z]**2
	test5[,z] <- test5[,z]**2
	model_square <- multinom(popularity ~ ., data = train5, maxit = 100, MaxNWts = 10000)
	results_square <- predict(model_square, test5, "probs")
	classes_square <- apply(results_square, 1, which.max)
	accuracy_square <- (sum(classes_square == actualLabels))/9000

	train6[,z] <- (train6[,z]**2) - mean(train6[,z])
	test6[,z] <- (test6[,z]**2) - mean(test6[,z])
	model_squaremean <- multinom(popularity ~ ., data = train6, maxit = 100, MaxNWts = 10000)
	results_squaremean <- predict(model_squaremean, test6, "probs")
	classes_squaremean <- apply(results_squaremean, 1, which.max)
	accuracy_squaremean <- (sum(classes_squaremean == actualLabels))/9000

	train7[,z] <- (train7[,z]**2) - train7[,z]
	test7[,z] <- (test7[,z]**2) - test7[,z]
	model_squarex <- multinom(popularity ~ ., data = train7, maxit = 100, MaxNWts = 10000)
	results_squarex <- predict(model_squarex, test7, "probs")
	classes_squarex <- apply(results_squarex, 1, which.max)
	accuracy_squarex <- (sum(classes_squarex == actualLabels))/9000

	train8[,z] <- (train8[,z]**3)
	test8[,z] <- (test8[,z]**3)
	model_cube <- multinom(popularity ~ ., data = train8, maxit = 100, MaxNWts = 10000)
	results_cube <- predict(model_cube, test8, "probs")
	classes_cube <- apply(results_cube, 1, which.max)
	accuracy_cube <- (sum(classes_cube == actualLabels))/9000

	train9[,z] <- (train9[,z]**3) - train9[,z]
	test9[,z] <- (test9[,z]**3) - test9[,z]
	model_cubex <- multinom(popularity ~ ., data = train9, maxit = 100, MaxNWts = 10000)
	results_cubex <- predict(model_cubex, test9, "probs")
	classes_cubex <- apply(results_cubex, 1, which.max)
	accuracy_cubex <- (sum(classes_cubex == actualLabels))/9000

	train10[,z] <- (train10[,z]**3) - mean(train10[,z])
	test10[,z] <- (test10[,z]**3) - mean(test10[,z])
	model_cubemean <- multinom(popularity ~ ., data = train10, maxit = 100, MaxNWts = 10000)
	results_cubemean <- predict(model_cubemean, test10, "probs")
	classes_cubemean <- apply(results_cubemean, 1, which.max)
	accuracy_cubemean <- (sum(classes_cubemean == actualLabels))/9000

	train11[,z] <- (train11[,z]**3) - (train11[,z]**2)
	test11[,z] <- (test11[,z]**3) - (test11[,z]**2)
	model_cubesq <- multinom(popularity ~ ., data = train11, maxit = 100, MaxNWts = 10000)
	results_cubesq <- predict(model_cubesq, test11, "probs")
	classes_cubesq <- apply(results_cubesq, 1, which.max)
	accuracy_cubesq <- (sum(classes_cubesq == actualLabels))/9000

	train12[,z] <- sqrt(abs(train12[,z]))
	test12[,z] <- sqrt(abs(test12[,z]))
	model_sqrtabs <- multinom(popularity ~ ., data = train12, maxit = 100, MaxNWts = 10000)
	results_sqrtabs <- predict(model_sqrtabs, test12, "probs")
	classes_sqrtabs <- apply(results_sqrtabs, 1, which.max)
	accuracy_sqrtabs <- (sum(classes_sqrtabs == actualLabels))/9000
		
	accuracy_measure[1] <- accuracy_scale
	accuracy_measure[2] <- accuracy_log
	accuracy_measure[3] <- accuracy_square
	accuracy_measure[4] <- accuracy_squaremean
	accuracy_measure[5] <- accuracy_squarex
	accuracy_measure[6] <- accuracy_cube
	accuracy_measure[7] <- accuracy_cubex
	accuracy_measure[8] <- accuracy_cubemean
	accuracy_measure[9] <- accuracy_cubesq
	accuracy_measure[10] <- accuracy_sqrtabs

	perform <- which.max(accuracy_measure)


	if(perform == 1){
		X1 <- train2
		Y1 <- test2
	}

	else if(perform == 2){
		X1 <- train3
		Y1 <- test3
	}

	else if(perform == 3){
		X1 <- train5
		Y1 <- test5
	}

	else if(perform == 4){
		X1 <- train6
		Y1 <- test6
	}

	else if(perform == 5){
		X1 <- train7
		Y1 <- test7
	}

	else if(perform == 6){
		X1 <- train8
		Y1 <- test8
	}

	else if(perform == 7){
		X1 <- train9
		Y1 <- test9
	}

	else if(perform == 8){
		X1 <- train10
		Y1 <- test10
	}

	else if(perform == 9){
		X1 <- train11
		Y1 <- test11
	}

	else if(perform == 10){
		X1 <- train12
		Y1 <- test12
	}

	else {
		X1 <- X1
		Y1 <- Y1
	}

	X[,1:(ncol(X) - 1)] <- X1
	Y[,1:(ncol(Y) - 1)] <- Y1
	X[z,ncol(X)] <- actions[perform]
	Y[z,ncol(Y)] <- actions[perform]
	new <- as.data.frame(matrix(0,(nrow(X) + nrow(Y)), ncol(X)))
	colnames(new) <- colnames(X)
	new[1:nrow(X),1:ncol(X)] <- as.data.frame(X[1:nrow(X), 1:ncol(X)])
	new[(nrow(X) +1):(nrow(X) + nrow(Y)),] <- as.data.frame(Y[1:nrow(Y),1:ncol(Y)])

	return(data.frame = new)
}

#The actual loop
for(i in 1:50){
	new <- accuracy1(train,test,i)
	train <- as.data.frame(new[1:nrow(train),])
	test <- as.data.frame(new[(nrow(train) +1):(nrow(train) + nrow(test)),])
	colnames(train) <- col
	colnames(test) <- col
}

#Checking the final accuracy
train_round2 <- train[,-c(52)]
test_round2 <- test[,-c(52)]
# I hope it works
model_round2 <- multinom(popularity ~ ., data = train_round2)
results_round2 <- predict(model_round2, test_round2, "probs")
classes_round2 <- apply(results_round2, 1, which.max)
accuracy_round2 <- (sum(classes_round2 == actualLabels))/9000

#Storing the new matrices
write.csv(train,"train_accuracyv3.csv")
write.csv(test,"test_accuracyv3.csv")
