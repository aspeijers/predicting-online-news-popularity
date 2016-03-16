setwd("~/WorkSpace/predicting-online-news-popularity/Data")
setwd("~/WorkSpace/predicting-online-news-popularity/Code")
#accuracy comparison function
# X is features matrix input 
# Y is test features matrix input 
# z is column number
# Reads in the datasets
data1 <- read.csv("training70.csv")
data2 <- read.csv("test30.csv")

#It takes out the identifier variables
train <- data1[,-c(1,2,3)]
test <- data2[,-c(1,2,3,63)]

#Creating a separate lables vector for final predicted labels comparison
actualLabels <- data2[,63]

#This is the empty accuracy matrix that stores all the compared accuracies
accuracy_measure <- matrix(0,ncol(train),13)
colnames(accuracy_measure) <- c("drop", "scale", "log", "abs", "square", 
	"square - mean", "square - x", "cube", "cube - x", "cube - mean", "cube - square", 
	"sqrt_abs", "x[z]*x[,z + 1]")

# This is a vector with the strings so that we can just use this as a reference 
# to read the transformation that we should apply 

actions <- c("drop", "scale", "log", "abs", "square", 
	"square - mean", "square - x", "cube", "cube - x", "cube - mean", "cube - square", 
	"sqrt_abs", "x[z]*x[,z + 1]", "baseline")

#The multinom function is part of the nnet package
library(nnet)

#The accuracy function works in the following manner 
# 1) It assigns the input training features matrix and test features matrix to new objects
# 2) It then uses one object per transformation and then does the multionmial regression
# 3) It predicts the classes and stores the accuracy score
# 4) At the ending of each iteration, the accuracy scores are loaded into a single vector
# 5) This vector is returned as the output which is loaded into a row of the accuracy_measure matrix
accuracy <- function(X, Y, z = NULL){
	train1 <- X
	train2 <- X
	train3 <- X
	train4 <- X
	train5 <- X
	train6 <- X
	train7 <- X
	train8 <- X
	train9 <- X
	train10 <- X
	train11 <- X
	train12 <- X
	train13 <- X

	test1 <- Y
	test2 <- Y
	test3 <- Y
	test4 <- Y
	test5 <- Y
	test6 <- Y
	test7 <- Y
	test8 <- Y
	test9 <- Y
	test10 <- Y
	test11 <- Y
	test12 <- Y
	test13 <- Y

	accuracy_measure2 <- rep(0,14)
	
	train1 <- train1[,-z]
	test1 <- test1[,-z]
	model_drop <- multinom(popularity ~ ., data = train1)
	results_drop <- predict(model_drop, test1, "probs")
	classes_drop <- apply(results_drop, 1, which.max)
	accuracy_drop <- (sum(classes_drop == actualLabels))/9000

	train2[,z] <- as.numeric(scale(train2[,z]))
	test2[,z] <- as.numeric(scale(test2[,z]))
	model_scale <- multinom(popularity~., data = train2)
	results_scale <- predict(model_scale, test2, "probs")
	classes_scale <- apply(results_scale, 1, which.max)
	accuracy_scale <- (sum(classes_scale == actualLabels))/9000

	mn <- min(train[,z])
	off <- 1 + abs(mn)
	train3[,z] <- log(train3[,z] + off)
	test3[,z] <- log(test3[,z] + off)
	model_log <- multinom(popularity~., data = train3)
	results_log <- predict(model_log, test3, "probs")
	classes_log <- apply(results_log, 1, which.max)
	accuracy_log <- (sum(classes_log == actualLabels))/9000

	train4[,z] <- abs(train4[,z])
	test4[,z] <- abs(test4[,z])
	model_abs <- multinom(popularity ~ ., data = train4)
	results_abs <- predict(model_abs, test4, "probs")
	classes_abs <- apply(results_abs, 1, which.max)
	accuracy_abs <- (sum(classes_abs == actualLabels))/9000

	train5[,z] <- train5[,z]**2
	test5[,z] <- test5[,z]**2
	model_square <- multinom(popularity ~ ., data = train5)
	results_square <- predict(model_square, test5, "probs")
	classes_square <- apply(results_square, 1, which.max)
	accuracy_square <- (sum(classes_square == actualLabels))/9000

	train6[,z] <- (train6[,z]**2) - mean(train6[,z])
	test6[,z] <- (test6[,z]**2) - mean(test6[,z])
	model_squaremean <- multinom(popularity ~ ., data = train6)
	results_squaremean <- predict(model_squaremean, test6, "probs")
	classes_squaremean <- apply(results_squaremean, 1, which.max)
	accuracy_squaremean <- (sum(classes_squaremean == actualLabels))/9000

	train7[,z] <- (train7[,z]**2) - train7[,z]
	test7[,z] <- (test7[,z]**2) - test7[,z]
	model_squarex <- multinom(popularity ~ ., data = train7)
	results_squarex <- predict(model_squarex, test7, "probs")
	classes_squarex <- apply(results_squarex, 1, which.max)
	accuracy_squarex <- (sum(classes_squarex == actualLabels))/9000

	train8[,z] <- (train8[,z]**3)
	test8[,z] <- (test8[,z]**3)
	model_cube <- multinom(popularity ~ ., data = train8)
	results_cube <- predict(model_cube, test8, "probs")
	classes_cube <- apply(results_cube, 1, which.max)
	accuracy_cube <- (sum(classes_cube == actualLabels))/9000

	train9[,z] <- (train9[,z]**3) - train9[,z]
	test9[,z] <- (test9[,z]**3) - test9[,z]
	model_cubex <- multinom(popularity ~ ., data = train9)
	results_cubex <- predict(model_cubex, test9, "probs")
	classes_cubex <- apply(results_cubex, 1, which.max)
	accuracy_cubex <- (sum(classes_cubex == actualLabels))/9000

	train10[,z] <- (train10[,z]**3) - mean(train10[,z])
	test10[,z] <- (test10[,z]**3) - mean(test10[,z])
	model_cubemean <- multinom(popularity ~ ., data = train10)
	results_cubemean <- predict(model_cubemean, test10, "probs")
	classes_cubemean <- apply(results_cubemean, 1, which.max)
	accuracy_cubemean <- (sum(classes_cubemean == actualLabels))/9000

	train11[,z] <- (train11[,z]**3) - (train11[,z]**2)
	test11[,z] <- (test11[,z]**3) - (test11[,z]**2)
	model_cubesq <- multinom(popularity ~ ., data = train11)
	results_cubesq <- predict(model_cubesq, test11, "probs")
	classes_cubesq <- apply(results_cubesq, 1, which.max)
	accuracy_cubesq <- (sum(classes_cubesq == actualLabels))/9000

	train12[,z] <- sqrt(abs(train12[,z]))
	test12[,z] <- sqrt(abs(test12[,z]))
	model_sqrtabs <- multinom(popularity ~ ., data = train12)
	results_sqrtabs <- predict(model_sqrtabs, test12, "probs")
	classes_sqrtabs <- apply(results_sqrtabs, 1, which.max)
	accuracy_sqrtabs <- (sum(classes_sqrtabs == actualLabels))/9000

	train13[,z] <- (train13[,z])*(train13[,z+1])
	test13[,z] <- (test13[,z])*(test13[,z+1])
	model_xx1 <- multinom(popularity ~ ., data = train13)
	results_xx1 <- predict(model_xx1, test13, "probs")
	classes_xx1 <- apply(results_xx1, 1, which.max)
	accuracy_xx1 <- (sum(classes_xx1 == actualLabels))/9000
		
	accuracy_measure2[1] <- accuracy_drop
	accuracy_measure2[2] <- accuracy_scale
	accuracy_measure2[3] <- accuracy_log
	accuracy_measure2[4] <- accuracy_abs
	accuracy_measure2[5] <- accuracy_square
	accuracy_measure2[6] <- accuracy_squaremean
	accuracy_measure2[7] <- accuracy_squarex
	accuracy_measure2[8] <- accuracy_cube
	accuracy_measure2[9] <- accuracy_cubex
	accuracy_measure2[10] <- accuracy_cubemean
	accuracy_measure2[11] <- accuracy_cubesq
	accuracy_measure2[12] <- accuracy_sqrtabs
	accuracy_measure2[13] <- accuracy_xx1
	return(accuracy_measure2)
}

# I initially did it just for 1 column to check the time duration it will require
# for running the code across the entire feature matrix
system.time(accuracy_measure[1,] <- accuracy(train,test,1))

# Iteratively running the code across the entire matrix
for (i in 1:59) {
	accuracy_measure[i,] <- accuracy(train, test, i)
}
write.csv(accuracy_measure, "accuracies.csv")

# Creating the baseline model so that a comparison can be made for whether a 
# transformation is worth it or not 

model_base <- multinom(popularity ~ ., data = train)
results_base <- predict(model_base, test, "probs")
classes_base <- apply(results_base, 1, which.max)
accuracy_base <- (sum(classes_base == actualLabels))/9000

# This is a vector that consists of just the baseline accuracy so that I can bind it with the 
# accuracy_measure matrix

accuracy <- rep(accuracy_base, nrow(accuracy_measure))
new_measure <- cbind(accuracy_measure, accuracy)

colnames(new_measure) <- c("drop", "scale", "log", "abs", "square", 
                           "square - mean", "square - x", "cube", "cube - x", 
                           "cube - mean", "cube - square", 
                           "sqrt_abs", "x[z]*x[,z + 1]", "baseline")

#Developing a final matrix that stores the recommended action no. and string 
# Each row in this matrix corresponds to a feature column in our train
action <- matrix(0, nrow(new_measure), 2)
action[,1] <- apply(new_measure, 1, which.max)

#Converting to data frame as matrix only stores homogenous data
# Updating the strings into the matrix by referring to it from the actions vector
# created in the beginning
action <- as.data.frame(action)
for( j in 1:ncol(action)){
	action[,2] <- actions[action[,1]]
	}	

# This is the function that transforms the features by checking the number from the
# column in the action matrix
# X is the features matrix input, y is the action[i,1], i is the index of feature column
feature_trsfm <- function(X,y,i){
	X1 <- X
	
	if(y == 2){
		X1[,i] <- scale(X1[,i])
	}

	if(y == 3){
		X1[,i] <- log(X1[,i] + min(abs(X1[,i])) + 1)
	}

	if(y == 4){
		X1[,i] <- abs(X1[,i])
	}

	if(y == 5){
		X1[,i] <- (X1[,i])**2
	}

	if(y == 6){
		X1[,i] <- (X1[,i]**2) - mean(X1[,i])
	}

	if(y == 7){
		X1[,i] <- (X1[,i]**2) - X1[,i]
	}

	if(y == 8){
		X1[,i] <- (X1[,i]**3)
	}

	if(y == 9){
		X1[,i] <- (X1[,i]**3) - X1[,i]
	}

	if(y == 10){
		X1[,i] <- (X1[,i]**3) - mean(X1[,i])
	}

	if(y == 11){
		X1[,i] <- (X1[,i]**3) - (X1[,i]**2)
	}

	if(y == 12){
		X1[,i] <- sqrt(abs(X1[,i]))
	}

	if(y == 13){
		X1[,i] <- (X1[,i])*(X1[,i+1])
	}
return(X1)
}

# Creating a couple of replicas for testing out the transformations 
train_round1 <- train[-16452,]
test_round1 <- test

#Input the new replicas and only columns 1:59 because the 60th column is the output
for(i in 1:59) {
	train_round1 <- feature_trsfm(train_round1, actions_to_take[i,1],i)
}

for(i in 1:59) {
	test_round1 <- feature_trsfm(test_round1, actions_to_take[i,1],i)
}

#Checking the result (was a trainwreck)
model_round1 <- multinom(popularity ~ ., data = train_round1)
results_round1 <- predict(model_round1, test_round1, "probs")
classes_round1 <- apply(results_round1, 1, which.max)
accuracy_round1 <- (sum(classes_round1 == actualLabels))/9000