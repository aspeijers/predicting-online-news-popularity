setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/analysis")

data <- read.csv("../data/training70.csv", header = TRUE)
X <- as.matrix(data[,5:62])
Y <- as.factor(data[,63])
Y.binaries <- model.matrix( ~ Y - 1)

weights <- solve(t(X) %*% X) %*% t(X) %*% Y.binaries ## problem here


