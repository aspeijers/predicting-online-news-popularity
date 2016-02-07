setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/data")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)
h2o.init(nthreads = -1)

# import data
path1 <- "train_result.csv"
path2 <- "test_result.csv"
path3 <- "labels.csv"
training.hex <- h2o.uploadFile(path = path1, destination_frame = "training.hex", sep=",")
test.hex <- h2o.uploadFile(path =path2, destination_frame = "test.hex", sep=",")
labels.hex <- h2o.uploadFile(path = path3, destination_frame = "labels.hex")

# remove rates >1 from n_unique_tokens, n_non_stop_words, n_non_unique_tokens
#training.hex <- training.hex[training.hex$n_unique_tokens<=1,]
training.hex <- training.hex[training.hex$n_non_stop_words<=1,]
#training.hex <- training.hex[training.hex$n_non_unique_tokens<=1,]

# transform some variables with natural logs
#training.hex[,c(6,7,10,11,12,13,30,31,32,33)] <- log(training.hex[,c(6,7,10,11,12,13,30,31,32,33)])

# change y variable to factor
training.hex$popularity <- as.factor(training.hex$popularity)  

# change binary x variables to factors
#training.hex[,c(16:21,34:41)] <- as.factor(training.hex[,c(16:21,34:41)])
#trainingWithout5.hex[,c(16:21,34:41)] <- as.factor(trainingWithout5.hex[,c(16:21,34:41)])

# run rf algorithm
RF1 <- h2o.randomForest(y = 36, x = 2:35, training_frame = training.hex, ntrees = 500, max_depth = 100)
print(RF1)

# predictions and accuracy
prediction <- h2o.predict(RF1, newdata=test.hex)
pred <- as.data.frame(prediction)
actual <- as.data.frame(labels.hex)[,2]
percent_correct <- mean(pred[,1] == actual)
percent_correct
