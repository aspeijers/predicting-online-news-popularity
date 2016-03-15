setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/Data - too large for git/")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)
h2o.init(nthreads = -1)

### using all transformed features (not XGBoost regression fitted values)
# import data
path1 <- "Transformed_Train.csv"
path2 <- "Transformed_Test.csv"

training.hex <- h2o.uploadFile(path = path1, destination_frame = "training.hex", sep=",")
test.hex <- h2o.uploadFile(path =path2, destination_frame = "test.hex", sep=",")

# change y variable to factor
training.hex$Popularity <- as.factor(training.hex$Popularity)  

# run rf algorithm
RF1 <- h2o.randomForest(y=388, x=1:387, training_frame=training.hex, 
                        ntrees=500, 
                        max_depth=5, 
                        seed=12345, 
                        nfolds=5)

# performance
performance <- h2o.performance(RF1, data=training.hex, valid=TRUE)

### Using original features

setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/Data/")

h2o.init(nthreads = -1)

# import data
path3 <- "news_popularity_training.csv"
path4 <- "news_popularity_test.csv"

training1.hex <- h2o.uploadFile(path = path3, destination_frame = "training1.hex", sep=",")
test1.hex <- h2o.uploadFile(path =path4, destination_frame = "test1.hex", sep=",")

# change y variable to factor
training1.hex$popularity <- as.factor(training1.hex$popularity)  

# run rf algorithm
RF2 <- h2o.randomForest(y=62, x=3:61, training_frame=training1.hex, 
                        ntrees=500, 
                        max_depth=5, 
                        seed=12345, 
                        nfolds=5)

# performance
performance <- h2o.performance(RF2, data=training1.hex, valid=TRUE)



# predictions and accuracy
prediction <- h2o.predict(RF1, newdata=test.hex)
pred <- as.data.frame(prediction)
actual <- as.data.frame(labels.hex)[,2]
percent_correct <- mean(pred[,1] == actual)
percent_correct

# submission
path4 <- "test_accuracyfn.csv"
realTest.hex <- h2o.uploadFile(path =path4, destination_frame = "realTest.hex", sep=",")
submission <- data.frame( id= as.data.frame(realTest.hex$id) )
submission$popularity <- as.data.frame( h2o.predict(RF4, newdata=realTest.hex[,3:52]) )[,1]
write.csv(submission, file="../Submissions/submission_RFH2o1.csv", row.names=FALSE)

#variable importance
varImportance <- h2o.varimp(RF1)
forty.pc <- round( (dim(training.hex)[2]-1) * 0.4 )
fifty.pc <- round( (dim(training.hex)[2]-1) * 0.5 )
sixty.pc <- round( (dim(training.hex)[2]-1) * 0.6 )
seventy.pc <- round( (dim(training.hex)[2]-1) * 0.7 )
eighty.pc <- round( (dim(training.hex)[2]-1) * 0.8 )
ninety.pc <- round( (dim(training.hex)[2]-1) * 0.9 )

top40pc <- varImportance$variable[1:forty.pc]
top50pc <- varImportance$variable[1:fifty.pc]
top60pc <- varImportance$variable[1:sixty.pc]
top70pc <- varImportance$variable[1:seventy.pc]
top80pc <- varImportance$variable[1:eighty.pc]
top90pc <- varImportance$variable[1:ninety.pc]


# run rf algorithm again
RF2 <- h2o.randomForest(y = 90, x = which(names(training.hex) %in% top40pc), training_frame = training.hex, ntrees = 1000, max_depth = 150)
print(RF2)


################# example from the internet #########################
# Create some random data
myframe = h2o.createFrame(rows = 20, cols = 5,
                          seed = -12301283, randomize = TRUE, value = 0,
                          categorical_fraction = 0.8, factors = 10, real_range = 1,
                          integer_fraction = 0.2, integer_range = 10,
                          binary_fraction = 0, binary_ones_fraction = 0.5,
                          missing_fraction = 0.2,
                          response_factors = 1)

# Turn integer column into a categorical
myframe[,5] <- as.factor(myframe[,5])
head(myframe, 20)

# Create pairwise interactions
pairwise <- h2o.interaction(myframe, destination_frame = 'pairwise',
                            factors = list(c(1,2),c("C2","C3","C4")),
                            pairwise=TRUE, max_factors = 10, min_occurrence = 1)
head(pairwise, 20)
h2o.levels(pairwise,2)

# Create 5-th order interaction
higherorder <- h2o.interaction(myframe, destination_frame = 'higherorder', factors = c(1,2,3,4,5),
                               pairwise=FALSE, max_factors = 10000, min_occurrence = 1)
head(higherorder, 20)
