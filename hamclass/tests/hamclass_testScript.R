# set working directory to data subfolder
setwd("../data")

library(devtools)

install_github( "aspeijers/predicting-online-news-popularity/hamclass", auth_token = "ea6f1377a7e2028cb8a451498859fdc14108c8ba" )
library(hamclass)

# read in unit test data 
train       <- read.csv("trainSet.csv", header=TRUE)
predict     <- read.csv("predictSet.csv", header=TRUE)


# read in labels that should be predicted by code
correctPred     <- read.csv("predictedLabels.csv", header=TRUE)
correctPred     <- as.numeric(correctPred[,1])


# run function hamclass on unit test data
hamclassRF  <- hamclass(train, predict, save.csv = FALSE)


## TESTING
# 1. Test that the hamclass function is producing consistent out-of-bag error
if ( round(hamclassRF$oob_error,2) != 0.00 ) print("Function hamclass is not producing consistent oob_error on unit test data")


# 2. Test that the hamclass function is producing consistent predictions
orderedLabels <- as.numeric( hamclassRF$prediction[order(hamclassRF$prediction$id), 2] )

for ( i in 1:(length(predict$id)) ) {
    if ( orderedLabels[i] != correctPred[i] ) {
        print("Function hamclass is not producing consistent labels on unit test data")
    }
}


