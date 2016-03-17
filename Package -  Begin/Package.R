#####################################################################################################
# ----------------------------------------------------------------------
# H2o Implementation of Random Forest for Multiclass Output
# ----------------------------------------------------------------------
#' Random Forest Classifier
#' 
#' Classify the input with a random forest classifier.
#'
#' @param train A data frame or a matrix where rows are observations and columns are features and the label. The label is the final column.
#' @param predict A data frame or a matrix where rows are observations and columns are features. 
#' @param save.csv A TRUE or FALSE variable defining whether a .csv file with the id and predicted labels is to be saved or not. File will be saved to current working directory. 
#' @param seed A seed to use to enable reproducibility of output. 
#' @return A list with the following elements: data frame with id and predicted popularity class, out of bag error.  
#' @export
#' @import assertthat 
#' @import h2o
#' @examples
#' # create artificial dataset
#' inputsTrain   <- cbind( matrix(rnorm(200), ncol=2), sample( c(1,2,3,4,5), 100, replace=TRUE) )
#' inputsPredict  <- matrix(rnorm(200), ncol=2)
#' classesTrain <- c(rep(0, 50), rep(1, 50))
#' # get the hamclass predictions for the predict set and save them in a .csv file in the current working directory
#' classes  <- hamclass(inputsTrain, inputsPredict, save.csv=TRUE)
#' classes$prediction
#' # get the hamclass out of bag error for the training set
#' classes$oob_error

hamclass <- function(train, predict, save.csv=FALSE, seed=12345) {
  
  ## install dependent packages
  if (!require("assertthat")) install.packages("assertthat")
  library(assertthat)
  
  if (!require("h2o")) {
    # download packages that H2O depends on.
    pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
    for (pkg in pkgs) {
      if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
    }
    
    # download, install and initialize the H2O package for R.
    install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tukey/2/R")))
    
  }
  library(h2o)
  
  ## check inputs
  not_empty(train); not_empty(predict)
  assert_that( is.data.frame(train) | is.matrix(train) )
  assert_that( is.data.frame(predict) | is.matrix(predict) )
  assert_that( ncol(predict) == (ncol(train)-1) )
  assert_that ( save.csv %in% c("TRUE", "FALSE") )
  is.count( seed )
  
  ## initialise H2o
  h2o.init( nthreads = -1 )
  
  ## convert inputs to h2o frame object
  train       <- as.data.frame( train )
  predict       <- as.data.frame( predict )
  
  ## Creating two new data frames: train without popularity labels and a replica of test
  train.mod <- train[, -62 ]
  test.mod <- predict
  
  ## Developing a data frame with the features used from the original features matrix
  original <- c(3,5,6,8,9,11,13,22,23,24,26,27,28,29,32,40,41,42,43,44,45,46,47,48,49,52,53,55,56,59)
  train.ori <- train.mod[,original]
  test.ori <- test.mod[,original]
  
  ## Developing a data frame with the square - x terms 
  sqx <- c(5,6,45,49)
  
  train.sqx <- ((train.mod[,sqx])**2) - train.mod[,sqx]
  test.sqx <- ((test.mod[,sqx])**2) - test.mod[,sqx]
  
  sqx_names <- paste( "square - x", sqx)
  
  colnames(train.sqx) <- sqx_names
  colnames(test.sqx) <- sqx_names
  
  ## Developing 3 terms: dci feature and the interaction terms outside of the anova function  
  interact.train.1 <- (train.mod[,29])**2
  interact.test.1 <- (test.mod[,29])**2
  
  interact.train.2 <- (train.mod[,29])*(train.mod[,50])
  interact.test.2 <- (test.mod[,29])*(test.mod[,50])
  
  # dci_feature
  dci_feature1 <- rep(0, nrow(train.mod))
  for (i in 1:nrow(train.mod)){
    if ( train.mod[i,16] == 1){
      dci_feature1[i] <- 2
    }
    else if (train.mod[i,17] == 1){
      dci_feature1[i] <- 5
    }
    else if (train.mod[i,18] == 1){
      dci_feature1[i] <- 4
    }
    else if (train.mod[i,19] == 1){
      dci_feature1[i] <- 1
    }
    else if (train.mod[i,20] == 1){
      dci_feature1[i] <- 3
    }
    else if(train.mod[i,15] == 1){
      dci_feature1[i] <- 6
    }
    else { dci_feature1[i] <- 0}
  }
  
  dci_feature2 <- rep(0, nrow(test.mod))
  for (i in 1:nrow(test.mod)){
    if ( test.mod[i,16] == 1){
      dci_feature2[i] <- 2
    }
    else if (test.mod[i,17] == 1){
      dci_feature2[i] <- 5
    }
    else if (test.mod[i,18] == 1){
      dci_feature2[i] <- 4
    }
    else if (test.mod[i,19] == 1){
      dci_feature2[i] <- 1
    }
    else if (test.mod[i,20] == 1){
      dci_feature2[i] <- 3
    }
    else if(test.mod[i,15] == 1){
      dci_feature2[i] <- 6
    }
    else { dci_feature2[i] <- 0}
  }
  
  # Add the anova function here 
  
  train.anova <- # extract the same terms as xx_anov_train3 into this (without id)
  test.anova <- # extrac the same terms as xx_anov_test3 into this (without id)
  
  # putting the dci feature and interaction terms into one dataframe 
    
  interact <- c(1,2,5,7,13,15,18,19,23,24,29,30,31)
  
  train.newterms <- cbind(train.anova[,interact], interact.train.1, interact.train.2, dci_feature1)
  test.newterms <- cbind(test.anova[,interact], interact.test.1, interact.test.2, dci_feature2)
  
  # names
  
  interact_names <- paste( "interact.", 1:15, sep = "")
  interact_names <- c(interact_names, "dci")
  
  colnames(train.newterms) <- interact_names
  colnames(test.newterms) <- interact_names
  
  ### Putting everything together
  training <- cbind(train.ori, train.sqx, train.newterms, train[,62])
  testing <- cbind(test.ori, test.sqx, test.newterms)
  
  colnames(training)[col(training)] <- "popularity"
  
  # converting it into h2o object
  train.hex   <- as.h2o( training, destination_frame="train.hex" )
  test.hex    <- as.h2o( testing, destination_frame="test.hex" )
  
  ## convert label to factor
  k               <- dim( test.hex )[2]
  train.hex[,k+1] <- as.factor( train.hex[,k+1] ) 
  
  ## run RF - add grid search
  GBM                     <- h2o.gbm(y = (k+1), x = 1:(k), training_frame = train.hex, 
                                     ntrees=350, max_depth=5, min_rows= 14, learn_rate=0.025, 
                                     distribution="multinomial")
  
  ## out of bag error
  #nclasses    <- length( unique(train[,k+1]) )
  #oob_error   <- RF@model$training_metrics@metrics$cm$table[ nclasses + 1, nclasses + 1]
  
  ## predict
  results                 <- h2o.predict( GBM, newdata = test.hex )
  prediction              <- data.frame( id=predict[,1] )
  prediction$popularity   <- as.data.frame( results )[,1]
  
  ## save csv to working directory
  if ( save.csv==TRUE )   write.csv( prediction, "prediction.csv", row.names=FALSE )
  
  ## output
  list <- list( prediction=prediction, oob_error=oob_error )
}