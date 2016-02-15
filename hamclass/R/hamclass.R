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
    
    train.hex   <- as.h2o( train, destination_frame="train.hex" )
    predict.hex    <- as.h2o( predict, destination_frame="predict.hex" )
    
    ## convert label to factor
    k               <- dim( predict.hex )[2]
    train.hex[,k+1] <- as.factor( train.hex[,k+1] ) 
    
    ## run RF - add grid search
    RF          <- h2o.randomForest( y = (k+1), x = 2:(k), training_frame = train.hex, ntrees = 1000, max_depth = 100, mtries = 10 )
    
    ## out of bag error
    nclasses    <- length( unique(train[,k+1]) )
    oob_error   <- RF@model$training_metrics@metrics$cm$table[ nclasses + 1, nclasses + 1]
    
    ## predict
    results                 <- h2o.predict( RF, newdata=predict.hex[,2:k] )
    prediction              <- data.frame( id=predict[,1] )
    prediction$popularity   <- as.data.frame( results )[,1]
    
    ## save csv to working directory
    if ( save.csv==TRUE )   write.csv( prediction, "prediction.csv", row.names=FALSE )

    ## output
    list <- list( prediction=prediction, oob_error=oob_error )
}