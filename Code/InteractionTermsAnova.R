# Use ANOVA to pick a reasonable subset of interaction terms to include in models.
setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/Data")

contInt <- function(train, test) {
    # read in training data
    train       <- read.csv("../data/news_popularity_training.csv", header = TRUE)
    train       <- train[,3:62]
    X_train     <- train[,1:59]
    Y_train     <- factor(train[,60])

    # read in test data
    test        <- read.csv("../data/news_popularity_test.csv", header = TRUE)
    test        <- test[,3:61]
    X_test      <- test

    # discover class of each feature - "numeric" or "integer"
    varType     <- rep( NA, length(X_train) )

    for ( i in 1:length(X_train) ) {
        varType[i] <- class( X_train[,i] )
    }

    # subset training and test sets for continuous variables
    X_train_cont    <- X_train[,(varType=="numeric")]
    X_test_cont     <- X_test[ ,(varType=="numeric")]

    # define a vector consisting of the names of the continuous variables
    features_cont   <- names(X_train_cont)

    p_cont  <- ncol(X_test_cont)

    # ANOVA - Check whether the means of all possible first order interaction terms are 
    # significantly different. ie whether they exhibit power in separating the classes. 
    term1 <- vector() 
    term2 <- vector()
    interactions_train <- vector()
    interactions_test  <- vector()

    for ( i in 1:p_cont ) {
        if (i<p_cont) {
            for ( j in (i+1):p_cont ) {
            
                # construct potential interaction term
                interact_train <- X_train_cont[,i]*X_train_cont[,j]
            
                # run ANOVA for potential interaction term regressed on the labels
                XX_anov <- aov( interact_train ~ Y_train )
            
                # calculate TukeyHSD to determine which means are significantly different
                # if all (10) means are statistically significantly different at 5% level, accept the interaction variable
                if (sum( TukeyHSD(XX_anov)[[1]][,4] < 0.05 ) == 10) {     # if null is NOT accepted if (p-value < 0.05) (ie means are not equal at significance level 5%), 
                    term1 <- c(term1, features_cont[i])
                    term2 <- c(term2, features_cont[j])
                
                    # add interaction term to data frame - training set
                    interactions_train <- data.frame(cbind(interact_train, interactions_train))
                
                    # add same interaction term for test set 
                    interact_test <- X_test_cont[,i]*X_test_cont[,j]
                    interactions_test <- data.frame(cbind(interact_test, interactions_test))
                }
            }
        }     
    }        

    
write.csv(interactions_train, file = "../data/XX_anov_train3.csv", row.names=FALSE)
write.csv(interactions_test, file = "../data/XX_anov_test3.csv", row.names=FALSE)

#interaction_names <- rep(NA, length(term1))
#for (i in 1:length(term1)) {
 #  interaction_names[i] <- paste(term1[i], "x", term2[i])
#}

#write.csv(interaction_names, file="../Data/interaction_names.csv", row.names=FALSE)
