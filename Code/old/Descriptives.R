setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/code")

## Read in training data
data <- read.csv("../data/training70.csv", header = TRUE)
X <- data[,4:62]
popularity <- data[,63]

combined <- cbind(X, popularity)
combined$popularity <- as.factor(combined$popularity)

features <- names(X)

## Read in test data
data <- read.csv("../data/test30.csv", header = TRUE)
X_test <- data[,4:62]

## Cleaning
combined <- combined[combined$n_unique_tokens<=1, ] 
combined <- combined[combined$n_non_stop_words<=1, ] 
combined <- combined[combined$n_non_stop_unique_tokens<=1, ] 

## Class of each feature - continuous, count, logical
varType <- rep( NA, length(X) )
for ( i in 1:length(X) ) {
    type <- class( X[,i] )
    if (type=="int") {
        if (length(unique(X[,i])) > 2) {
            varType[i] <- "count"
        } else {
            varType[i] <- "binary"
        }
    } else {
        varType[i] <- type
    }
}

combinedCont <- cbind( combined[,(varType=="numeric")], popularity=as.factor(combined$popularity))
X_testCont <- X_test[ ,(varType=="numeric")]

featuresCont <- names(combinedCont)

## Notes 
# - "n_non_stop_words" classes as numeric but only appears to have values very close to 0 or 1. 


## Plot histograms 
pdf(file="histograms.pdf")
par(mfrow = c(3,3))

counts <- table(popularity)
barplot(counts, main ="Y Distribution")

for (i in 1:58) {
    hist(X[,i], main = names(X)[i])
}
dev.off()


# Calculated how much of distribution falls into cat 1 and 2
(counts[[1]] + counts[[2]]) / sum(counts)                   # 78%


## Plot correlation between Y and each X var
#pdf(file="../images/correlationsXtoY.pdf")
par(mfrow = c(3,3))

for (i in 1:58) {
    plot(X[,i], Y, main=names(X)[i])
}
dev.off()


## Plot feature correlations for continuous features
for ( i in 1:(length(combinedCont)-1) ) {
    for ( j in 2:(length(combinedCont)-1) ) {
        if (j > i) {
            plot(combinedCont[,i], combinedCont[,j], xlab=featuresCont[i], ylab=featuresCont[j], 
                 col=combinedCont$popularity, pch=20)
            legend(x="topleft", legend=levels(combinedCont$popularity),col=1:5, pch=20)
        }
    }
}


## Analysis of variance (ANOVA) - check whether means of each X var are significantly different for each category.
noVarDiff <- vector()                                       #initialise empty vector

for (i in 1:58){
    X.anov <- aov( combined[,i] ~ combined$popularity ) 
    
    if (sum( TukeyHSD(X.anov)[[1]][,4] < 0.05 ) < 5) {     # if null is accepted for all category interactions (ie means are equal at significance level 5%), 
        noVarDiff <- c(noVarDiff, names(X)[i])              # var is not helping us discriminate between categories. Add variable to list. 
    }
}
noVarDiff

combined_updated <- combined[ ,-which(names(X) %in% noVarDiff)]
write.csv(combined_updated, file = "../data/X_anov_train1.csv")

# select same variables from test data
X_test_updated <- X_test[,-which(names(X) %in% noVarDiff)]
write.csv(X_test_updated, file = "../data/X_anov_test1.csv")


## ANOVA for interaction terms - (includes squares!) Note. This is only for continuous features. Could we extend this to all features??
varDiff1 <- vector() # vectors of interaction terms to include (term1 in varDiff1 and term2 in varDiff2)
varDiff2 <- vector()
combined_updated1 <- combined
X_test_updated1 <- X_test
combined_updated2 <- combined
X_test_updated2 <- X_test

for ( i in 1:(length(combinedCont)-1) ) {
    for ( j in i:(length(combinedCont)-1) ) {
        
        interact <- combinedCont[,i]*combinedCont[,j]
        XX.anov <- aov( interact ~ combinedCont$popularity )
        
        if (sum( TukeyHSD(XX.anov)[[1]][,4] < 0.05 ) == 10) {     # if null is NOT accepted if (p-value < 0.05) (ie means are not equal at significance level 5%), 
            varDiff1 <- c(varDiff1, featuresCont[i])
            varDiff2 <- c(varDiff2, featuresCont[j])                # interaction term may help us discriminate between categories. 
            
            combined_updated1 <- data.frame( cbind( interact, combined_updated1 ) )
            interact <- X_testCont[,i]*X_testCont[,j]
            X_test_updated1 <- data.frame( cbind( interact, X_test_updated1 ) )
        }
    }    
}        

# add variables that are NOT included in any interaction term
combined_updated2 <- combined_updated1[, -(which(  (names(combined_updated1) %in% varDiff1) | (names(combined_updated1) %in% varDiff2)  ))]
X_test_updated2 <- X_test_updated1[, -(which(  (names(X_test_updated1) %in% varDiff1) | (names(X_test_updated1) %in% varDiff2)  ))]

write.csv(combined_updated1, file = "../data/XX_anov_train1.csv")
write.csv(X_test_updated1, file = "../data/XX_anov_test1.csv")
write.csv(combined_updated2, file = "../data/XX_anov_train2.csv")
write.csv(X_test_updated2, file = "../data/XX_anov_test2.csv")

## ANOVA for STANDARDISED interaction terms
varDiff1.std <- vector() # vectors of interaction terms to include (term1 in varDiff1 and term2 in varDiff2)
varDiff2.std <- vector()
combined_updated3 <- combined
X_test_updated3 <- X_test

for ( i in 1:(length(combinedCont)-1) ) {
    for ( j in i:(length(combinedCont)-1) ) {
        
        interact.std <- ( (combinedCont[,i] - mean(combinedCont[,i])) / sd(combinedCont[,i]) ) * ( (combinedCont[,j] - mean(combinedCont[,j])) / sd(combinedCont[,j]) )
        XX.std.anov <- aov( interact.std ~ combinedCont$popularity )
        
        if (sum( TukeyHSD(XX.std.anov)[[1]][,4] < 0.05 ) == 10) {     # if null is NOT accepted if (p-value < 0.05) (ie means are not equal at significance level 5%), 
            varDiff1 <- c(varDiff1.std, featuresCont[i])
            varDiff2 <- c(varDiff2.std, featuresCont[j])                # interaction term may help us discriminate between categories. 
            
            combined_updated3 <- data.frame( cbind( interact, combined_updated1 ) )
            X_test_updated3 <- data.frame( cbind( (X_testCont[,i]*X_testCont[,j]), X_test_updated1 ) )
        }
    }    
}        

write.csv(combined_updated1, file = "../data/XX_anov_train1.csv")
write.csv(X_test_updated1, file = "../data/XX_anov_test1.csv")

## ANOVA for interaction terms - (includes squares!) Note. This is for ALL features. 
varDiff1.all <- vector() 
varDiff2.all <- vector()
combined_updated4 <- combined
X_test_updated4 <- X_test

for ( i in 1:(length(combined)-1) ) {
    for ( j in i:(length(combined)-1) ) {
        
        interact.all <- combined[,i]*combined[,j]
        XX.anov.all <- aov( interact ~ combined$popularity )
        
        if (sum( TukeyHSD(XX.anov.all)[[1]][,4] < 0.05 ) == 10) {     # if null is NOT accepted if (p-value < 0.05) (ie means are not equal at significance level 5%), 
            varDiff1.all <- c(varDiff1.all, features[i])
            varDiff2.all <- c(varDiff2.all, features[j])                # interaction term may help us discriminate between categories. 
            
            combined_updated4 <- data.frame( cbind( interact.all, combined_updated3 ) )
            X_test_updated4 <- data.frame( cbind( (X_test[,i]*X_test[,j]), X_test_updated3 ) )
        }
    }    
}        

write.csv(combined_updated1, file = "../data/XX_anov_train4.csv")
write.csv(X_test_updated1, file = "../data/XX_anov_test4.csv")
