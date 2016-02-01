setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity/code")

data <- read.csv("../data/training70.csv", header = TRUE)
X <- data[,5:62]
Y <- data[,63]
combined <- cbind(Y, X)


## Plot histograms 
pdf(file="histograms.pdf")
par(mfrow = c(3,3))

counts <- table(Y)
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

## Plot correlations between X vars
pdf(file="../images/correlations.pdf")
par(mfrow=c(3,3))

for (i in 1:59) {
    for (j in 1:59) {
        plot(combined[,i], combined[,j])
    }
}
dev.off()


## Analysis of variance (ANOVA) - check whether means of each X var are significantly different for each category.
combined[,1] <- as.factor(combined[,1])

noVarDiff <- vector()                                       #initialise empty vector

for (i in 1:58){
    X.anov <- aov( X[,i] ~ combined[,1] ) 
    
    if (sum( TukeyHSD(X.anov)[[1]][,4] < 0.05 ) == 0) {     # if null is accepted for all category interactions (ie means are equal at significance level 5%), 
        noVarDiff <- c(noVarDiff, names(X[i]))              # var is not helping us discriminate between categories. Add variable to list. 
    }
}
noVarDiff

TukeyHSD(aov(X$kw_avg_avg ~ combined[,1]))
