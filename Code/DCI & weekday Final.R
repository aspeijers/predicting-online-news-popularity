# Reading in the data 
train_l <- read.csv("news_popularity_training.csv") 
test_l <- read.csv("news_popularity_test.csv")

weekday_feature1 <- rep(0, nrow(train_l))
for (i in 1:nrow(train_l)){
  if ( train_l[i,34] == 1){
    weekday_feature1[i] <- 4
  }
  else if (train_l[i,35] == 1){
    weekday_feature1[i] <- 2
  }
  else if (train_l[i,36] == 1){
    weekday_feature1[i] <- 1
  }
  else if (train_l[i,37] == 1){
    weekday_feature1[i] <- 3
  }
  else if (train_l[i,38] == 1){
    weekday_feature1[i] <- 5
  }
  else if (train_l[i,39] == 1){
    weekday_feature1[i] <- 6
  }
  else {weekday_feature1[i] <- 7}
}

weekday_feature2 <- rep(0, nrow(test_l))
for (i in 1:nrow(test_l)){
  if ( test_l[i,34] == 1){
    weekday_feature2[i] <- 4
  }
  else if (test_l[i,35] == 1){
    weekday_feature2[i] <- 2
  }
  else if (test_l[i,36] == 1){
    weekday_feature2[i] <- 1
  }
  else if (test_l[i,37] == 1){
    weekday_feature2[i] <- 3
  }
  else if (test_l[i,38] == 1){
    weekday_feature2[i] <- 5
  }
  else if (test_l[i,39] == 1){
    weekday_feature2[i] <- 6
  }
  else {weekday_feature2[i] <- 7}
}

weekday_feature3 <- rep(0, nrow(train_l))
for (i in 1:nrow(train_l)){
  if ( train_l[i,34] == 1){
    weekday_feature3[i] <- 0
  }
  else if (train_l[i,35] == 1){
    weekday_feature3[i] <- -2
  }
  else if (train_l[i,36] == 1){
    weekday_feature3[i] <- -3
  }
  else if (train_l[i,37] == 1){
    weekday_feature3[i] <- -1
  }
  else if (train_l[i,38] == 1){
    weekday_feature3[i] <- 1
  }
  else if (train_l[i,39] == 1){
    weekday_feature3[i] <- 2
  }
  else {weekday_feature3[i] <- 3}
}

weekday_feature4 <- rep(0, nrow(test_l))
for (i in 1:nrow(test_l)){
  if ( test_l[i,34] == 1){
    weekday_feature4[i] <- 0
  }
  else if (test_l[i,35] == 1){
    weekday_feature4[i] <- -2
  }
  else if (test_l[i,36] == 1){
    weekday_feature4[i] <- -3
  }
  else if (test_l[i,37] == 1){
    weekday_feature4[i] <- -1
  }
  else if (test_l[i,38] == 1){
    weekday_feature4[i] <- 1
  }
  else if (test_l[i,39] == 1){
    weekday_feature4[i] <- 2
  }
  else {weekday_feature4[i] <- 3}
}

dci_feature1 <- rep(0, nrow(train_l))
for (i in 1:nrow(train_l)){
  if ( train_l[i,16] == 1){
    dci_feature1[i] <- 2
  }
  else if (train_l[i,17] == 1){
    dci_feature1[i] <- 5
  }
  else if (train_l[i,18] == 1){
    dci_feature1[i] <- 4
  }
  else if (train_l[i,19] == 1){
    dci_feature1[i] <- 1
  }
  else if (train_l[i,20] == 1){
    dci_feature1[i] <- 3
  }
    else if(train_l[i,15] == 1){
    dci_feature1[i] <- 6
  }
  else { dci_feature1[i] <- 0}
}

dci_feature2 <- rep(0, nrow(test_l))
for (i in 1:nrow(test_l)){
  if ( test_l[i,16] == 1){
    dci_feature2[i] <- 2
  }
  else if (test_l[i,17] == 1){
    dci_feature2[i] <- 5
  }
  else if (test_l[i,18] == 1){
    dci_feature2[i] <- 4
  }
  else if (test_l[i,19] == 1){
    dci_feature2[i] <- 1
  }
  else if (test_l[i,20] == 1){
    dci_feature2[i] <- 3
  }
  else if(test_l[i,15] == 1){
    dci_feature2[i] <- 6
  }
  else { dci_feature2[i] <- 0}
}

dci_feature3 <- rep(0, nrow(train_l))
for (i in 1:nrow(train_l)){
  if ( train_l[i,16] == 1){
    dci_feature3[i] <- -2
  }
  else if (train_l[i,17] == 1){
    dci_feature3[i] <- 2
  }
  else if (train_l[i,18] == 1){
    dci_feature3[i] <- 1
  }
  else if (train_l[i,19] == 1){
    dci_feature3[i] <- -3
  }
  else if (train_l[i,20] == 1){
    dci_feature3[i] <- -1
  }
  else if(train_l[i,15] == 1){
    dci_feature3[i] <- 6
  }
  else { dci_feature3[i] <- 0}
}

dci_feature4 <- rep(0, nrow(test_l))
for (i in 1:nrow(test_l)){
  if ( test_l[i,16] == 1){
    dci_feature4[i] <- -2
  }
  else if (test_l[i,17] == 1){
    dci_feature4[i] <- 2
  }
  else if (test_l[i,18] == 1){
    dci_feature4[i] <- 1
  }
  else if (test_l[i,19] == 1){
    dci_feature4[i] <- -3
  }
  else if (test_l[i,20] == 1){
    dci_feature4[i] <- -1
  }
  else if(test_l[i,15] == 1){
    dci_feature4[i] <- 6
  }
  else { dci_feature4[i] <- 0}
}

dci_train <- as.data.frame(cbind(dci_feature1, dci_feature3))
dci_test <- as.data.frame(cbind(dci_feature2, dci_feature4))
weekday_train <- as.data.frame(cbind(weekday_feature1, weekday_feature3))
weekday_test <- as.data.frame(cbind(weekday_feature2,weekday_feature4))

train_newf <- as.data.frame(cbind(dci_train, weekday_train))
test_newf <- as.data.frame(cbind(dci_test, weekday_test))
colnames(train_newf)<- c("feature1", "feature2", "feature3", "feature4")
colnames(test_newf)<- c("feature1", "feature2", "feature3", "feature4")


write.csv(train_newf, "train_dciweekday.csv")
write.csv(test_newf, "test_dciweekday.csv")
