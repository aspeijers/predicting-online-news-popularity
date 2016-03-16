setwd("~/WorkSpace/Local/Kaggle Final ")

TRAIN <- read.csv("news_popularity_training.csv") 
TEST <- read.csv("news_popularity_test.csv")

AnnTrain <- read.csv("XX_anov_train3.csv")
AnnTest <- read.csv("XX_anov_test3.csv")

goalx <- read.csv("final_features.csv")
goalx <- goalx[,-1]
goal <- as.character(goalx)

original <- which(colnames(TRAIN) %in% goal)

train_original <- TRAIN[,original]
test_original <- TEST[,original[-length(original)]]

# Update goal 

update <- which(goal  %in% colnames(train_original)) 

goal <- goal[-update]

# Now to update the interaction terms matrices 
interact <- c(1,2,5,7,13,15,18,19,23,24,29,30,31)

new_interact_tr_1 <- (TRAIN[,29])**2
new_interact_te_1 <- (TEST[,29])**2

new_interact_tr_2 <- (TRAIN[,29])*(TRAIN[,50])
new_interact_te_2 <- (TEST[,29])*(TEST[,50])

train_interact <- cbind(AnnTrain[,interact], new_interact_tr_1,
						new_interact_tr_2)

test_interact <- cbind(AnnTest[,interact], new_interact_te_1,
						new_interact_te_2)

interact_names <- paste( "interact.", 1:15, sep = "")

colnames(train_interact) <- interact_names
colnames(test_interact) <- interact_names


# Update goal again 
update_2 <- 5:19

goal <- goal[-update_2]

# Developing feature 1. 
dci_feature1 <- rep(0,nrow(TRAIN))
dci_feature2 <- rep(0,nrow(TEST))

dci_feature1 <- rep(0, nrow(TRAIN))
for (i in 1:nrow(TRAIN)){
  if ( TRAIN[i,16] == 1){
    dci_feature1[i] <- 2
  }
  else if (TRAIN[i,17] == 1){
    dci_feature1[i] <- 5
  }
  else if (TRAIN[i,18] == 1){
    dci_feature1[i] <- 4
  }
  else if (TRAIN[i,19] == 1){
    dci_feature1[i] <- 1
  }
  else if (TRAIN[i,20] == 1){
    dci_feature1[i] <- 3
  }
  else if(TRAIN[i,15] == 1){
  	dci_feature1[i] <- 6
  }
  else { dci_feature1[i] <- 0}
}

dci_feature2 <- rep(0, nrow(TEST))
for (i in 1:nrow(TEST)){
  if ( TEST[i,16] == 1){
    dci_feature2[i] <- 2
  }
  else if (TEST[i,17] == 1){
    dci_feature2[i] <- 5
  }
  else if (TEST[i,18] == 1){
    dci_feature2[i] <- 4
  }
  else if (TEST[i,19] == 1){
    dci_feature2[i] <- 1
  }
  else if (TEST[i,20] == 1){
    dci_feature2[i] <- 3
  }
  else if(TEST[i,15] == 1){
  	dci_feature2[i] <- 6
  }
  else { dci_feature2[i] <- 0}
}

# Update goal again 
goal <- goal[-3]


### Developing the feature matrix with all of the material except 
# xgboost linear predicted values 
train_pre_final <- cbind(train_original, train_interact, dci_feature1)
test_pre_final <- cbind(test_original, test_interact, dci_feature2)

write.csv(train_pre_final, "Train_pre_final.csv")
write.csv(test_pre_final, "Test_pre_final.csv")

###########################################################
# Developing the input matrix for xgboost linear predicitons 
train_xg <- TRAIN[,-c(1,2,62)]
test_xg <- TEST[,-c(1,2)]

# Now begins the mess
train_pre_final <- read.csv("Train_pre_final.csv")
test_pre_final <- read.csv("Test_pre_final.csv")

train_pre_final <- train_pre_final[,-1]
test_pre_final <- test_pre_final[,-1]

xg_train_meta <- read.csv("completion_train.csv")
xg_test_meta <- read.csv("completion_test.csv")

train_final <- cbind(train_pre_final, xg_train_meta[2:4])
test_final <- cbind(test_pre_final, xg_test_meta[2:4])

train_final <- cbind(train_final,TRAIN[,62])
colnames(train_final)[51] <- "popularity"
write.csv(train_final, "train49.csv")
write.csv(test_final, "test49.csv")



colnames(train_final)
colnames(whytry_f)

train_final <- train_final[,-31]
dim(whytry_f)
dim(train_final)
dim(test_final)
colnames(train_final)[46] <- "dci"  
colnames(test_final)[46] <- "dci"

write.csv(train_final, "train49.csv")
write.csv(test_final, "test49.csv")

#############################################################


sqx_final <- c(3,4,43,47)

temptr <- TRAIN[,-c(1,2,62)]
tempte <- TEST[,-c(1,2)]

sqx_final_tr <- ((temptr[,sqx_final])**2) - temptr[,sqx_final]
sqx_final_te <- ((tempte[,sqx_final])**2) - tempte[,sqx_final]

squaredx_final_names <- paste( "square - x", sqx_final)

colnames(sqx_final_tr) <- squaredx_final_names
colnames(sqx_final_te) <- squaredx_final_names


head(sqx_final_tr)

colnames(train_final)

train_final2 <- cbind(train_pre_final, sqx_final_tr)
train_pre_final[,-31] -> train_pre_final

test_final2 <- cbind(test_pre_final, sqx_final_te)
colnames(train_final2)[46] <- "dci"  
colnames(test_final2)[46] <- "dci"

colnames(test_final2) == colnames(train_final2)

train_final2 <- cbind(train_final2,TRAIN[,62])
colnames(train_final2)[51] <- "popularity"

write.csv(train_final2, "train50.csv")
write.csv(test_final2, "test50.csv")

##############################################################3