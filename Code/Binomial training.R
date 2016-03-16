setwd("~/WorkSpace/predicting-online-news-popularity/Code")
binom_train <- read.csv("train_result3.csv")
binom_test <- read.csv("test_result3.csv")
labels <- read.csv("labels.csv")

binom_train1 <- binom_train[,-1]
binom_test1 <- binom_test[,-1]
labels1 <- labels[,-1]

empty <- matrix(0,21000,5)
binom_train2 <- cbind(binom_train1,empty)

for(i in 1:nrow(binom_train2)){
  value <- binom_train2[i,31] 
  binom_train2[i, 31+value] <- 1
  value <- 0
}

binom_reg_1 <- binom_train2[, c(1:30,32)]
colnames(binom_reg_1)[31] <- "predicted"
reg_1 <- glm(predicted ~ ., data = binom_reg_1, family = "binomial")

binom_reg_2 <- binom_train2[, c(1:30,33)]
colnames(binom_reg_2)[31] <- "predicted"
reg_2 <- glm(predicted ~ ., data = binom_reg_2, family = "binomial")

binom_reg_3 <- binom_train2[, c(1:30,34)]
colnames(binom_reg_3)[31] <- "predicted"
reg_3 <- glm(predicted ~ ., data = binom_reg_3, family = "binomial")

binom_reg_4 <- binom_train2[, c(1:30,35)]
colnames(binom_reg_4)[31] <- "predicted"
reg_4 <- glm(predicted ~ ., data = binom_reg_4, family = "binomial")

binom_reg_5 <- binom_train2[, c(1:30,36)]
colnames(binom_reg_5)[31] <- "predicted"
reg_5 <- glm(predicted ~ ., data = binom_reg_5, family = "binomial")

summary(reg_1)
summary(reg_2)
summary(reg_2)
summary(reg_4)
summary(reg_5)

colnames(binom_train1)
# variables:   timedelta + n_tokens_title + n_tokens_content + n_non_stop_words 
#             + n_non_stop_unique_tokens + num_hrefs + num_self_hrefs + num_imgs
#             + num_videos + num_keywords + data_channel_is_lifestyle
#             + data_channel_is_entertainment + data_channel_is_bus 
#             + data_channel_is_socmed + data_channel_is_tech
#             + data_channel_is_world + kw_min_min + kw_avg_max + kw_min_avg 
#             + kw_max_avg + kw_avg_avg + self_reference_max_shares 
#             + self_reference_avg_sharess + is_weekend + global_subjectivity 
#             + avg_negative_polarity + max_negative_polarity + title_sentiment_polarity
#             + abs_title_subjectivity + abs_title_sentiment_polarity

#timedelta + n_tokens_title + n_tokens_content + n_non_stop_words + n_non_stop_unique_tokens + num_hrefs + num_self_hrefs + num_imgs+ num_videos + num_keywords + data_channel_is_lifestyle + data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech + data_channel_is_world + kw_min_min + kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_max_shares + self_reference_avg_sharess + is_weekend + global_subjectivity + avg_negative_polarity + max_negative_polarity + title_sentiment_polarity + abs_title_subjectivity + abs_title_sentiment_polarity



new_reg1 <- glm(predicted ~ timedelta + n_non_stop_words 
                + num_hrefs + num_self_hrefs + num_keywords
                + data_channel_is_lifestyle + data_channel_is_entertainment 
                + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
                + kw_min_min + kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg 
                + self_reference_max_shares + self_reference_avg_sharess + is_weekend 
                + abs_title_subjectivity, data = binom_reg_1, family = "binomial")

new_reg2 <- glm(predicted ~ timedelta + n_non_stop_words + n_non_stop_unique_tokens 
                + num_imgs + data_channel_is_lifestyle 
                + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
                + kw_min_min + kw_avg_max + kw_max_avg 
                + kw_avg_avg + is_weekend, data = binom_reg_2, family = "binomial")

new_reg3 <- glm(predicted ~ n_non_stop_words 
                + num_imgs+ num_videos + data_channel_is_lifestyle + data_channel_is_bus 
                + data_channel_is_socmed + data_channel_is_tech 
                + kw_avg_max + kw_max_avg + kw_avg_avg 
                + is_weekend + global_subjectivity, data = binom_reg_3, family = "binomial")

new_reg4 <- glm(predicted ~ timedelta + n_tokens_title + n_non_stop_words + num_hrefs 
                + num_self_hrefs + data_channel_is_bus 
                + data_channel_is_socmed + kw_min_avg + kw_max_avg + kw_avg_avg 
                + self_reference_max_shares + global_subjectivity 
                + avg_negative_polarity, data = binom_reg_4, family = "binomial")

new_reg5 <- glm(predicted ~ n_tokens_content + n_non_stop_words 
                + num_self_hrefs 
                + self_reference_max_shares, data = binom_reg_5, family = "binomial")

# predicting now 
predicted <- matrix(0,9000,6)
predicted[,1] <- predict(new_reg1, newdata = binom_test1, type = "response") 
predicted[,2] <- predict(new_reg2, newdata = binom_test1, type = "response") 
predicted[,3] <- predict(new_reg3, newdata = binom_test1, type = "response") 
predicted[,4] <- predict(new_reg4, newdata = binom_test1, type = "response") 
predicted[,5] <- predict(new_reg5, newdata = binom_test1, type = "response") 

predicted[,6] <- apply(predicted[,1:5], 1, which.max)

# moment of truth - accuracy 
accuracy <- sum(predicted[,6] == labels[,2])/9000

# 
summary(new_reg1)
summary(new_reg2)
summary(new_reg3)
summary(new_reg4)
summary(new_reg5)

kappa(binom_train1)

