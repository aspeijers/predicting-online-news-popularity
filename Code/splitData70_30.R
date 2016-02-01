# change this to suit your working directory
setwd("~/Desktop/BGSE/AdvancedCompMethods/Project/predicting-online-news-popularity")

data <- read.csv("news_popularity_training.csv", header = TRUE)

set.seed(1234)
training70 <- data[sample(nrow(data), 21000), ]
test30 <- data[!(data$id %in% training70$id), ]

write.csv(training70, file = "training70.csv")
write.csv(test30, file = "test30.csv")
