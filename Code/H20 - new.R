setwd("/home/max/Escritorio/Data Science/2nd term/Adv. Computational Methods/predicting-online-news-popularity/Data")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)
h2o.init(nthreads = -1)

path1 <- "train_result2.csv"
path2 <- "test_result2.csv"
path3 <- "labels.csv"
training.hex <- h2o.uploadFile(path = path1, destination_frame = "training.hex" )
test.hex <- h2o.uploadFile(path = path2, destination_frame = "test.hex" )
labels.hex <- h2o.uploadFile(path = path3, destination_frame = "labels.hex")
#str(training.hex)
#dim(training.hex)
#class(training.hex) # "Frame"
#quantile(x = training.hex$popularity, probs = (1:100)/100 )
#head(training.hex)
training.hex$popularity <- as.factor(training.hex$popularity)

### GBM
training.gbm <- h2o.gbm(y=32, x=2:31, training_frame= training.hex, ntrees=285, 
                        max_depth=6, min_rows= 20, learn_rate=0.01, 
                        distribution="multinomial", nbins=1024)
training.gbm@model$training_metrics

prediction <- h2o.predict(training.gbm, newdata=test.hex)
pred <- as.data.frame(prediction)
actual <- as.data.frame(labels.hex)[,2]
percent_correct <- sum(pred[,1] == actual) / length(actual)
percent_correct



## Grid search for model comparison
ntrees_opt <- c(5,25,50,100)
maxdepth_opt <- c(2,5,15,35)
learnrate_opt <- c(0.001, 0.05, 0.1, 0.2)
hyper_parameters <- list(ntrees=ntrees_opt, max_depth=maxdepth_opt, learn_rate=learnrate_opt)

grid <- h2o.grid("gbm", hyper_params = hyper_parameters, y = 63, x = 5:62, 
                 distribution="multinomial", training_frame= training.hex, 
                 validation_frame=test.hex )

grid

grid_models <- lapply(grid@model_ids, function(model_id) {model = h2o.getModel(model_id)})
for (i in 1:length(grid_models)) {
    print(sprintf("auc: %f", h2o.auc(grid_models[[i]])))
}

precisions <- rep(0, length(grid_models))
for (i in 1:length(grid_models)) {
    precisions[i] <- h2o.confusionMatrix(grid_models[[i]])[6,6]
}

plot(precisions)
max(precisions)


h2o.shutdown()
