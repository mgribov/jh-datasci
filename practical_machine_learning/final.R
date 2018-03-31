library(caret)

set.seed(12345)

dat_test = read.csv("~/code/jh-datasci-projects/practical_machine_learning/pml-testing.csv", na.strings=c("","NA"))
clean_test <- dat_test[,8:160]
clean_test <- clean_test[, colnames(clean_test)[colSums(is.na(clean_test)) == 0]]

# read the csv, make sure to mark empty cols as NA
dat = read.csv("~/code/jh-datasci-projects/practical_machine_learning/pml-training.csv", na.strings=c("","NA"))

# clean up data to have only meaningful columns, first 8 are admin stuff
clean <- dat[,8:160]

# now deal with large num of NA by using only cols without NA (reduce predictors to 53)
clean <- clean[, colnames(clean)[colSums(is.na(clean)) == 0]]

# split the original training set into training and testing sets
inTrain = createDataPartition(clean$classe, p = 3/4)[[1]]
training = clean[ inTrain,]
testing = clean[-inTrain,]

# @todo bias in predictor, classe A is most common, double any other classe?


# preProcess, 
# trControl=trainControl() params to train()
# trainControl()
#   allowParallel = TRUE
#   method = 
#     boot - bootstrapping 
#     boot632 - bootstrapping but adjust to repeated samples (reduce some bias)
#     cv - crossvalidation
#   number - num of samples
#   repeats - num of repeate subsampling (big will be slow)


# train random forest
# na.action = na.omit
rf <- train(classe ~ ., data=training, method="rf", prox=TRUE, trControl=trainControl(allowParallel = TRUE, method="boot"), preProcess = "pca")

# train boosted trees
gb <- train(classe ~ ., data=training, method="gbm", verbose=FALSE, na.action = na.omit)

# train linear discriminant analysis
ld <- train(classe ~ ., data=training, method="lda", na.action = na.omit)

# glm, lasso?

# predictions
rf_pred <- predict(rf, testing)
gb_pred <- predict(gb, testing)
ld_pred <- predict(ld, testing)

# accuracy
confusionMatrix(rf_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(gb_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(ld_pred, testing$classe)$overall[["Accuracy"]]

