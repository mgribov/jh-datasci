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

# bias in predictor, classe A is most common, double any other classe
# mean for other class counts: 3510 => (3797 + 3422 + 3216 + 3607)/4
#summary(clean)

# reduce instance of the class to mean of counts for other classes
classeA <- clean[which(clean$classe == 'A'), ]
classeA <- classeA[0:3510, ]
clean <- rbind(classeA, clean[which(clean$classe != 'A'), ])

# split the original training set into training and testing sets
inTrain = createDataPartition(clean$classe, p = 3/4)[[1]]
training = clean[ inTrain,]
testing = clean[-inTrain,]


##################################
# train control
fitControl <- trainControl(
  allowParallel = TRUE, 
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10
)


# set up parallel processing 
# per https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1, outfile="") # convention to leave 1 core for OS
registerDoParallel(cluster)

# linux
library(doMC)
registerDoMC(cores = 3)

# @todo: preProcess = "pca"?

# train random forest
rf <- train(
  classe ~ ., 
  data=training, 
  method="rf", 
  prox=TRUE, 
  metric="Kappa", 
  na.action = na.omit,
  trControl=fitControl
)
saveRDS(rf, "~/accel_model_rf.rds")

# train boosted trees
gb <- train(
  classe ~ ., 
  data=training, 
  method="gbm", 
  trControl=fitControl, 
  verbose=FALSE, 
  metric="Kappa",
  na.action = na.omit
)
saveRDS(gb, "~/accel_model_gbm.rds")

# train linear discriminant analysis
ld <- train(
  classe ~ ., 
  data=training, 
  method="lda", 
  metric="Kappa",
  trControl=fitControl, 
  na.action = na.omit
)
saveRDS(ld, "~/accel_model_lda.rds")

# explicitly shutdown parallel cluster
stopCluster(cluster)
registerDoSEQ()


#######################################
# predictions
rf_pred <- predict(rf, testing)
gb_pred <- predict(gb, testing)
ld_pred <- predict(ld, testing)


######################################
# model performance comparison
# accuracy, kappa
confusionMatrix(rf_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(rf_pred, testing$classe)$overall[["Kappa"]]

confusionMatrix(gb_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(gb_pred, testing$classe)$overall[["Kappa"]]

confusionMatrix(ld_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(ld_pred, testing$classe)$overall[["Kappa"]]


######################################
# roc curve
#library(ROCR)
#roc_gb_pred <- prediction(predictions=gb_pred, labels=testing$classe)
#roc_perf_gb <- performance(roc_gb_pred, measure="tpr", x.measure="fpr")

################################
# test: combine gbm and lda models
# result way worse than either gbm or lda alone
predDF <- data.frame(gb_pred, ld_pred, classe=testing$classe)
comboFit <- train(
  classe ~ ., 
  method="gam", # generalized adaptive model using splines
  data=predDF, 
  trControl=fitControl
)
comboPred <- predict(comboFit, predDF)

confusionMatrix(comboPred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(comboPred, testing$classe)$overall[["Kappa"]]
################################


################################
# test some more models and compare to: 
#   gbm - accuracy:0.9671683, kappa:0.9589332 -- took hours to train, ~4 hours
#   rf -- train failed every time, at some point it was running for ~8 hours
# decision trees, both trains took a long time, ~2 hours: 
#   C5.0 - accuracy:0.7200182, kappa:0.6498743
#   rpart - accuracy:0.3896489, kappa:0.2234486
# knn - accuracy:0.9220246, kappa:0.9025003 -- fast to train
# lda - accuracy:0.6972184, kappa:0.6216202 -- fast to train
# regularized logistic regression: regLogistic: accuracy:0.7200182, kappa:0.6498743 -- took very long time, ~6 hours
# suport vector machines: lssvmLinear (least squares svm)
# neuralnet: mxnet
# naive bayes: nb - accuracy:0.753306, kappa:0.6918142 -- fast to train
test_model <- train(
  classe ~ ., 
  data=training, 
  method="gam", 
  metric="Kappa",
  trControl=fitControl, 
  na.action = na.omit
)
test_model_pred <- predict(test_model, testing)
confusionMatrix(test_model_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(test_model_pred, testing$classe)$overall[["Kappa"]]
saveRDS(test_model, "~/accel_model_gam.rds")
#################################

test_model <- readRDS('/home/max/code/jh-datasci-projects/practical_machine_learning/accel_model_lda.rds')
test_model_pred <- predict(test_model, testing)
confusionMatrix(test_model_pred, testing$classe)$overall[["Accuracy"]]
confusionMatrix(test_model_pred, testing$classe)$overall[["Kappa"]]


