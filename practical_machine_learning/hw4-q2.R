# q2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

# prep data, split into train/test
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# train random forest
rf <- train(diagnosis ~ ., data=training, method="rf", prox=TRUE)

# train boosted trees
gb <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)

# train linear discriminant analysis
ld <- train(diagnosis ~ ., data=training, method="lda")

# predictions
rf_pred <- predict(rf, testing)
gb_pred <- predict(gb, testing)
ld_pred <- predict(ld, testing)

# accuracy
confusionMatrix(rf_pred, testing$diagnosis)$overall[["Accuracy"]]
confusionMatrix(gb_pred, testing$diagnosis)$overall[["Accuracy"]]
confusionMatrix(ld_pred, testing$diagnosis)$overall[["Accuracy"]]

# now stack the 3 models using random forest
predDF <- data.frame(gb_pred, rf_pred, ld_pred, diagnosis=testing$diagnosis)
comboFit <- train(diagnosis ~ ., method="rf", data=predDF)
comboPred <- predict(comboFit, predDF)

# accuracy
confusionMatrix(comboPred, testing$diagnosis)$overall[["Accuracy"]]


