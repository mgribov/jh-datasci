# q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# as factor
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

# train random forest
rf <- train(y ~ ., data=vowel.train, method="rf", prox=TRUE)

# train boosted trees
gb <- train(y ~ ., data=vowel.train, method="gbm", verbose=FALSE)

# predictions for rf
rf_pred <- predict(rf, vowel.test)

# how many did we get right for rf
rf_match <- vowel.test$y == rf_pred

# predictions for gbm
gb_pred <- predict(gb, vowel.test)

# how many did we get right for gbm
gb_match <- vowel.test$y == gb_pred

# use both models
predDF <- data.frame(gb_pred, rf_pred, y=vowel.test$y)
comboFit <- train(y ~ ., method="gam", data=predDF)
comboPred <- predict(comboFit, predDF)
combo_match <- vowel.test$y == comboPred

# prediction accuracy
length(rf_match[rf_match == TRUE]) / dim(vowel.test)[1] # how many rf got right
length(gb_match[gb_match == TRUE]) / dim(vowel.test)[1] # how many gbm got right

# prediction accuracy as confusion matrix, should be same vals as above
confusionMatrix(rf_pred, vowel.test$y)$overall[["Accuracy"]]
confusionMatrix(gb_pred, vowel.test$y)$overall[["Accuracy"]]

# how many predictions match with both models
#agree_match <- rf_match == gb_match

# combo model
length(combo_match[combo_match == TRUE]) / dim(vowel.test)[1]
confusionMatrix(comboPred, vowel.test$y)$overall[["Accuracy"]]

