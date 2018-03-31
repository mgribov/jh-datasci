set.seed(3523)

library(caret)
library(AppliedPredictiveModeling)
library(e1071)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

m <- svm(CompressiveStrength ~ ., data=training)

pred <- predict(m, testing)

# get rmse
#sqrt(sum( (testing$CompressiveStrength - pred)^2 )) # WRONG
RMSE(pred, testing$CompressiveStrength)
