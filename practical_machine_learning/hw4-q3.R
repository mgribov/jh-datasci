# q3
library(elasticnet)
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

# train linear regression with lasso
lass <- train(CompressiveStrength ~ ., data=training, method="lasso")

# plot the decay of the coeffs as lambda (penlaty) increases
plot(lass$finalModel, xvar="penalty", use.color=T)
