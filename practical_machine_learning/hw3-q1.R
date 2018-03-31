library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)

inTrain = createDataPartition(segmentationOriginal$Class, p = 3/4)[[1]]
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[-inTrain,]

#rpart::rpart.control(usesurrogate = 1)

# decision tree model
modFit <- train(Class ~ ., method="rpart", data=training)

# predict all for test data
pred_all <- predict(modFit, newdata=testing)

# make empty df with same column names
t <- testing[0,]

# create test data
t1 <- list(TotalIntenCh2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1 = 2)
t2 <- list(TotalIntenCh2 = 50000, FiberWidthCh1 = 10, VarIntenCh4 = 100)
t3 <- list(TotalIntenCh2 = 57000, FiberWidthCh1 = 8, VarIntenCh4 = 100)
t4 <- list(FiberWidthCh1 = 8, VarIntenCh4 = 100, PerimStatusCh1 = 2)

# add all the test data to new df
t[nrow(t) + 1, names(t1)] <- t1
t[nrow(t) + 1, names(t2)] <- t2
t[nrow(t) + 1, names(t3)] <- t3
t[nrow(t) + 1, names(t4)] <- t4

# run predict
# since the data is misisng most cols (NA), use na.action = na.pass
pred <- predict(modFit, newdata=t, na.action = na.pass)
