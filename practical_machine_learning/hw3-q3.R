library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

modFit <- train(Area ~ ., method="rpart", data=olive)

predict(modFit, newdata=newdata)