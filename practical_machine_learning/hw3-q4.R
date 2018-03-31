library(ElemStatLearn)
data(SAheart)
set.seed(8484)

train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

m <- train(chd ~., data=trainSA, method="glm", family="binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

pred <- predict(m, newdata=testSA)
m_test <- missClass(testSA$chd, pred)
#m_train <- missClass(trainSA$chd, pred)

