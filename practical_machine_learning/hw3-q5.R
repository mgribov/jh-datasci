library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

set.seed(33833)

vowel.test$y = as.factor(vowel.test$y)
vowel.train$y = as.factor(vowel.train$y)

m <- train(y ~ ., data=vowel.train, method="rf")