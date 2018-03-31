library(lubridate) # For year() function below
library(forecast)

dat = read.csv("~/code/jh-datasci-projects/practical_machine_learning/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

# train model
m <- bats(tstrain)

# predict
preds <- forecast(m, level=95)

# what values are less than 95% prediction interval
in_range <- testing$visitsTumblr < mean(preds$upper)
length(in_range == TRUE) / dim(testing)[1]

# what about this
#in_range <- testing$visitsTumblr < min(preds$upper)
