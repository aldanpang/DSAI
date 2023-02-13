library(dplyr)
library(nycflights13)
library(ggplot2)

data("flights", package="nycflights13")

finalF = mutate(flights, gain = arr_delay-dep_delay) %>%
  filter(air_time>550, carrier=="UA")

# final goal is predicting gain of delay by flying time
summary(finalF)

# plotting data
ggplot(finalF, aes(x=air_time, y=arr_gain))+geom_point()

# splitting data
set.seed(100)
training.idx = sample(1:nrow(finalF), nrow(finalF)*0.8)
train.data = finalF[training.idx,]
test.data = finalF[-training.idx,]

# fitting model
library(caret)
set.seed(101)
knnmodel = train(
  gain~air_time, data = train.data, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k=c(5:20)))
knnmodel

# plotting model
ggplot(model) + geom_point()

prediction = predict(knnmodel, test.data)
RMSE(prediction, test.data$gain)
