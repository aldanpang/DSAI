# Lab 6, Machine Learning Methods for Prediction
# Goal: Applying kNN and linear regression to predict sales units using the amounts of money spent on three medias, and explore the impacts of three advertising medias on the sales

library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)

data("marketing", package="datarium" )
#Inspecting summary of variables
summary(marketing)
ggplot(marketing, aes(sales)) +geom_histogram() #Show distribution of outcome variable sales

# Visualising relationship among variables
corrplot(cor(marketing),type="upper",method="color",addCoef.col = "black",number.cex = 0.6)

# Splitting into training and test sets
set.seed(100)
training.idx = sample(1:nrow(marketing), size=nrow(marketing)*0.8)
train.data = marketing[training.idx,]
test.data = marketing[-training.idx,]

# KNN MODEL 
set.seed(101)
knnmodel = train(
  sales~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 4),
  preProcess = c("center","scale"),
  tuneLength = 10)


######## improve this
# Plot model error RMSE vs different values of k
plot(knnmodel)
# Best tuning parameter k that minimize the RMSE
knnmodel$bestTune #returns k = 5
# Make predictions on the test data
predictions<-predict(knnmodel, test.data)
# Compute the prediction error RMSE
RMSE(predictions, test.data$sales)
# RMSE returned is 1.3669

# LINEAR REGRESSION MODEL (lm)
lmodel = lm(sales~., data=train.data)
summary(lmodel)
plot(lmodel)

# Making predictions on test data
predictions = predict(lmodel, test.data)
plot(test.data$sales, predictions, main="Prediction performance of linear regression")
# Adding reference line y=x
abline(0,1, col="red")

RMSE(predictions, test.data$sales)
# Returns 1.95369

# Check residuals from plots, note residual = observed outcome y - fitted outcome ycap. focus on top left bc the rest requires statistics knowledge
par(mfrow=c(2,2))
plot(lmodel)

# Visualise correlation between outcome sales and predictor to form 2nd order terms using predictors highly related to outcome only
corrplot(cor(train.data), type="upper", method="color",addCoef.col = "black",number.cex = 0.6)

# Removing outliers from training data
marketing1 = marketing[-c(131,179,36),]
set.seed(100)
training.idx = sample(1: nrow(marketing1), size=nrow(marketing1)*0.8)
train.data = marketing1[training.idx,]
test.data = marketing1[-training.idx, ]

# Creating 2nd order model, I() function bracket portions of model formula
p2model = lm(sales~youtube + I(youtube^2), data = train.data)

# Making predictions on test data, realise that residual plot doesnt show clear pattern again
predictions = predict(p2model, test.data)
RMSE(predictions, test.data$sales)
plot(p2model)

# Improving model by transforming variable ### FIGURE OUT WHATS WRONG ###
ggplot(train.data, aes("youtube", "sales")) + geom_point()+ stat_smooth(method=lm, formula=y~log(x))

# linear regression with predictor log(sales)
lmodel.1 = lm(sales~log(youtube), data=train.data)
summary(lmodel.1)
