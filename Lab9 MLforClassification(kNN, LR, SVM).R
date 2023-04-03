# ML for classification (3rd method): Support Vector Machine (SVM)
# Note first and second methods are logistic regression and kNN
# Intuition: kind of like linear regression, but now we extend concept to hyperplane (for nonlinear SVM) to split the two categories, then we maximise distance between the closest data points

install.packages("e1071") # contains function svm()
install.packages("mlbench") # getting PimaIndiansDiabetes dataset
install.packages("dplyr") # for data preparation
install.packages("class")

library("e1071")
library("mlbench")
library("dplyr")
library("class")

data(PimaIndiansDiabetes, package="mlbench")

#1 Familiarising with dataset and data prep
pid = PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),] # removing missing values
pid[,1:8] = sapply(pid[,1:8], as.numeric) # converting all variables to numerical ones
pid = pid%>%mutate(y=factor(ifelse(diabetes=="pos", 1,0)))%>% 
  select(1:8, y) #selecting predictors and Y for classification

#2 Train-test split
set.seed(100)
training.idx = sample(1: nrow(pid), size=nrow(pid)*0.8)
train.data = pid[training.idx, ]
test.data = pid[-training.idx, ]

#3(a) [Classification] Logistic Regression
mlogit = glm(y~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age, data = train.data, family = "binomial") # performing logistic regression
summary(mlogit)

Pred.p = predict(mlogit, newdata =test.data, type = "response") # probability of P(Y=1)
y_pred_num = ifelse(Pred.p > 0.5, 1, 0) # 0.5 as threshold value. If greater than 0.5, then event (Y=1) will happen
y_pred = factor(y_pred_num, levels=c(0, 1))
mean(y_pred ==test.data$y ) 
table(y_pred,test.data$y) #confusion matrix
# Observations: Logistic regression correctly classified patients with accuracy of 76%

#3(b) [Classification] kNN 
nor = function(x) { (x -min(x))/(max(x)-min(x)) } #normalising numeric variables
pid[,1:8] = sapply(pid[,1:8], nor)

set.seed(101)
knn1 = knn(train.data[,1:8], test.data[,1:8], cl=train.data$y, k=14)
mean(knn1 ==test.data$y)
table(knn1,test.data$y)
# Observations: When testing values of k = [2:25], it was found that the best value for k is 14. Accuracy for correct classification with 
# this k is 79%

#3(c) [Classification] SVM 
#3(c)(i) SVM (Linear)
m.svm = svm(y~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age, data = train.data, kernel = "linear")
summary(m.svm)
pred.svm = predict(m.svm, newdata=test.data[,1:8]) # predict new data in test set
table(pred.svm, test.data$y) # evaluate classification performance and check accuracy
mean(pred.svm ==test.data$y)
# Observations: SVM with linear kernel correctly classified patients with accuracy of 76%

#3(c)(ii) SVM (nonlinear - radial)
set.seed(123)
m.svm.tune = tune.svm(y~., data=train.data, kernel="radial", cost=10^(-1:2), gamma=c(.1,.3,2,4)) # tuning best parameters for nonlinear kernel function in SVM
# Note cost and gamma can be tuned to change accuracy of classification

summary(m.svm.tune)
plot(m.svm.tune) #visualize results of parameter tuning
# confusion matrix and accuracy
best.svm = m.svm.tune$best.model 
pred.svm.tune = predict(best.svm, newdata=test.data[,1:8])
table(pred.svm.tune, test.data$y)
mean(pred.svm.tune ==test.data$y)
# Observations: SVM with nonlinear kernel has classification accuracy of 77% after leaving cost as (10^-1: 10^-2), and changing gamma values to the above.

# In summary, the classification accuracy are as follows for each classification method
# Logistic Regression: 76%
# kNN (with best k value): 79%
# SVM (Linear Kernel): 76%
# SVM (Radial Kernel): 77%

# Furthermore, misclassification rates are as follows:
# Logistic Regression: (11+26)/(87+30+11+26) *100 = 24%
# kNN (with best k value): (8+25)/(90+31+25+8) *100 = 21%
# SVM (Linear Kernel): (13+24)/(85+32+24+13) *100 = 24%
# SVM (Radial Kernel): (11+33)/(87+23+11+33) *100 = 29%

# From this, we can see that kNN classification is the best option with the highest accuracy and lowest misclassification rate
# Logistic regression and SVM with linear kernel are equal in classification accuracy and misclassification
# SVM with radial kernel can be seen as the worse performing with the highest misclassification rate amongst the 4 methods, yet with only
# 1% better accuracy for classification


