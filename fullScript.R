#Full Script
install.packages("class")
install.packages("MASS")
install.packages("VGAM")
install.packages("caret")

library(MASS)
library(VGAM)
library(caret)
library(class)

#load traing set
trainSet = read.csv("C:/Users/James Moore/Documents/LDA_model/dataFiles/PA_HW1_train.csv")
#load test set
testSet = read.csv("C:/Users/James Moore/Documents/LDA_model/dataFiles/PA_HW1_test.csv")

#knn with k = 1 on training
modelTrain = knn(cbind(trainSet$x1,trainSet$x2),
                 cbind(trainSet$x1,trainSet$x2),
                 cl = trainSet$col,
                 k = 1,
                 prob = TRUE)
table(modelTrain,trainSet$col)

#knn with k = 1 on test
modelTest = knn(cbind(trainSet$x1,trainSet$x2),
                cbind(testSet$x1,testSet$x2),
                cl = trainSet$col,
                k = 1,
                prob = TRUE)

table(modelTest,testSet$col)

#initialize error rate vectors
errorRateTrain = numeric(100)
errorRateTest = numeric(100)

#begin loop for k = 1 to 100
for (kVal in 1:100) {
  #knn with k = kVal on training
  modelTrain = knn(cbind(trainSet$x1,trainSet$x2),
                   cbind(trainSet$x1,trainSet$x2),
                   cl = trainSet$col,
                   k = kVal,
                   prob = TRUE)
  #knn with k = kVal on test
  modelTest = knn(cbind(trainSet$x1,trainSet$x2),
                  cbind(testSet$x1,testSet$x2),
                  cl = trainSet$col,
                  k = kVal,
                  prob = TRUE)
  
  #Calculate Error Rate
  tabTrain = table(modelTrain,trainSet$col)
  errorRateTrain[kVal] = 1-sum(diag(tabTrain))/sum(tabTrain)
  
  tabTest = table(modelTest,testSet$col)
  errorRateTest[kVal] = 1-sum(diag(tabTest))/sum(tabTest)
}

#plot
library(ggplot2)
dat  = data.frame(k = rep(seq(1:100),2),
                  errorRates = c(errorRateTrain,errorRateTest),
                  level = factor(c(rep("Train",100),rep("Test",100))))
ggplot(data = dat, aes(x = k, y = errorRates, color = level))+
  geom_line(size = 1.5)+
  ggtitle("Error Rate By K value")+
  xlab("K Nearest Neighbors")+
  ylab("Error Rate (%)")


###########
## LDA Portion

#Plot Sepal Width as a Function of Sepal Length based on species from iris data
ggplot(data = iris, aes(x = Sepal.Length,y = Sepal.Width,color = Species))+
  geom_point(size = 2)+
  ggtitle("Dimensions of Sepal")+
  xlab("Length")+
  ylab("Width")

#Build Model
ldaModel = lda(Species~Sepal.Width+Sepal.Length,data = iris)
#Make Predictions
out = predict(ldaModel,iris)
#Output Confusion Matrix
confusionMatrix(out$class,iris$Species)

##Visualize Results
# Grid of classifications
pred = expand.grid(seq(4,8.5,0.1),seq(1.5,5,0.1))
names(pred) = c("Sepal.Length","Sepal.Width")
out = predict(ldaModel,pred)

dat = data.frame(x = pred$Sepal.Length,
                 y = pred$Sepal.Width,
                 level = factor(out$class))

ggplot()+
  geom_point(data = iris, aes(x = Sepal.Length,y = Sepal.Width,color = Species),size = 2.5)+
  geom_point(data = dat, aes(x = x,y = y, color = level),size = 1, shape = 3)+
  ggtitle("Dimensions of Sepal")+
  xlab("Length")+
  ylab("Width")















