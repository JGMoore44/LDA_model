install.packages("class")

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