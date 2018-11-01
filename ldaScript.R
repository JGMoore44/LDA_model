install.packages("MASS")
install.packages("VGAM")
install.packages("caret")
install.packages("e1071")

library(MASS)
library(VGAM)
library(caret)
library(e1071)

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