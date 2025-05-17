# Set the working directory:
setwd("D:/personal/data science/datasets/titanic")

# Import the data set:
train <- read.csv("D:/personal/data science/datasets/titanic/titanic/train.csv")
test <- read.csv("D:/personal/data science/datasets/titanic/titanic/test.csv")

# install package
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

mytree1 <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(mytree1)

# 
round(prop.table(table(train$Survived)),2)
round(prop.table(table(train$Sex, train$Survived),margin = 1),2)

model <- rpart(Survived ~ Pclass + Age, data=train, method="class")
fancyRpartPlot(model)
rpart.plot(model)


par(xpd = NA) # plotting parameter to allow text or labels to be drawn outside the plot area. Otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)


# gender model
mytree1 <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(mytree1)

# Passenger class + Age model
mytree2 <- rpart(Survived ~ Pclass + Age, data=train, method="class")
fancyRpartPlot(mytree2)

# deeper
mytree3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                   Fare + Embarked, data=train, method="class")
fancyRpartPlot(mytree3)

plot(mytree3)
text(mytree3)


# prediction
prediction4th <- predict(mytree3, test, type = "class")
prediction4 <- data.frame(PassengerId = test$PassengerId, Survived = prediction4th)
write.csv(prediction4, file = "prediction4.csv", row.names = FALSE)

