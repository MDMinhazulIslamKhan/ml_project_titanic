# Set the working directory:
setwd("D:/personal/data science/datasets/titanic")

# Import the data set:
test1 <- read.csv("D:/personal/data science/datasets/titanic/titanic/test1.csv")

# delete data set
rm(test1)


train <- read.csv("D:/personal/data science/datasets/titanic/titanic/train.csv")
test <- read.csv("D:/personal/data science/datasets/titanic/titanic/test.csv")

# View Data raw format:
train

# View Data table format:
View(train)

# View Data Structure:
str(train)


# View Data Summary:
summary(train)
summary(train$Sex)

train$Sex <- as.character(train$Sex) # ⇒ convert result to type
summary(train$Sex)

train$Sex <- as.factor(train$Sex)  # ⇒ convert result to count
summary(train$Sex)


#All column name:
names(train)

#Counts the frequency of each unique value:
table(train$Survived)
table(train$Sex, train$Survived)

#Converts the above counts into proportions:
prop.table(table(train$Survived))
prop.table(table(train$Survived)) * 100 # ⇒ percentage

prop.table(table(train$Sex, train$Survived)) * 100 # ⇒ total values percentage

prop.table(table(train$Sex, train$Survived), margin = 1) * 100 # ⇒ Within each gender, how many survived or not? [ row wise ]

prop.table(table(train$Sex, train$Survived), margin = 2) * 100 # ⇒ Within each survival status, how many are male or female? [ column wise ]

prop.table(table(train$Sex, train$Survived), margin = 1) * 100

# Add column:
test$Survived <- 0
test$Survived <- rep(0, 418)  # ⇒ add a column survived in test data set, value = 0 in 418 row [ best practice ]

# Delete column:
train$Survived <- NULL

# Quickly inspect the data:
head(test)

# Create New Data frame:
prediction <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)


#Save file:
write.csv(prediction, file = "alldies.csv", row.names = FALSE)


# Data munging
# Definition: The process of cleaning, transforming, and preparing raw data into a usable format.

#Empty data to N/A:
train[train == ""] <- NA

#N/A data to empty:
train[is.na(train)] <- ""


# Install package:
install.packages("Amelia") # Amelia:  dealing with missing data

# Load package:
library("Amelia")

# Empty data to N/A:
train[train == ""] <- NA

# Function missmap for Visualize missing data
missmap(train, main="Titanic Training Data - Missings Map", col=c("yellow", "black"), legend=FALSE) # ⇒ Need to see the missing data

# more data visualization
#Bar plot:
barplot(table(train$Sex), xlab="Passenger", ylab="People", main="Train Data Passenger")

# How many survived?
barplot(table(train$Survived), xlab = "Survived", ylab = "People", main = "Train Data Survival")
barplot(table(train$Survived), names.arg = c("Perished", "Survived"), main="Survived (passenger fate)", col="black")

# Passengers travelling in different classes 
barplot(table(train$Pclass), names.arg = c("first", "second", "third"), main="Pclass (passenger traveling class)", col="firebrick")
# How many survived gender-wise?
barplot(table(train$Sex), main="Sex (gender)", col="darkviolet")

# How in group people (sibling+spouse) were traveling?
barplot(table(train$SibSp), main="SibSp (siblings + spouse aboard)", col="darkblue")

# How parents and children were traveling?
barplot(table(train$Parch), main="Parch (parents + kids aboard)", col="gray50")

# Where most people Embarked?
train[is.na(train)] <- "" # with missing data
barplot(table(train$Embarked), names= c("Missing", "Cherbourg", "Queenstown", "Southampton"), main="Port of Embarkation")
train[train == ""] <- NA # without missing data
barplot(table(train$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"), main="Embarked (port of embarkation)", col="sienna")
  

# Histogram plot:
hist(train$Age, main="Age", xlab = NULL, col="brown") # Age distribution in the Titanic
# What was the fair most people paid for Titanic?
hist(train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, col="darkgreen")


# prediction 2
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

prediction2 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(prediction2, file = "2ndprediction.csv", row.names = FALSE)


# prediction 3 start 
# Need to create subsets of Fare
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# Aggregation:
aggregate(Survived ~ Sex, data=train, FUN=sum)
aggregate(Survived ~ Sex, data=train, FUN=length)

aggregate(Survived ~ Sex, data=train, FUN=function(x) {sum(x)/length(x)})
aggregate(Survived ~ Sex, data=train, FUN=function(x) {sum(x)/length(x)} * 100)

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=sum)  # ⇒ ( + ) for add variable


# Let's look at actually how many were they in each subset
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=length)

# comparing subsets in terms of FUN=function(x) {sum(x)/length(x)}
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) 
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x) * 100}) # In percentage


# prediction 3
# Create new column in test set with our prediction that everyone dies
test$Survived <- 0

# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# for woman 5 ==> 100%, 2 ==> 97%, and 8,9 less Survived
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

prediction3 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(prediction3, file = "prediction3.csv", row.names = FALSE)