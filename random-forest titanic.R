# Set the working directory:
setwd("D:/personal/data science/datasets/titanic")

# Import the data set:
train <- read.csv("D:/personal/data science/datasets/titanic/titanic/train.csv")
test <- read.csv("D:/personal/data science/datasets/titanic/titanic/test.csv")

test$Survived <- NA
combined_set <- rbind(train, test)

# Convert to a string
combined_set$Name <- as.character(combined_set$Name)

# create variable: Title
combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_set$Title <- sub(' ', '', combined_set$Title)

# Combined_set small title groups
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_set$Title[combined_set$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_set$Title[combined_set$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined_set$Title <- factor(combined_set$Title)


# Adding Mother variable
combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18] <- 'Mother'
combined_set$Mother <- factor(combined_set$Mother)

# Adding Child variable
combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'
combined_set$Child <- factor(combined_set$Child)

# Adding Cabin variable
combined_set$Cabin <- as.character(combined_set$Cabin)
strsplit(combined_set$Cabin[2], NULL)[[1]]
combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1])) 

# Engineered variable: Family size
combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1

# Engineered variable: Family
combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined_set$FamilyID <- paste(as.character(combined_set$FamilySize), combined_set$Surname, sep="")

combined_set$FamilyID[combined_set$FamilySize <= 2] <- 'Small'

# Inspect new feature
table(combined_set$FamilyID)

# Removing all erroneous family IDs
famIDs <- data.frame(table(combined_set$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combined_set$FamilyID[combined_set$FamilyID %in% famIDs$Var1] <- 'Small'
combined_set$FamilyID <- factor(combined_set$FamilyID)

#Family size group
combined_set$FamilySizeGroup[combined_set$FamilySize == 1] <- 'single'
combined_set$FamilySizeGroup[combined_set$FamilySize < 5 & combined_set$FamilySize > 1] <- 'Smaller'
combined_set$FamilySizeGroup[combined_set$FamilySize > 4] <- 'large'
combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

# Fill in Age NA's
summary(combined_set$Age)
library(rpart)
FillAge <- rpart(Age ~ Pclass + Mother + FamilySize + Sex + SibSp + Parch + Deck + Fare + Embarked + Title + FamilyID + FamilySizeGroup + FamilySize, 
                 data=combined_set[!is.na(combined_set$Age),], method="anova")
combined_set$Age[is.na(combined_set$Age)] <- predict(FillAge, combined_set[is.na(combined_set$Age),])
summary(combined_set$Age)

# Fill in Embarked blanks
summary(combined_set$Embarked)
table(combined_set$Embarked)
which(combined_set$Embarked == '')
combined_set$Embarked[c(62,830)] = "S"
combined_set$Embarked <- factor(combined_set$Embarked)

# Fill in Fare NA's
summary(combined_set$Fare)
which(is.na(combined_set$Fare))
combined_set$Fare[1044] <- median(combined_set$Fare, na.rm=TRUE)

# New factor for new technique , only allowed <32 levels, so reduce number
combined_set$FamilyID2 <- combined_set$FamilyID

combined_set$FamilyID2 <- as.character(combined_set$FamilyID2)
combined_set$FamilyID2[combined_set$FamilySize <= 3] <- 'Small'
# And convert back to factor
combined_set$FamilyID2 <- factor(combined_set$FamilyID2)

# Check what else might be missing
summary(combined_set)

# Split back into test and train sets
train <- combined_set[1:891,]
test <- combined_set[892:1309,]

library(randomForest)

set.seed(291)
summary(combined_set$Pclass)
rftrain01 <- combined_set[1:891, c("Pclass", "Title")]
rflabel <- as.factor(train$Survived)

# fit1 <- randomForest(x = rftrain01, y = rflabel, importance = TRUE, ntree = 1000)

fit1 <- randomForest(as.factor(Survived) ~ Pclass + Title, data = train, importance = TRUE, ntree = 1000)

fit1
varImpPlot(fit1, main = 'Variable Importance in Titanic Survival Model')

# submission
prediction6 <- predict(fit1, test, type = "class")
submit_6th <- data.frame(PassengerId = test$PassengerId, Survived = prediction6)
write.csv(submit_6th, file = "prediction6.csv", row.names = FALSE)



# Build a new tree with our new features
dtree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID + FamilyID2,
               data=train, method="class")

set.seed(291)

rftrain02 <- combined_set[1:891, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "FamilySize", "FamilySizeGroup", "FamilyID2")]

# fit2 <- randomForest(x = rftrain02, y = rflabel, importance = TRUE, ntree = 1000)
fit2 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID2, data = train, importance = TRUE, ntree = 1000)

fit2

varImpPlot(fit2, main = 'Variable Importance in Titanic Survival Model')


# submission
prediction7 <- predict(fit2, test, type = "class")
submit_7th <- data.frame(PassengerId = test$PassengerId, Survived = prediction7)
write.csv(submit_7th, file = "prediction7.csv", row.names = FALSE)

# package party
install.packages('party')
library(party)

set.seed(291)
# sapply(train, class) # check all data type
# train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor) # convert data type

fit3 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID,
                data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

varimp(fit3)

importance_values <- varimp(fit3)

barplot(importance_values, 
        main = "Variable Importance by Party (cforest)",
        col = "steelblue", 
        las = 2, 
        cex.names = 0.8)


# test$Embarked <- factor(test$Embarked, levels = levels(train$Embarked))
# test$Sex <- factor(test$Sex, levels = levels(train$Sex))
# test$Title <- factor(test$Title, levels = levels(train$Title))
# test$FamilyID <- factor(test$FamilyID, levels = levels(train$FamilyID))

# submission
prediction8 <- predict(object = fit3, newdata = test, OOB=TRUE, type = "response")
submit_8th <- data.frame(PassengerId = test$PassengerId, Survived = prediction8)
write.csv(submit_8th, file = "prediction8.csv", row.names = FALSE)
