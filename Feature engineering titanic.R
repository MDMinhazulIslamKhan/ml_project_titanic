# Set the working directory:
setwd("D:/personal/data science/datasets/titanic")

# Import the data set:
train <- read.csv("D:/personal/data science/datasets/titanic/titanic/train.csv")
test <- read.csv("D:/personal/data science/datasets/titanic/titanic/test.csv")

# Join test and train sets for easier feature engineering
test$Survived <- NA
combined_set <- rbind(train, test)


# Convert to a string
combined_set$Name <- as.character(combined_set$Name)

# Creating new variable Child and Adult
combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'

# Show counts
table(combined_set$Child)
table(combined_set$Child, combined_set$Survived)

# Convert to a factor
combined_set$Child <- factor(combined_set$Child) # R models perform better with categorical variables as factors — not as numbers or characters.

# Convert to a string
combined_set$Name <- as.character(combined_set$Name)

# find title from name
combined_set$Name
combined_set$Name[1] # "Braund, Mr. Owen Harris"
strsplit(combined_set$Name[1], split = ",") # "Braund"   " Mr. Owen Harris"
strsplit(combined_set$Name[1], split = "[,.]")
strsplit(combined_set$Name[1], split='[,.]')[[1]] # "Braund"   " Mr"   " Owen Harris"
strsplit(combined_set$Name[1], split = "[,.]")[[1]][2] # " Mr"

# create variable: Title
combined_set$Title <- strsplit(combined_set$Name, split='[,.]')[[1]][2]  # Won't work!
combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_set$Title <- sub(' ', '', combined_set$Title)

table(combined_set$Title)

# Combined_set small title groups
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_set$Title[combined_set$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_set$Title[combined_set$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

table(combined_set$Title)

# convert factor
combined_set$Title <- factor(combined_set$Title)

# Adding Mother variable
combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18 & combined_set$Title != 'Miss'] <- 'Mother'

# Convert to a factor
combined_set$Mother <- factor(combined_set$Mother)

# Show counts
table(combined_set$Mother)
table(combined_set$Mother, combined_set$Survived)

mother_data <- rpart(Survived ~ Mother, data=combined_set, method="class")
fancyRpartPlot(mother_data,caption = "Mother Death Ratio")

# fare_type
combined_set$Fare_type<-NULL
combined_set$Fare_type[combined_set$Fare<=50]<-"low"
combined_set$Fare_type[combined_set$Fare>50 & combined_set$Fare<=100]<-"med1"
combined_set$Fare_type[combined_set$Fare>100 & combined_set$Fare<=150]<-"med2"
combined_set$Fare_type[combined_set$Fare>150 & combined_set$Fare<=500]<-"high"
combined_set$Fare_type[combined_set$Fare>500]<-"vhigh"

aggregate(Survived~Fare_type, data=combined_set,mean)
aggregate(Survived~Fare_type, data=combined_set,sum) 
aggregate(cbind(Survived, Not_Survived = 1 - Survived) ~ Fare_type, data = combined_set, sum)

# make factor for for barplot
combined_set$Fare_type <- factor(combined_set$Fare_type,
                                 levels = c("low", "med1", "med2", "high", "vhigh"))

barplot(
  aggregate(Survived ~ Fare_type, data = combined_set, mean)[,2],
  names.arg = c("low", "med1", "med2", "high", "vhigh"),
  col = "steelblue",
  main = "Survival Rate by Fare Category",
  ylab = "Survival Rate"
)

# shortcut or simplify category creation [ best practice ]
combined_set$Fare_type <- cut(combined_set$Fare,
                              breaks = c(-Inf, 50, 100, 150, 500, Inf),
                              labels = c("low", "med1", "med2", "high", "vhigh"))

barplot(
  aggregate(Survived ~ Fare_type, data = combined_set, mean)[,2],
  names.arg = c("low", "med1", "med2", "high", "vhigh"),
  col = "steelblue",
  main = "Survival Rate by Fare Category",
  ylab = "Survival Rate"
)

# Cabin
combined_set$Cabin <- as.character(combined_set$Cabin)
strsplit(combined_set$Cabin[2], NULL)[[1]]
combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1])) 


# Convert Cabin to a factor
combined_set$Cabin <- factor(combined_set$Cabin)

aggregate(Survived~Deck, data=combined_set,mean)
aggregate(Survived~Deck, data=combined_set,sum) 
aggregate(cbind(Survived, Not_Survived = 1 - Survived) ~ Deck, data = combined_set, sum)

barplot(table(combined_set$Deck),
        main = "Passenger Count by Deck",
        xlab = "Deck",
        ylab = "Count",
        col = "skyblue")


barplot(
  aggregate(Survived ~ Deck, data = combined_set, mean)[, 2],
  names.arg = levels(combined_set$Deck),
  col = "darkgreen",
  main = "Survival Rate by Deck",
  ylab = "Survival Rate"
)

# variable for Family size
combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1

aggregate(Survived~FamilySize, data=combined_set,mean)
aggregate(Survived~FamilySize, data=combined_set,sum) 
aggregate(cbind(Survived, Not_Survived = 1 - Survived) ~ FamilySize, data = combined_set, sum)


# variable for Family size group
combined_set$FamilySizeGroup <- cut(combined_set$FamilySize,
                              breaks = c(0, 1, 4, Inf),
                              labels = c("Single", "Smaller", "Large"))

aggregate(Survived~FamilySizeGroup, data=combined_set,mean)
aggregate(Survived~FamilySizeGroup, data=combined_set,sum) 
aggregate(cbind(Survived, Not_Survived = 1 - Survived) ~ FamilySizeGroup, data = combined_set, sum)

barplot(
  aggregate(Survived ~ FamilySizeGroup, data = combined_set, mean)[,2],
  col = "#3ef0bd",
  names.arg = levels(combined_set$FamilySizeGroup),
  main = "Survival Rate by Family Size",
  ylab = "Survival Rate"
)

mosaicplot(table(combined_set$FamilySizeGroup, combined_set$Survived), main='Survival affected by Family Size', shade=TRUE)

# variable for Family
strsplit(combined_set$Name[1], split = "[,.]")[[1]][1] # "Braund"
combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combined_set$FamilyID <- paste(as.character(combined_set$FamilySize), combined_set$Surname, sep="")

table(combined_set$FamilyID)
table(combined_set$FamilySizeGroup)

combined_set$FamilyID <- factor(combined_set$FamilyID)
combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

# Split back into test and train sets
train <- combined_set[1:891,]
test <- combined_set[892:1309,]

# load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + Mother + SibSp + Parch + Deck + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

fancyRpartPlot(fit, caption = "Titanic Survival Decision Tree")

# for save better image
png("image/8-fe.png", width = 1200, height = 900)
fancyRpartPlot(fit, caption = "Titanic Survival Decision Tree")
dev.off()


# prediction and write a submission file
prediction5 <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction5)
write.csv(submit, file = "prediction5.csv", row.names = FALSE)

### data processing and data cleaning 
# Fill in Age NA's
summary(combined_set$Age)

FillAge <- rpart(Age ~ Pclass + Mother + FamilySize + Sex + SibSp + Parch + Deck + Fare + Embarked + Title + FamilyID + FamilySizeGroup + FamilySize, 
                 data=combined_set[!is.na(combined_set$Age),], method="anova")

combined_set$Age[is.na(combined_set$Age)] <- predict(FillAge, combined_set[is.na(combined_set$Age),]) # anova used for numeric outcome prediction like Age
summary(combined_set$Age)

png("image/9-fe.png", width = 1200, height = 900)
rpart.plot(FillAge, main = "Regression Tree: Age ~ Survived")
dev.off()

# Fill in Embarked blanks
summary(combined_set$Embarked)
which(combined_set$Embarked == '') # get the position of ''
combined_set$Embarked[c(62,830)] = "S" # add 'S' in 62 and 830 no column
combined_set$Embarked <- factor(combined_set$Embarked)

# Fill in Fare NA's
summary(combined_set$Fare)
which(is.na(combined_set$Fare))
combined_set$Fare[1044] <- median(combined_set$Fare, na.rm=TRUE) # add median value in 1044 position
summary(combined_set$Fare)

# New factor for new technique , only allowed <32 levels, so reduce number
combined_set$FamilyID2 <- combined_set$FamilyID

# Convert back to string
combined_set$FamilyID2 <- as.character(combined_set$FamilyID2)
combined_set$FamilyID2[combined_set$FamilySize <= 3] <- 'Small'
# And convert back to factor
combined_set$FamilyID2 <- factor(combined_set$FamilyID2)


# Check what else might be missing
summary(combined_set)

# Split back into test and train sets
train <- combined_set[1:891,]
test <- combined_set[892:1309,]

# create model with random Forest
install.packages('randomForest')
library(randomForest)

set.seed(291)

rftrain01 <- combined_set[1:891, c("Pclass", "Title")]
rflabel <- as.factor(train$Survived)


fit1 <- randomForest(x = rftrain01, y = rflabel, importance = TRUE, ntree = 1000)
fit1
varImpPlot(fit1, main = 'Variable Importance in Titanic Survival Model')

# another train
set.seed(42)
rftrain1 <- combined_set[1:891, c("Pclass", "Sex", "Age", "Fare", "Title")]
rflabel1 <- as.factor(train$Survived)

fit_rf <- randomForest( x = rftrain1, y = rflabel1, importance = TRUE, ntree = 500 )

print(fit_rf)
varImpPlot(fit_rf, main = "Variable Importance in Titanic Survival Prediction")

importance(fit_rf)

#           0         1         MeanDecreaseAccuracy   MeanDecreaseGini
#Pclass 21.41614 40.490376             44.86731         38.94585
#Sex    21.35010  8.001462             22.50083         52.14517
#Age    16.81155 20.222425             27.11458         51.79402
#Fare   11.64383 28.588604             30.52794         66.02975
#Title  37.79828 17.658981             39.21553         83.54642

# MeanDecreaseAccuracy: How much worse does the model perform if we remove or mess up this feature?
# MeanDecreaseGini: How good is this feature at splitting the data cleanly inside the trees?

#  | Term                     | Like...                                            | What it tells you                            |
#  | ------------------------ | -------------------------------------------------- | -------------------------------------------- |
#  | **MeanDecreaseAccuracy** | *How much a car slows down when you remove a part* | Measures **overall importance** for accuracy |
#  | **MeanDecreaseGini**     | *How sharp a knife is when cutting food*           | Measures **how cleanly it splits the data**  |
  

#  | Feature    | Key Insight                                                 |
#  | ---------- | ----------------------------------------------------------- |
#  | **Title**  | Most important overall — shows passenger status/social role |
#  | **Fare**   | Splits the data very well — related to wealth/class         |
#  | **Sex**    | Strong classifier — tells who survived or not               |
#  | **Pclass** | Good predictor — survival tied to class                     |
#  | **Age**    | Helpful but not as critical as the above                    |
  
