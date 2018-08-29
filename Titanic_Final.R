#Andrew Dahbura, amd6ua
#SYS 6018 Titanic Kaggle

setwd("~/Desktop/SYS 6018/Kaggle/Titanic")
install.packages("tidyverse")
library(tidyverse)

#read prediction set from csv
pred <- read_csv("test.csv")
#read training set from csv
train <- read_csv("train.csv")


#Divide data into 80% train and 20% test for validation
sub <- sample(1:891,size=714)
train.training <- train[sub,]     # Select subset for cross-validation
train.testing <- train[-sub,]

#Remove name
train.training <- train.training[,-c(4)]

#Variables Pclass, Sex, and Embarked are categorical
#so need to convert them to factors
train.training$Pclass <- factor(train.training$Pclass)
train.training$Sex <- factor(train.training$Sex)
train.training$Embarked <- factor(train.training$Embarked)

#Remove PassengerID, Ticket and Cabin for now
train.training <- train.training[,-c(1)]
train.training <- train.training[,-c(7)]
train.training <- train.training[,-c(8)]

#Replace missing ages with average age
training.na <- which(is.na(train.training$Age))
training.avg.age <- mean(train.training[-training.na,]$Age, na.rm = TRUE)
train.training[training.na,4] <- training.avg.age

#Run logistic regression
train.training.lg <- glm(Survived~., data=train.training, family = "binomial")
summary(train.training.lg)

# ***Use the p-values for coeffients to help guide 
# variable selection, and compare models with the
# Residual deviance and AIC (smaller is better for both metrics).

train2.training.lg <- glm(Survived~.-Embarked-Parch-Fare, data=train.training, family = "binomial")
summary(train2.training.lg)

###########################################
#CV step now
#Remove name
train.testing <- train.testing[,-c(4)]

#Remove PassengerID, Ticket and Cabin
train.testing <- train.testing[,-c(1)]
train.testing <- train.testing[,-c(7)]
train.testing <- train.testing[,-c(8)]

#Variables Pclass, Sex, and Embarked are categorical
#so need to convert them to factors
train.testing$Pclass <- factor(train.testing$Pclass)
train.testing$Sex <- factor(train.testing$Sex)
train.testing$Embarked <- factor(train.testing$Embarked)

#Replace missing ages with average age
testing.na <- which(is.na(train.testing$Age))
testing.avg.age <- mean(train.testing[-testing.na,]$Age, na.rm = TRUE)
train.testing[testing.na,4] <- testing.avg.age

# Model without Embarked, Parch, Fare
train2.training.lg <- glm(Survived~.-Embarked-Parch-Fare, data=train.training, family = "binomial")
summary(train2.training.lg)
probs<-as.vector(predict(train2.training.lg,newdata=train.testing, type="response"))
preds <- rep(0,177)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,train.testing$Survived)

# A reminder: The full model
train.training.lg <- glm(Survived~., data=train.training, family = "binomial")
summary(train.training.lg)
probs<-as.vector(predict(train.training.lg,newdata=train.testing, type="response"))
preds <- rep(0,177)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,train.testing$Survived)
####################################################

#Prediction step now
#Remove name
pred <- pred[,-c(3)]

#Variables Pclass, Sex, and Embarked are categorical
#so need to convert them to factors
pred$Pclass <- factor(pred$Pclass)
pred$Sex <- factor(pred$Sex)
pred$Embarked <- factor(pred$Embarked)

#Remove PassengerID, Ticket and Cabin for now
pred <- pred[,-c(1)]
pred <- pred[,-c(6)]
pred <- pred[,-c(7)]

#Replace missing ages with average age
pred.na <- which(is.na(pred$Age))
pred.avg.age <- mean(pred[-pred.na,]$Age, na.rm = TRUE)
pred[pred.na,4] <- pred.avg.age

#Prediction values and store predictions as zeros and ones
pred2_values <- predict(train2.training.lg, newdata = pred)
preds2 <- rep(0,418)  # Initialize prediction vector
preds2[pred2_values>0.5] <- 1 # p>0.5 -> 1

#Write out predictions to csv
write.table(preds2, file = "titanic_predictions.csv", row.names=F, col.names=c("Survived"), sep=",")

