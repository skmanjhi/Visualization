##########################################################
# Credit Card Default Dataset Analysis
# 
##########################################################
options(scipen=7)

# Install these packages via Packages tab
# For fancy scatterplots
library(car)

setwd("C:/Sachin/Work/Projects/DS_Workshop/Module 10")
ClientRecs <- read.csv("CredClients.csv", header=TRUE)

# Change the column name for the last column
colnames(ClientRecs)[25] <- "DEFAULT"

# Declare factor variables
factorVars <- c(3:5, 7:12, 25)
for (var in factorVars) {
  ClientRecs[ , var] <- factor(ClientRecs[ , var])
}

# Declare numeric variables
numericVars <- c(6, 13:24)
for(i in numericVars) {
  ClientRecs[ , i] <- as.numeric(ClientRecs[ , i])
}

# Drop the first column - it's a serial number!
ClientRecs <- ClientRecs[ , -1]

# Maybe these variables will be useful?
ClientRecs$TOTAL_BILL <- apply(ClientRecs[ , 12:17], 1, 
                               FUN=sum)
ClientRecs$TOTAL_PAY <- apply(ClientRecs[ , 18:23], 1, 
                              FUN=sum)

##########################################################
# Break the data into two sets: training & testing
Training <- ClientRecs[1:25000, ]
Testing <- ClientRecs[25001:30000, ]

# Further split by Default Status
# Defaulters versus Non-defaulters
TrainingDefaulters <- subset(Training, DEFAULT==1)
TrainingNondefaulters <- subset(Training, DEFAULT==0)

TestingDefaulters <- subset(Testing, DEFAULT==1)
TestingNondefaulters <- subset(Testing, DEFAULT==0)

##########################################################
# Start by examining trends in single variables

# Explore the composition of the two sets
# Training and Testing
# Patterns would be reproduced if Testing is
# representative of the overall population

# Amount of Credit
par(mfrow=c(1, 3))
hist(ClientRecs[sample(nrow(ClientRecs), 5000), 1],
     main="Amount of Credit (SAMPLE)",
     breaks=20,
     col=colors()[30:50])

hist(Training$LIMIT_BAL,
     main="Amount of Credit (TRAINING)",
     breaks=20,
     col=colors()[30:50])

hist(Testing$LIMIT_BAL,
     main="Amount of Credit (TESTING)",
     breaks=20,
     col=colors()[30:50])

# Amount of Credit for Defaulters/Non-defaulters
hist(ClientRecs$LIMIT_BAL,
     main="Amount of Credit (POPULATION)",
     breaks=20,
     col=colors()[30:50])

hist(TrainingNondefaulters$LIMIT_BAL,
     main="Amount of Credit (NON-DEFAULTERS)",
     breaks=20,
     col=colors()[30:50])

hist(TrainingDefaulters$LIMIT_BAL,
     main="Amount of Credit (DEFAULTERS)",
     breaks=20,
     col=colors()[30:50])

# Gender split
pie(table(ClientRecs$SEX),
    main="Gender Distribution (POPULATION)",
    col=c("yellow", "blue"),
    labels=c("Male", "Female"))

pie(table(Training$SEX),
    main="Gender Distribution (TRAINING)",
    col=c("yellow", "blue"),
    labels=c("Male", "Female"))

pie(table(Testing$SEX),
    main="Gender Distribution (TESTING)",
    col=c("yellow", "blue"),
    labels=c("Male", "Female"))

# Distribution of Education levels
barplot(table(ClientRecs$EDUCATION),
        main="Education Distribution (POPULATION)",
        col=colors()[30:35])

barplot(table(Training$EDUCATION),
        main="Education Distribution (TRAINING)",
        col=colors()[30:35])

barplot(table(Testing$EDUCATION),
        main="Education Distribution (TESTING)",
        col=colors()[30:35])

# Distribution of Education levels by Gender
PopByGender <- table(ClientRecs$EDUCATION, ClientRecs$SEX)
TrainBP <- barplot(PopByGender,
                   main="Education By Gender (POPULATION)",
                   col=colors()[30:35])

TrainingByGender <- table(Training$EDUCATION, Training$SEX)
TrainBP <- barplot(TrainingByGender,
                   main="Education By Gender (TRAINING)",
                   col=colors()[30:35])

TestingByGender <- table(Testing$EDUCATION, Testing$SEX)
TestBP <- barplot(TestingByGender,
                   main="Education By Gender (TESTING)",
                   col=colors()[30:35])

# Defaulter split
pie(table(ClientRecs$DEFAULT),
    main="Defaulter Distribution (POPULATION)",
    col=c("orange", "darkgreen"),
    labels=c("Nondefaulter", "Defaulter"))

pie(table(Training$DEFAULT),
    main="Defaulter Distribution (TRAINING)",
    col=c("orange", "darkgreen"),
    labels=c("Nondefaulter", "Defaulter"))

pie(table(Testing$DEFAULT),
    main="Defaulter Distribution (TESTING)",
    col=c("orange", "darkgreen"),
    labels=c("Nondefaulter", "Defaulter"))

# Is Education the culprit?
barplot(table(ClientRecs$EDUCATION),
        main="Education Levels (POPULATION)",
        col=colors()[35:40])

barplot(table(TrainingNondefaulters$EDUCATION),
        main="Non-defaulter Education Levels (TRAINING)",
        col=colors()[35:40])

barplot(table(TrainingDefaulters$EDUCATION),
        main="Defaulter Education Levels (TRAINING)",
        col=colors()[35:40])

barplot(table(ClientRecs$EDUCATION),
        main="Education Levels (POPULATION)",
        col=colors()[35:40])

barplot(table(TestingNondefaulters$EDUCATION),
        main="Non-defaulter Education Levels (TESTING)",
        col=colors()[35:40])

barplot(table(TestingDefaulters$EDUCATION),
        main="Defaulter Education Levels (TESTING)",
        col=colors()[35:40])


# Is Marital status the culprit?
par(mfrow=c(1,3))
barplot(table(ClientRecs$MARRIAGE),
        main="Marital Levels (ALL)",
        col=colors()[35:40])
barplot(table(TrainingDefaulters$MARRIAGE),
        main="Defaulter Marital Levels (TRAINING)",
        col=colors()[35:40])
barplot(table(TrainingNondefaulters$MARRIAGE),
        main="Non-defaulter Marital Levels (TRAINING)",
        col=colors()[35:40])

barplot(table(ClientRecs$MARRIAGE),
        main="Marital Levels (ALL)",
        col=colors()[35:40])
barplot(table(TestingDefaulters$MARRIAGE),
        main="Defaulter Marital Levels (TESTING)",
        col=colors()[35:40])
barplot(table(TestingNondefaulters$MARRIAGE),
        main="Non-defaulter Marital Levels (TESTING)",
        col=colors()[35:40])

# Age distribution among defaulters vs. non-defaulters
par(mfrow=c(1, 2))
hist(Training$AGE,
     main="Distribution of Ages (TRAINING)",
     breaks=25,
     col=colors()[30:50])

hist(Testing$AGE,
     main="Distribution of Ages (TESTING)",
     breaks=20,
     col=colors()[30:50])

hist(TrainingDefaulters$AGE,
     main="Defaulter Ages (TRAINING)",
     breaks=20,
     col=colors()[30:50])
hist(TrainingNondefaulters$AGE,
     main="Non-defaulter Ages (TRAINING)",
     breaks=25,
     col=colors()[30:50])

hist(TestingDefaulters$AGE,
     main="Defaulter Ages (TESTING)",
     breaks=20,
     col=colors()[30:50])
hist(TestingNondefaulters$AGE,
     main="Non-defaulter Ages (TESTING)",
     breaks=20,
     col=colors()[30:50])

##########################################################
# Explore multiple variables at once
# Fancy scatter plot for the categorical variables
scatterplotMatrix(~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE,
                  data=ClientRecs,
                  pch=20,
                  spread=FALSE,
                  diagonal="histogram",
                  smoother=FALSE,
                  main="Multiple Variable Plot")

scatterplotMatrix(~ LIMIT_BAL + TOTAL_PAY + TOTAL_BILL + DEFAULT,
                  data=ClientRecs,
                  pch=20,
                  spread=FALSE,
                  diagonal="histogram",
                  smoother=FALSE,
                  main="Multiple Variable Plot for DEFAULT")

##########################################################
# Try a Linear Regression
#
par(mfrow=c(1,2))
TNDMinusOutliers <- subset(TrainingNondefaulters,
                           TOTAL_BILL < 3000000 &
                             TOTAL_PAY < 1000000)
plot(TOTAL_PAY ~ TOTAL_BILL,
     data=TNDMinusOutliers,
     main="What is billed versus paid? (NONDEF)",
     pch=20, cex=0.8, col="darkgreen")

lmNonDef <- lm(TOTAL_PAY ~ TOTAL_BILL + factor(SEX) + AGE,
               data=TNDMinusOutliers)
abline(lmNonDef,
        lwd=2, col="red")

summary(lmNonDef)

TDMinusOutliers <- subset(TrainingDefaulters,
                           TOTAL_BILL < 3000000 &
                             TOTAL_PAY < 1000000)
plot(TOTAL_PAY ~ TOTAL_BILL,
     data=TDMinusOutliers,
     main="What is billed versus paid? (DEF)",
     pch=20, cex=0.8, col="orange")
lmDef <- lm(TOTAL_PAY ~ TOTAL_BILL + factor(SEX) + AGE,
               data=TDMinusOutliers)
abline(lmDef,
       lwd=2, col="red")

summary(lmDef)

plot(TOTAL_BILL ~ TOTAL_PAY,
     data=Testing,
     main="What is billed versus paid? (TESTING)",
     pch=20, cex=0.8, col="darkorange")

##########################################################
# Now begin modelling. We try to predict the 
# Defaulter status using Logistic Regression

LRPartial <- glm(DEFAULT ~ LIMIT_BAL + SEX + 
               EDUCATION + MARRIAGE + AGE,
             family=binomial(link='logit'),
             data=Training)

print(summary(LRPartial), digits=3)

# Check for the accuracy of the prediction
# based on a PARTIAL fit
fitted.results <- predict(LRPartial,
                          newdata=Testing,
                          type='response')

# Assume a threshold value of 0.5 for classification
Threshold <- 0.5
Testing$FITTED <- ifelse(fitted.results > Threshold, 
                         1, 0)

misClasificError <- mean(Testing$FITTED != Testing$DEFAULT)
print(paste('Accuracy =',1-misClasificError))

LRFull <- glm(DEFAULT ~ .,
             family=binomial(link='logit'),
             data=Training)

print(summary(LRFull), digits=3)

# Check for the accuracy of the prediction
# based on a FULL fit
fitted.results <- predict(LRFull,
                          newdata=Testing,
                          type='response')

Testing$FITTED <- ifelse(fitted.results > Threshold, 1, 0)

misClasificError <- mean(Testing$FITTED != Testing$DEFAULT)
print(paste('Accuracy =',1-misClasificError))

# What is a good setting for the Threshold parameter?
library(ROCR)
pr <- prediction(fitted.results, Testing$DEFAULT)
prf <- performance(pr, 
                   measure = "tpr", 
                   x.measure = "fpr")
par(mfrow=c(1,1))
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
