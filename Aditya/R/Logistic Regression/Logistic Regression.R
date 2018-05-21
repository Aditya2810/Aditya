#Decision Tree

install.packages("Hmisc")
#install.packages("mice")
#install.packages("randomForest")
library(Hmisc)
library(rpart)
library(rpart.plot)

#Reading the file
organics <- read.csv("organics1.csv")
attach(organics)
organics$ID <- NULL

sapply(organics, function(x) sum(is.na(x)))
organics$DemAffl <- with(organics, impute(DemAffl, mean))
organics$DemAge <- with(organics, impute(DemAge, mean))
organics$DemClusterGroup <- with(organics, impute(DemClusterGroup, max))
organics$DemGender <- with(organics, impute(DemGender, max))
organics$DemReg <- with(organics, impute(DemReg, max))
organics$DemTVReg <- with(organics, impute(DemTVReg, max))
organics$PromTime <- with(organics, impute(PromTime, mean))
str(organics)

#Randomize the data
set.seed(42)
rand <- runif(nrow(organics))
organicsrand <- organics[order(rand), ]
str(organicsrand)

#Partition the data
organicstrain <- organicsrand[1:11111, ]
organicstest <- organicsrand[11112:22223, ]

#Distribution of target variable
table(organics$TargetBuy)
table(organicstrain$TargetBuy)
table(organicstest$TargetBuy)


#Building decision tree using train data
organicstree <- rpart(TargetBuy ~ ., data = organicstrain, method = "class")
organicstree
rpart.plot(organicstree)
printcp(organicstree)

#Confusion matrix and Accuracy

#Training data
organicstrain$pred <- predict(organicstree, organicstrain, type = "class") 
table(Actual = organicstrain$TargetBuy, Predicted = organicstrain$pred)

organicstrain$correct <- organicstrain$TargetBuy == organicstrain$pred
traincorrectcount <- length(which(organicstrain$correct))
trainincorrectcount <- nrow(organicstrain) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(organicstrain)
trainaccuracy <- 1-trainerrorrate
trainaccuracy

#Test Data
organicstest$pred <- predict(organicstree, organicstest, type = "class")
table(Actual = organicstest$TargetBuy, Predicted = organicstest$pred)

organicstest$correct <- organicstest$TargetBuy == organicstest$pred
testcorrectcount <- length(which(organicstest$correct))
testincorrectcount <- nrow(organicstest) - testcorrectcount
testerrorrate <- testincorrectcount/nrow(organicstest)
testaccuracy <- 1-testerrorrate
testaccuracy

#Comparing accuracies
paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")

###################################################
#Logistic Regression

#Build logistic regression

organicstrainlogit$treepred <- NULL
organicstrainlogit$treepredcorrect <- NULL

organicslogit <- glm(TargetBuy ~ organicstrain$PromClass + organicstrain$PromSpend + organicstrain$DemAffl + organicstrain$DemAge + organicstrain$PromTime + organicstrain$DemClusterGroup + organicstrain$DemGender + organicstrain$DemReg, data = organicstrain, family = binomial(link = "logit"))
summary(organicslogit)

organicslogit <- glm(TargetBuy ~ DemAffl + DemAge + DemGender, data = organicstrain, family = binomial(link = "logit"))
summary(organicslogit)

confint.default(organicslogit) #Build confidence intervals
exp(coef(organicslogit)) #Calculate odd sratio

#Evaluate logistic model performance- Confusion Matrix and Accuracy

organicstrain$probsurv <- predict(organicslogit, newdata = organicstrain, type = "response")
organicstrain$logitpred <- round(organicstrain$probsurv)
table(Actual = organicstrain$TargetBuy, Predicted = organicstrain$logitpred)#create confusion matrix
organicstrain$correctlogit <- organicstrain$TargetBuy == organicstrain$logitpred #count number of cases predicted correctly
traincorrectcountlogit <- length(which(organicstrain$correctlogit))
trainincorrectcountlogit <- nrow(organicstrain) - traincorrectcount
trainerrorratelogit <- trainincorrectcountlogit/nrow(organicstrain)
trainaccuracylogit <- 1-trainerrorratelogit #calcuate accuracy on the training data 
trainaccuracylogit

organicstest$probsurv <- predict(organicslogit, newdata = organicstest, type = "response")
organicstest$logitpred <- round(organicstest$probsurv)
table(Actual = organicstest$TargetBuy, Predicted = organicstest$logitpred)
organicstest$correctlogit <- organicstest$TargetBuy == organicstest$logitpred #count number of cases predicted correctly
testcorrectcountlogit <- length(which(organicstrain$correctlogit))
testincorrectcountlogit <- nrow(organicstrain) - traincorrectcountlogit
testerrorratelogit <- testincorrectcountlogit/nrow(organicstrain)
testaccuracylogit <- 1-testerrorratelogit #calcuate accuracy on the test data 
testaccuracylogit


