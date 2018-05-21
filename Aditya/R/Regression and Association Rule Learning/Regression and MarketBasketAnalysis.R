setwd("D:/R files")
getwd()
mydatanew <- read.csv("mortality.csv", header=TRUE)
head(mydatanew)

# mortality file has been modified by removing outliers from certain elements.
# The outliers were identified with the help of histogram

#Normalization of skewed data
mydatanew$JanTemp <- log10(mydatanew$JanTemp)
mydatanew$NOxPot <- log10(mydatanew$NOxPot)
mydatanew$S02Pot <- log10(mydatanew$S02Pot)
mydatanew$PopDensity <- log10(mydatanew$PopDensity)
mydatanew$pop <- log10(mydatanew$pop)
mydatanew$NW <- log10(mydatanew$NW)
mydatanew$income < log10(mydatanew$income)
mydatanew$HCPot <- log10(mydatanew$HCPot)
mydatanew$HHSiz <- log10(3.53-1)

# Removing City Identifier
mydatanew$City <- NULL
head(mydatanew)

#Running a forward regression with transformed variables

null <- lm(Mortality ~ 1, data=mydatanew)
null
full <- lm(Mortality ~ ., data=mydatanew)
full
linearmodel <- step(null, scope=list(lower=null, upper=full), direction="forward")
summary(linearmodel)

#Running a regression with selected components
finalmodel <- lm(Mortality ~ NW+JanTemp+NOxPot+Rain, data=mydatanew)
summary(finalmodel)

#Heteroscedasticity Test
install.packages("car")
library(car)
car::ncvTest(linearmodel)
car::ncvTest(finalmodel)

#PCA Analysis
pcamodel <- mydatanew
pcamodel$Mortality <- NULL
pca <- princomp(pcamodel, cor = TRUE)
summary(pca)
pca$loadings
pca$scores
plot(pca, type='lines')

#Using the scree plot, the components with eigen values greter than 1 are identified
mydatanew <- cbind(mydatanew, pca$scores)
head(mydatanew)
pcanew <- lm(Mortality ~ (pca$scores[,1:5]), data=mydatanew)
summary(pcanew)

#Selecting the first 3 components based on their significance
pcafinal <- lm(Mortality ~ (pca$scores[,1:3]), data=mydata)
summary(pcafinal)



# Association Rules
library(arules)
library(readr)
transactions <- read.csv("D:/R files/transactions.csv")

#Considering only transactions and product from the imported excel file
trans <- read.transactions("transactions.csv", format = "single", sep = ",", cols = c("Transaction", "Product"), rm.duplicates = FALSE)
rules <- apriori(trans, parameter = list(supp = 0.03, conf = 0.20, minlen = 1, maxlen = 4))
rules <- sort(rules, by="lift", decreasing=TRUE)
options(digits=2)

inspect(rules)
summary(rules)

#Removing redundant fields
redundant_index <- is.redundant(rules)
pruned_rules <- rules[!redundant_index]
inspect(pruned_rules)
summary(pruned_rules)