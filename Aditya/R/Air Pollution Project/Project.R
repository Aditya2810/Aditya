install.packages("xlsx")
library(mice)
library(xlsx)
data<-read.csv("D:/R files/Original Data.csv")
attach(data)
data$ï..Country<-NULL
factor_vars <- c('')
data$CO2.emissions..metric.tons.per.capita.<-NULL
data$CO2.emissions.from.gaseous.fuel.consumption..kt.<-NULL
data$CO2.emissions.from.liquid.fuel.consumption..kt.<-NULL
data$CO2.emissions.from.solid.fuel.consumption..kt.<-NULL
data$

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(data, method='cart') 
mice_output <- complete(mice_mod)
write.csv(mice_output,"mice1.csv")
getwd()
reg<-lm(mice_output$CO2.emissions..kt.~.,mice_output)
summary(reg)


decade2 <- read.csv("D:/R files/Decade2_New1.csv")
sum(is.na(decade2$Age.dependency.ratio..old....of.working.age.population.))
sum(is.na(decade2$Agricultural.land....of.land.area.))
sum(is.na(decade2$Agricultural.machinery..tractors))
sum(is.na(decade2$Air.transport..passengers.carried))
sum(is.na(decade2$Annual.freshwater.withdrawals..industry....of.total.freshwater.withdrawal.))
sum(is.na(decade2$Arable.land....of.land.area.))
sum(is.na(decade2$CO2.emissions..kt.))
sum(is.na(decade2$CO2.emissions..metric.tons.per.capita.))
sum(is.na(decade2$CO2.emissions.from.solid.fuel.consumption..kt.))
sum(is.na(decade2$Electric.power.consumption..kWh.per.capita.))
sum(is.na(decade2$Electricity.production.from.coal.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.hydroelectric.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.natural.gas.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.nuclear.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.oil.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.oil..gas.and.coal.sources....of.total.))
sum(is.na(decade2$Electricity.production.from.renewable.sources..excluding.hydroelectric....of.total.))
sum(is.na(decade2$Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh.))
sum(is.na(decade2$Employment.in.industry....of.total.employment.))
sum(is.na(decade2$Fossil.fuel.energy.consumption....of.total.))
sum(is.na(decade2$Fuel.imports....of.merchandise.imports.))
sum(is.na(decade2$GDP.per.capita..current.US..))
sum(is.na(decade2$Manufacturing..value.added....of.GDP.))
sum(is.na(decade2$Population.density..people.per.sq..km.of.land.area.))
sum(is.na(decade2$Population..total))

# decade2$CountryName <- NULL
# decade2$Agricultural.machinery..tractors <- NULL
# decade2$Air.transport..passengers.carried <- NULL
# decade2$Annual.freshwater.withdrawals..industry....of.total.freshwater.withdrawal. <- NULL
# decade2$Electric.power.consumption..kWh.per.capita. <- NULL
# decade2$Electricity.production.from.coal.sources....of.total. <- NULL
# decade2$Electricity.production.from.hydroelectric.sources....of.total. <- NULL
# decade2$Electricity.production.from.natural.gas.sources....of.total. <- NULL
# decade2$Electricity.production.from.nuclear.sources....of.total. <- NULL
# decade2$Electricity.production.from.oil.sources....of.total. <- NULL
# decade2$Electricity.production.from.oil..gas.and.coal.sources....of.total. <- NULL
# decade2$Electricity.production.from.renewable.sources..excluding.hydroelectric....of.total. <- NULL
# decade2$Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. <- NULL
# decade2$Employment.in.industry....of.total.employment. <- NULL
# decade2$Fossil.fuel.energy.consumption....of.total. <- NULL
# decade2$Fuel.imports....of.merchandise.imports. <- NULL
# decade2$GDP.per.capita..current.US..<- NULL
# decade2$Manufacturing..value.added....of.GDP.<- NULL
# decade2$Manufacturing..value.added..current.US..<- NULL

# Set a random seed
set.seed(129)
library(mice)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(decade2, method='rf') 
mice_output <- complete(mice_mod)
decade2$CountryName <- NULL
mice_output$CountryName <- NULL
reg<-lm(mice_output$CO2.emissions..kt.~.,mice_output)
summary(reg)





