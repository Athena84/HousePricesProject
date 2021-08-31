library(car)
library(MASS) 

#Read the data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = TRUE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = TRUE)
cleaned_test_data <- read.csv("./Data/cleaned_test.csv", stringsAsFactors = TRUE)
prepped_train_data <- read.csv("./Data/prepped_train.csv", stringsAsFactors = FALSE)
prepped_target <- read.csv("./Data/prepped_target.csv", stringsAsFactors = FALSE)
prepped_test_data <- read.csv("./Data/prepped_test.csv", stringsAsFactors = FALSE)

cleaned_train_data <- subset(cleaned_train_data, select = -c(Id))
cleaned_test_data <- subset(cleaned_test_data, select = -c(Id))
cleaned_target <- subset(cleaned_target, select = -c(Id))
prepped_train_data <- subset(prepped_train_data, select = -c(Id))
prepped_test_data <- subset(prepped_test_data, select = -c(Id))
prepped_target <- subset(prepped_target, select = -c(Id))

#Stepwise regression using AIC as the criterion (the penalty k = 2).
model.empty = lm(prepped_target$SalePrice ~ 1, data = prepped_train_data) #The model with an intercept ONLY
model.full = lm(prepped_target$SalePrice ~ ., data = prepped_train_data) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

pentalty = 2
forwardAIC = step(model.empty, scope, direction = "forward", k = pentalty)
backwardAIC = step(model.full, scope, direction = "backward", k = pentalty)
bothAIC.empty = step(model.empty, scope, direction = "both", k = pentalty)
bothAIC.full = step(model.full, scope, direction = "both", k = pentalty)

#Stepwise regression using BIC as the criterion (the penalty k = log(n)).
penalty = log(1460)
forwardBIC = step(model.empty, scope, direction = "forward", k = pentalty)
backwardBIC = step(model.full, scope, direction = "backward", k = pentalty)
bothBIC.empty = step(model.empty, scope, direction = "both", k = pentalty)
bothBIC.full = step(model.full, scope, direction = "both", k = pentalty)

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
plot(forwardAIC)

influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)