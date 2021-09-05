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

penalty = 2
forwardAIC = step(model.empty, scope, direction = "forward", k = penalty)
backwardAIC = step(model.full, scope, direction = "backward", k = penalty)
bothAIC.empty = step(model.empty, scope, direction = "both", k = penalty)
bothAIC.full = step(model.full, scope, direction = "both", k = penalty)

#Stepwise regression using BIC as the criterion (the penalty k = log(n)).
penalty = log(1460)
forwardBIC = step(model.empty, scope, direction = "forward", k = penalty)
backwardBIC = step(model.full, scope, direction = "backward", k = penalty)
bothBIC.empty = step(model.empty, scope, steps = 8, direction = "both", k = penalty)
bothBIC.full = step(model.full, scope, direction = "both", k = penalty)

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
plot(forwardAIC)

influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)