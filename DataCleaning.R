library(VIM)

#Read the training data
train_data <- read.csv("./Data/train.csv", stringsAsFactors = FALSE)
test_data <- read.csv("./Data/test.csv", stringsAsFactors = FALSE)

selected_cols <- c("Id", "LotArea", "LotFrontage", "OverallQual", "GarageCars", "TotRmsAbvGrd", "OverallCond", "Fireplaces", "CentralAir", "Street", "KitchenQual", "ExterQual", "ExterCond", "HeatingQC", "GarageQual", "PoolQC", "BsmtCond", "Utilities", "Fence", "LandSlope", "LotShape", "BsmtExposure", "BldgType", "MasVnrType", "Foundation", "Electrical", "Functional", "Neighborhood", "MSSubClass", "MSZoning", "SaleCondition")
cleaned_train_data <- train_data[selected_cols]
cleaned_test_data <- test_data[selected_cols]
cleaned_target <- train_data[c("Id", "SalePrice")]
#head(cleaned_train_data)
#head(test_data)
#summary(train_data)
#apply(train_data, 2, unique)

#Impute Garage values in test data set
cleaned_test_data$GarageCars =  ifelse(is.na(cleaned_test_data$GarageCars), 0, cleaned_test_data$GarageCars)

#Impute Utilities values in test data set
#modeUtil = names(sort(table(cleaned_test_data$Utilities), decreasing = TRUE)[1])
cleaned_test_data$Utilities =  ifelse(is.na(cleaned_test_data$Utilities), "NoSeWa", cleaned_test_data$Utilities)

#Impute MSZoning by mode
modeMS = names(sort(table(cleaned_test_data$MSZoning), decreasing = TRUE)[1])
cleaned_test_data$MSZoning =  ifelse(is.na(cleaned_test_data$MSZoning), modeMS, cleaned_test_data$MSZoning)

#Impute LotFrontage based on combination of train and test data by kNN with k=sqrt(n)
imputed_LotSizes = rbind(cleaned_train_data[c("LotArea", "LotFrontage")], cleaned_test_data[c("LotArea", "LotFrontage")])
imputed_LotSizes = kNN(data = imputed_LotSizes, variable = "LotFrontage", dist_var = c("LotArea"), k = 54, useImputedDist = FALSE)
cleaned_train_data$LotFrontage =  imputed_LotSizes$LotFrontage[1:1460]
cleaned_test_data$LotFrontage =  imputed_LotSizes$LotFrontage[1461:2919]

#Combining month+year into single ordered feature
cleaned_train_data$YrSold = train_data$YrSold + ((train_data$MoSold - 1) / 12) - 2005
cleaned_test_data$YrSold = test_data$YrSold + ((test_data$MoSold - 1) / 12) - 2005

#Convert year built/remod into recency
cleaned_train_data$AgeBuilt = 2011 - train_data$YearBuilt
cleaned_test_data$AgeBuilt = 2011 - test_data$YearBuilt
cleaned_train_data$AgeRemod = 2011 - train_data$YearRemodAdd
cleaned_test_data$AgeRemod = 2011 - test_data$YearRemodAdd

#Combined living space feature 
cleaned_train_data$TotLivArea = train_data$GrLivArea + train_data$TotalBsmtSF
cleaned_test_data$TotLivArea = test_data$GrLivArea + ifelse(is.na(test_data$TotalBsmtSF), 0, test_data$TotalBsmtSF)

#Convert separate roximity features
cleaned_train_data$ProxPos = ifelse((train_data$Condition1 == "PosA") | (train_data$Condition2 == "PosA") | (train_data$Condition1 == "PosN") | (train_data$Condition2 == "PosN"), 1, 0)
cleaned_train_data$ProxRoad = ifelse((train_data$Condition1 == "Artery") | (train_data$Condition2 == "Artery") | (train_data$Condition1 == "Feedr") | (train_data$Condition2 == "Feedr"), 1, 0)
cleaned_train_data$ProxRail = ifelse((train_data$Condition1 == "RRAn") | (train_data$Condition1 == "RRAe") | (train_data$Condition2 == "RRAn") | (train_data$Condition2 == "RRAe") | (train_data$Condition1 == "RRNn") | (train_data$Condition1 == "RRNe") | (train_data$Condition2 == "RRNn") | (train_data$Condition2 == "RRNe"), 1, 0)

cleaned_test_data$ProxPos = ifelse((test_data$Condition1 == "PosA") | (test_data$Condition2 == "PosA") | (test_data$Condition1 == "PosN") | (test_data$Condition2 == "PosN"), 1, 0)
cleaned_test_data$ProxRoad = ifelse((test_data$Condition1 == "Artery") | (test_data$Condition2 == "Artery") | (test_data$Condition1 == "Feedr") | (test_data$Condition2 == "Feedr"), 1, 0)
cleaned_test_data$ProxRail = ifelse((test_data$Condition1 == "RRAn") | (test_data$Condition1 == "RRAe") | (test_data$Condition2 == "RRAn") | (test_data$Condition2 == "RRAe") | (test_data$Condition1 == "RRNn") | (test_data$Condition1 == "RRNe") | (test_data$Condition2 == "RRNn") | (test_data$Condition2 == "RRNe"), 1, 0)

#Convert heating into gas or not
cleaned_train_data$Heating = ifelse((train_data$Heating == "GasA" | train_data$Heating == "GasW" |train_data$Heating == "OthW"), 1, 0)
cleaned_test_data$Heating = ifelse((test_data$Heating == "GasA" | test_data$Heating == "GasW" |test_data$Heating == "OthW"), 1, 0)

#Convert basement type into ranking
basement_factor = 6
basement_convert_factor <- function(FinType) {
  basement = ifelse(is.na(FinType), 2,  FinType)
  basement = gsub("Unf", 1, basement)
  basement = gsub("LwQ", 1, basement)
  basement = gsub("Rec", 2, basement)
  basement = gsub("BLQ", 3, basement)
  basement = gsub("ALQ", 4, basement)
  basement = gsub("GLQ", 5, basement)
  basement = as.integer(basement)
  return(basement)
}

#Combine basement rankings in weighted average by surface of the 2 named types and counting the unfinished part with factor 1
basement_weight <- function(FinType1, FinType2, SF1, SF2, SF3) {
  basement1 = basement_convert_factor(FinType1)
  basement2 = basement_convert_factor(FinType2)
  basement1SF = ifelse(is.na(SF1), 0,  SF1)
  basement2SF = ifelse(is.na(SF2), 0,  SF2)
  basement3SF = ifelse(is.na(SF3), 0,  SF3)
  basement_weighted = ifelse((SF1+SF2+SF3) > 0, as.numeric((basement1 * basement1SF + basement2 * basement2SF + basement3SF) / (basement1SF + basement2SF + basement3SF)), 0)
  return(basement_weighted)
}
Train_Basement = basement_weight(train_data$BsmtFinType1, train_data$BsmtFinType2, train_data$BsmtFinSF1, train_data$BsmtFinSF2, train_data$BsmtUnfSF)
Test_Basement = basement_weight(test_data$BsmtFinType1, test_data$BsmtFinType2, test_data$BsmtFinSF1, test_data$BsmtFinSF2, test_data$BsmtUnfSF)
cleaned_train_data$BasementQualFactor = Train_Basement
cleaned_test_data$BasementQualFactor = ifelse(is.na(Test_Basement), 0, Test_Basement)

#Combining number of bathrooms above ground and basement
cleaned_train_data$FullBath = train_data$FullBath + train_data$BsmtFullBath 
cleaned_test_data$FullBath = test_data$FullBath + ifelse(is.na(test_data$BsmtFullBath), 1,  test_data$BsmtFullBath) 
cleaned_train_data$HalfBath = train_data$HalfBath + train_data$BsmtHalfBath
cleaned_test_data$HalfBath = test_data$HalfBath + ifelse(is.na(test_data$BsmtHalfBath), 1, test_data$BsmtHalfBath)

#Combined enclosed porch feature
cleaned_train_data$PorchSF = train_data$EnclosedPorch + train_data$X3SsnPorch + train_data$ScreenPorch
cleaned_test_data$PorchSF = test_data$EnclosedPorch + test_data$X3SsnPorch + test_data$ScreenPorch

#Save cleaned data set
write.csv(cleaned_train_data, "./Data/cleaned_train.csv", row.names = FALSE)
write.csv(cleaned_target, "./Data/cleaned_target.csv", row.names = FALSE)
write.csv(cleaned_test_data, "./Data/cleaned_test.csv", row.names = FALSE)



#==============================================================================
#Check missing values as % for columns
colMeans(is.na(train_data)) * 100
colMeans(is.na(test_data)) * 100
colMeans(is.na(cleaned_train_data)) * 100
colMeans(is.na(cleaned_test_data)) * 100
