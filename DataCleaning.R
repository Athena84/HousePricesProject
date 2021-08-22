
#Read the training data
train_data <- read.csv("./Data/train.csv", stringsAsFactors = FALSE)
test_data <- read.csv("./Data/test.csv", stringsAsFactors = FALSE)

#head(cleaned_train_data)
#head(test_data)
#summary(train_data)
#apply(train_data, 2, unique)


selected_cols <- c("Id", "LotArea", "OverallQual", "GarageCars", "YearBuilt", "TotRmsAbvGrd", "OverallCond", "MSSubClass", "LotShape", "LotConfig", "GrLivArea", "TotalBsmtSF", "Condition1", "Condition2", "MoSold")
cleaned_train_data <- train_data[selected_cols]
cleaned_test_data <- test_data[selected_cols]
cleaned_target <- train_data[c("Id", "SalePrice")]

#Impute missing values in test data set
cleaned_test_data$GarageCars =  ifelse(is.na(cleaned_test_data$GarageCars), 0, cleaned_test_data$GarageCars)
#cleaned_test_data$TotalBsmtSF =ifelse(is.na(cleaned_test_data$TotalBsmtSF), 0, cleaned_test_data$TotalBsmtSF)

#Combining month+year into single ordered feature
cleaned_train_data$YrSold = train_data$YrSold + ((train_data$MoSold - 1) / 12)
cleaned_test_data$YrSold = test_data$YrSold + ((test_data$MoSold - 1) / 12)

#Convert quality text into ranking
convert_quality_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("Po", 1, ranking)
  ranking = gsub("Fa", 2, ranking)
  ranking = gsub("TA", 3, ranking)
  ranking = gsub("Gd", 4, ranking)
  ranking = gsub("Ex", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$KitchenQual = convert_quality_factor(train_data$KitchenQual)
cleaned_test_data$KitchenQual = convert_quality_factor(test_data$KitchenQual)

cleaned_train_data$ExterQual = convert_quality_factor(train_data$ExterQual)
cleaned_test_data$ExterQual = convert_quality_factor(test_data$ExterQual)

cleaned_train_data$ExterCond = convert_quality_factor(train_data$ExterCond)
cleaned_test_data$ExterCond = convert_quality_factor(test_data$ExterCond)

cleaned_train_data$HeatingQC = convert_quality_factor(train_data$HeatingQC)
cleaned_test_data$HeatingQC = convert_quality_factor(test_data$HeatingQC)

cleaned_train_data$GarageQual = convert_quality_factor(train_data$GarageQual)
cleaned_test_data$GarageQual = convert_quality_factor(test_data$GarageQual)

#Convert Fence quality text into ranking
convert_fence_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("MnWw", 1, ranking)
  ranking = gsub("GdWo", 2, ranking)
  ranking = gsub("MnPrv", 2, ranking)
  ranking = gsub("GdPrv", 3, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Fence = convert_fence_factor(train_data$Fence)
cleaned_test_data$Fence = convert_fence_factor(test_data$Fence)

#Convert Building Type text into ranking
convert_buildtype_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("Twnhs", 4, ranking)
  ranking = gsub("TwnhsE", 2, ranking)
  ranking = gsub("Duplex", 3, ranking)
  ranking = gsub("2fmCon", 1, ranking)
  ranking = gsub("1Fam", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$BldgType = convert_buildtype_factor(train_data$BldgType)
cleaned_test_data$BldgType = convert_buildtype_factor(test_data$BldgType)

#Convert Functional text into ranking
convert_functional_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("Sal", 0, ranking)
  ranking = gsub("Sev", 0, ranking)
  ranking = gsub("Maj1", 1, ranking)
  ranking = gsub("Maj2", 1, ranking)
  ranking = gsub("Mod", 2, ranking)
  ranking = gsub("Min1", 3, ranking)
  ranking = gsub("Min2", 3, ranking)
  ranking = gsub("Typ", 4, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Functional = convert_functional_factor(train_data$Functional)
cleaned_test_data$Functional = convert_functional_factor(test_data$Functional)

#Convert proximity features
cleaned_train_data$ProxPos = ifelse((train_data$Condition1 == "PosA") | (train_data$Condition2 == "PosA") | (train_data$Condition1 == "PosN") | (train_data$Condition2 == "PosN"), 1, 0)
cleaned_train_data$ProxRoad = ifelse((train_data$Condition1 == "Artery") | (train_data$Condition2 == "Artery") | (train_data$Condition1 == "Feedr") | (train_data$Condition2 == "Feedr"), 1, 0)
cleaned_train_data$ProxRail = ifelse((train_data$Condition1 == "RRAn") | (train_data$Condition1 == "RRAe") | (train_data$Condition2 == "RRAn") | (train_data$Condition2 == "RRAe") | (train_data$Condition1 == "RRNn") | (train_data$Condition1 == "RRNe") | (train_data$Condition2 == "RRNn") | (train_data$Condition2 == "RRNe"), 1, 0)

cleaned_test_data$ProxPos = ifelse((test_data$Condition1 == "PosA") | (test_data$Condition2 == "PosA") | (test_data$Condition1 == "PosN") | (test_data$Condition2 == "PosN"), 1, 0)
cleaned_test_data$ProxRoad = ifelse((test_data$Condition1 == "Artery") | (test_data$Condition2 == "Artery") | (test_data$Condition1 == "Feedr") | (test_data$Condition2 == "Feedr"), 1, 0)
cleaned_test_data$ProxRail = ifelse((test_data$Condition1 == "RRAn") | (test_data$Condition1 == "RRAe") | (test_data$Condition2 == "RRAn") | (test_data$Condition2 == "RRAe") | (test_data$Condition1 == "RRNn") | (test_data$Condition1 == "RRNe") | (test_data$Condition2 == "RRNn") | (test_data$Condition2 == "RRNe"), 1, 0)

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

#Combined living space feature
cleaned_train_data$WeightedTotLivArea = train_data$GrLivArea + (Train_Basement / basement_factor) * ifelse(is.na(train_data$TotalBsmtSF), 0, train_data$TotalBsmtSF)
cleaned_train_data$TotLivArea = train_data$GrLivArea + train_data$TotalBsmtSF
cleaned_test_data$TotLivArea = test_data$GrLivArea + (Test_Basement / basement_factor) * ifelse(is.na(test_data$TotalBsmtSF), 0, test_data$TotalBsmtSF)

cleaned_train_data$WeightedBasementSF = (Train_Basement / basement_factor) * ifelse(is.na(train_data$TotalBsmtSF), 0, train_data$TotalBsmtSF)
cleaned_train_data$BasementWeigths = Train_Basement

#Combining number of bathrooms above ground and basement
cleaned_train_data$FullBath = train_data$FullBath + (Train_Basement / basement_factor) * train_data$BsmtFullBath 
cleaned_test_data$FullBath = test_data$FullBath + 
  (Test_Basement / basement_factor) * ifelse(is.na(test_data$BsmtFullBath), 0,  test_data$BsmtFullBath) 

cleaned_train_data$HalfBath = train_data$HalfBath + (Train_Basement/ basement_factor) * train_data$BsmtHalfBath
cleaned_test_data$HalfBath = test_data$HalfBath +
  (Test_Basement / basement_factor) * ifelse(is.na(test_data$BsmtHalfBath), 0, test_data$BsmtHalfBath)

#Combined enclosed porch feature
cleaned_train_data$PorchSF = train_data$EnclosedPorch + train_data$X3SsnPorch + train_data$ScreenPorch
cleaned_test_data$PorchSF = test_data$EnclosedPorch + test_data$X3SsnPorch + test_data$ScreenPorch

#Store Neighborhoods as integers ordered by median saleprice
cleaned_train_data$Neighborhood = factor(reorder(train_data$Neighborhood, train_data$SalePrice, median))
cleaned_test_data$Neighborhood = as.integer(factor(test_data$Neighborhood, levels = levels(cleaned_train_data$Neighborhood)))
cleaned_train_data$Neighborhood = as.integer(cleaned_train_data$Neighborhood)

#Combined recency factor
#remod_factor = 0.5
#cleaned_train_data$Recency = 2011 - train_data$YearRemodAdd + remod_factor * (train_data$YearRemodAdd - train_data$YearBuilt)
#cleaned_test_data$Recency = 2011 - test_data$YearRemodAdd + remod_factor * (test_data$YearRemodAdd - test_data$YearBuilt)

#Save cleaned data set
write.csv(cleaned_train_data, "./Data/cleaned_train.csv", row.names = FALSE)
write.csv(cleaned_target, "./Data/cleaned_target.csv", row.names = FALSE)
write.csv(cleaned_test_data, "./Data/cleaned_test.csv", row.names = FALSE)

#Check missing values as % for columns
colMeans(is.na(train_data)) * 100
colMeans(is.na(test_data)) * 100
colMeans(is.na(cleaned_train_data)) * 100
colMeans(is.na(cleaned_test_data)) * 100

