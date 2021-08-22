
#Read the training data
train_data <- read.csv("./Data/train.csv", stringsAsFactors = FALSE)
test_data <- read.csv("./Data/test.csv", stringsAsFactors = FALSE)

#head(cleaned_train_data)
#head(test_data)
#summary(train_data)
#apply(train_data, 2, unique)


selected_cols <- c("Id", "LotArea", "OverallQual", "GarageCars", "YearBuilt", "TotRmsAbvGrd", "Functional")
cleaned_train_data <- train_data[selected_cols]
cleaned_test_data <- test_data[selected_cols]
cleaned_target <- train_data[c("Id", "SalePrice")]

#Impute missing values in test data set
cleaned_test_data$GarageCars =  ifelse(is.na(cleaned_test_data$GarageCars), 0, cleaned_test_data$GarageCars)
cleaned_test_data$Functional =  ifelse(is.na(cleaned_test_data$Functional), "Typ", cleaned_test_data$Functional)
cleaned_test_data$KitchenQual =  ifelse(is.na(cleaned_test_data$KitchenQual), "TA", cleaned_test_data$KitchenQual)
#cleaned_test_data$TotalBsmtSF =ifelse(is.na(cleaned_test_data$TotalBsmtSF), 0, cleaned_test_data$TotalBsmtSF)

#Combining month+year into single ordered feature
cleaned_train_data$YrSold = train_data$YrSold + ((train_data$MoSold - 1) / 12)
cleaned_test_data$YrSold = test_data$YrSold + ((test_data$MoSold - 1) / 12)

#Convert kitchen quality into ranking
kitchen_convert_factor <- function(KitQual) {
  kitchen = ifelse(is.na(KitQual), 1,  KitQual)
  kitchen = gsub("Po", 1, kitchen)
  kitchen = gsub("Fa", 2, kitchen)
  kitchen = gsub("TA", 3, kitchen)
  kitchen = gsub("Gd", 4, kitchen)
  kitchen = gsub("Ex", 5, kitchen)
  kitchen = as.integer(kitchen)
  return(kitchen)
}
cleaned_train_data$KitchenQual = kitchen_convert_factor(train_data$KitchenQual)
cleaned_test_data$KitchenQual = kitchen_convert_factor(test_data$KitchenQual)

#Convert basement type into ranking
basement_convert_factor <- function(FinType) {
  basement = ifelse(is.na(FinType), 0,  FinType)
  basement = gsub("Unf", 1, basement)
  basement = gsub("LwQ", 1, basement)
  basement = gsub("Rec", 2, basement)
  basement = gsub("BLQ", 3, basement)
  basement = gsub("ALQ", 4, basement)
  basement = gsub("GLQ", 5, basement)
  basement = as.integer(basement)
  return(basement)
}

#WCombine basement rankings in weighted average by surface of the 2 types
basement_weight <- function(FinType1, FinType2, SF1, SF2) {
  basement1 = basement_convert_factor(FinType1)
  basement2 = basement_convert_factor(FinType2)
  basement1SF = ifelse(is.na(SF1), 0,  SF1)
  basement2SF = ifelse(is.na(SF2), 0,  SF2)
  basement_weighted = ifelse(basement1SF + basement2SF == 0, 0, as.integer((basement1 * basement1SF + basement2 * basement2SF) / (basement1SF + basement2SF)))
  return(basement_weighted)
}
Train_Basement = basement_weight(train_data$BsmtFinType1, train_data$BsmtFinType2, train_data$BsmtFinSF1, train_data$BsmtFinSF2)
Test_Basement = basement_weight(test_data$BsmtFinType1, test_data$BsmtFinType2, test_data$BsmtFinSF1, test_data$BsmtFinSF2)

#Combined living space feature
cleaned_train_data$TotLivArea = train_data$GrLivArea + (Train_Basement / 6) * ifelse(is.na(train_data$TotalBsmtSF), 0, train_data$TotalBsmtSF)
cleaned_test_data$TotLivArea = test_data$GrLivArea + (Test_Basement / 6) * ifelse(is.na(test_data$TotalBsmtSF), 0, test_data$TotalBsmtSF)


#Combining number of bathrooms above ground and basement
cleaned_train_data$FullBath = train_data$FullBath + (Train_Basement/ 6) * train_data$BsmtFullBath 
cleaned_test_data$FullBath = test_data$FullBath + 
  (Test_Basement / 6) * ifelse(is.na(test_data$BsmtFullBath), 0,  test_data$BsmtFullBath) 

cleaned_train_data$HalfBath = train_data$HalfBath + (Train_Basement/ 6) * train_data$BsmtHalfBath
cleaned_test_data$HalfBath = test_data$HalfBath +
  (Test_Basement / 6) * ifelse(is.na(test_data$BsmtHalfBath), 0, test_data$BsmtHalfBath)

#Combined enclosed porch feature
cleaned_train_data$PorchSF = train_data$EnclosedPorch + train_data$X3SsnPorch + train_data$ScreenPorch
cleaned_test_data$PorchSF = test_data$EnclosedPorch + test_data$X3SsnPorch + test_data$ScreenPorch

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
