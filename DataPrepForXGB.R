library(car)

#Read the data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = FALSE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = FALSE)
cleaned_test_data <- read.csv("./Data/cleaned_test.csv", stringsAsFactors = FALSE)


#Normalize area values for combination of train and test data
mu <- mean(c(cleaned_train_data$TotLivArea, cleaned_test_data$TotLivArea))
sigma <- sd(c(cleaned_train_data$TotLivArea, cleaned_test_data$TotLivArea))
cleaned_train_data$TotLivArea = (cleaned_train_data$TotLivArea - mu) / sigma
cleaned_test_data$TotLivArea = (cleaned_test_data$TotLivArea - mu) / sigma

mu <- mean(c(cleaned_train_data$LotArea, cleaned_test_data$LotArea))
sigma <- sd(c(cleaned_train_data$LotArea, cleaned_test_data$LotArea))
cleaned_train_data$LotArea = (cleaned_train_data$LotArea - mu) / sigma
cleaned_test_data$LotArea = (cleaned_test_data$LotArea - mu) / sigma

mu <- mean(c(cleaned_train_data$LotFrontage, cleaned_test_data$LotFrontage))
sigma <- sd(c(cleaned_train_data$LotFrontage, cleaned_test_data$LotFrontage))
cleaned_train_data$LotFrontage = (cleaned_train_data$LotFrontage - mu) / sigma
cleaned_test_data$LotFrontage = (cleaned_test_data$LotFrontage - mu) / sigma

#Fitting linear model of saleprice by size, quality, condition and taking residuals
#Taking ln of prices to remove skew and adding it to train df for modelling
residuals <- log(cleaned_target$SalePrice)

model_data <- cleaned_train_data[c("OverallQual", "TotLivArea")]
model_data$LNSalePrice <- log(cleaned_target$SalePrice)
model_base = lm(LNSalePrice ~ TotLivArea + OverallQual, data = model_data)
residuals <- model_data$LNSalePrice - predict(model_base, model_data)
summary(model_base)
#plot(model_base)
#plot(residuals)
#influencePlot(model_base)
#vif(model_base)
#avPlots(model_base)
#confint(model_base)

#Calculating prediction base model on test date (XGBoost prediction on residuals will be added to this later)
pred_base = data.frame(predictions = predict(model_base, cleaned_test_data))

#For XGBoost, leave residuals of trained model as target for train data
cleaned_target$SalePrice = residuals

#Convert quality text into rankings implied by the text
convert_quality_factor <- function(Qual, imputed_Val) {
  ranking = ifelse(is.na(Qual), imputed_Val, Qual)
  ranking = gsub("Po", 2, ranking)
  ranking = gsub("Fa", 3, ranking)
  ranking = gsub("TA", 4, ranking)
  ranking = gsub("Gd", 5, ranking)
  ranking = gsub("Ex", 6, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}

for (col in c("KitchenQual", "ExterQual", "ExterCond", "HeatingQC", "BsmtCond")) {
  cleaned_train_data[,col] <- convert_quality_factor(cleaned_train_data[,col], 2)
  cleaned_test_data[,col] <- convert_quality_factor(cleaned_test_data[,col], 2)
}
for (col in c("GarageQual", "PoolQC")) {
  cleaned_train_data[,col] <- convert_quality_factor(cleaned_train_data[,col], 1)
  cleaned_test_data[,col] <- convert_quality_factor(cleaned_test_data[,col], 1)
}

#Combine new MSSubclass label into closest existing label
cleaned_test_data$MSSubClass <- gsub("150", "160", cleaned_test_data$MSSubClass)

#Re-order nominal categorical values according to order of median of residuals of the base linear model 
for (col in c("Neighborhood", "MSZoning", "MSSubClass", "SaleCondition", "LandSlope")) {
  cleaned_train_data[,col] <- factor(reorder(cleaned_train_data[,col], residuals, median))
  cleaned_test_data[,col] <- as.integer(factor(cleaned_test_data[,col], levels = levels(cleaned_train_data[,col])))
  cleaned_train_data[,col] <- as.integer(cleaned_train_data[,col])
}

#Re-order nominal categorical values manually

#Convert Fence quality text into ranking
convert_fence_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 5, Qual)
  ranking = gsub("MnWw", 1, ranking)
  ranking = gsub("GdWo", 2, ranking)
  ranking = gsub("MnPrv", 3, ranking)
  ranking = gsub("GdPrv", 4, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Fence = convert_fence_factor(cleaned_train_data$Fence)
cleaned_test_data$Fence = convert_fence_factor(cleaned_test_data$Fence)

#Convert Building Type text into ranking
convert_buildtype_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("Twnhs", 4, ranking)
  ranking = gsub("TwnhsE", 2, ranking)
  ranking = gsub("Duplex", 3, ranking)
  ranking = gsub("2fmCon", 1, ranking)
  ranking = gsub("1Fam", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$BldgType = convert_buildtype_factor(cleaned_train_data$BldgType)
cleaned_test_data$BldgType = convert_buildtype_factor(cleaned_test_data$BldgType)

#Convert Masonry Type text into ranking
convert_masonry_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("None", 1, ranking)
  ranking = gsub("BrkCmn", 2, ranking)
  ranking = gsub("BrkFace", 3, ranking)
  ranking = gsub("Stone", 4, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$MasVnrType = convert_masonry_factor(cleaned_train_data$MasVnrType)
cleaned_test_data$MasVnrType = convert_masonry_factor(cleaned_test_data$MasVnrType)

#Convert Foundation Type text into ranking
convert_foundation_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 2, Qual)
  ranking = gsub("Slab", 1, ranking)
  ranking = gsub("Stone", 2, ranking)
  ranking = gsub("Wood", 2, ranking)
  ranking = gsub("BrkTil", 2, ranking)
  ranking = gsub("CBlock", 2, ranking)
  ranking = gsub("PConc", 3, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Foundation = convert_foundation_factor(cleaned_train_data$Foundation)
cleaned_test_data$Foundation = convert_foundation_factor(cleaned_test_data$Foundation)

#Convert Electricity Type text into ranking
convert_elec_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 4, Qual)
  ranking = gsub("FuseP", 1, ranking)
  ranking = gsub("FuseF", 2, ranking)
  ranking = gsub("FuseA", 3, ranking)
  ranking = gsub("SBrkr", 4, ranking)
  ranking = gsub("Mix", 1, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Electrical = convert_elec_factor(cleaned_train_data$Electrical)
cleaned_test_data$Electrical = convert_elec_factor(cleaned_test_data$Electrical)


#Convert LotShape text into ranking
convert_lotshape_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("Reg", 1, ranking)
  ranking = gsub("IR1", 2, ranking)
  ranking = gsub("IR2", 3, ranking)
  ranking = gsub("IR3", 4, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$LotShape  = convert_lotshape_factor(cleaned_train_data$LotShape)
cleaned_test_data$LotShape  = convert_lotshape_factor(cleaned_test_data$LotShape)

#Convert Basement exposure text into ranking
convert_BsmtExp_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 1, Qual)
  ranking = gsub("No", 2, ranking)
  ranking = gsub("Mn", 3, ranking)
  ranking = gsub("Av", 4, ranking)
  ranking = gsub("Gd", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$BsmtExposure = convert_BsmtExp_factor (cleaned_train_data$BsmtExposure)
cleaned_test_data$BsmtExposure = convert_BsmtExp_factor (cleaned_test_data$BsmtExposure)

#Convert Functional text into ranking
convert_functional_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 2, Qual)
  ranking = gsub("Sal", 1, ranking)
  ranking = gsub("Sev", 1, ranking)
  ranking = gsub("Maj1", 2, ranking)
  ranking = gsub("Maj2", 2, ranking)
  ranking = gsub("Mod", 3, ranking)
  ranking = gsub("Min1", 4, ranking)
  ranking = gsub("Min2", 4, ranking)
  ranking = gsub("Typ", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Functional = convert_functional_factor(cleaned_train_data$Functional)
cleaned_test_data$Functional = convert_functional_factor(cleaned_test_data$Functional)

#Save prepped data set for XGB
write.csv(cleaned_train_data, "./Data/prepped_train.csv", row.names = FALSE)
write.csv(cleaned_test_data, "./Data/prepped_test.csv", row.names = FALSE)
write.csv(pred_base, "./Data/prediction_base.csv", row.names = FALSE)
write.csv(cleaned_target, "./Data/prepped_target.csv", row.names = FALSE)


#==============================================================================
#Check missing values as % for columns
colMeans(is.na(cleaned_train_data)) * 100
colMeans(is.na(cleaned_test_data)) * 100
