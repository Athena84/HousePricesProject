library(car)

#Read the training data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = FALSE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = FALSE)
cleaned_test_data <- read.csv("./Data/cleaned_test.csv", stringsAsFactors = FALSE)

#Normalize Living area values
#mu <- mean(rbind(cleaned_train_data$TotLivArea, cleaned_test_data$TotLivArea))
#sigma <- sd(rbind(cleaned_train_data$TotLivArea, cleaned_test_data$TotLivArea))
#cleaned_train_data$TotLivArea = (cleaned_train_data$TotLivArea - mu) / sigma
#cleaned_test_data$TotLivArea = (cleaned_test_data$TotLivArea - mu) / sigma

#Taking ln of prices to remove skew
#cleaned_target$SalePrice <- log(cleaned_target$SalePrice)

#Fitting linear model of saleprice by size and quality and taking residuals
model_aggregate_quality = lm(cleaned_target$SalePrice ~ cleaned_train_data$TotLivArea + cleaned_train_data$OverallQual + cleaned_train_data$GarageCars)
residuals <- cleaned_target$SalePrice - predict(model_aggregate_quality, cleaned_train_data)
summary(model_aggregate_quality)


#Convert quality text into rankings implied by the text
convert_quality_factor <- function(Qual, imputed_Val) {
  ranking = ifelse(is.na(Qual), imputed_Val, Qual)
  ranking = gsub("Po", 1, ranking)
  ranking = gsub("Fa", 2, ranking)
  ranking = gsub("TA", 3, ranking)
  ranking = gsub("Gd", 4, ranking)
  ranking = gsub("Ex", 5, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}

for (col in c("KitchenQual", "ExterQual", "ExterCond", "HeatingQC", "BsmtCond")) {
  cleaned_train_data[,col] <- convert_quality_factor(cleaned_train_data[,col], 1)
  cleaned_test_data[,col] <- convert_quality_factor(cleaned_test_data[,col], 1)
}
for (col in c("GarageQual", "PoolQC")) {
  cleaned_train_data[,col] <- convert_quality_factor(cleaned_train_data[,col], 0)
  cleaned_test_data[,col] <- convert_quality_factor(cleaned_test_data[,col], 0)
}

#Re-order nominal categorical values according to order of median of residuals of the basic linear model 
for (col in c("Neighborhood", "MSSubClass", "MSZoning", "SaleCondition")) {
  cleaned_train_data[,col] <- factor(reorder(cleaned_train_data[,col], residuals, median))
  cleaned_test_data[,col] <- as.integer(factor(cleaned_test_data[,col], levels = levels(cleaned_train_data[,col])))
  cleaned_train_data[,col] <- as.integer(cleaned_train_data[,col])
}


#Re-order nominal categorical values manually

#Convert Utility text into ranking
convert_utility_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 4, Qual)
  ranking = gsub("AllPub", 4, ranking)
  ranking = gsub("NoSewr", 3, ranking)
  ranking = gsub("NoSeWa", 2, ranking)
  ranking = gsub("ELO", 1, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$Utilities = convert_utility_factor(cleaned_train_data$Utilities)
cleaned_test_data$Utilities = convert_utility_factor(cleaned_test_data$Utilities)

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

#Convert Slope quality text into ranking
convert_slope_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("Gtl", 0, ranking)
  ranking = gsub("Mod", 1, ranking)
  ranking = gsub("Sev", 2, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$LandSlope = convert_slope_factor(cleaned_train_data$LandSlope)
cleaned_test_data$LandSlope = convert_slope_factor(cleaned_test_data$LandSlope)

#Convert LotShape text into ranking
convert_lotshape_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("Reg", 0, ranking)
  ranking = gsub("IR1", 1, ranking)
  ranking = gsub("IR2", 2, ranking)
  ranking = gsub("IR3", 3, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$LotShape  = convert_lotshape_factor(cleaned_train_data$LotShape )
cleaned_test_data$LotShape  = convert_lotshape_factor(cleaned_test_data$LotShape )

#Convert Basement exposure text into ranking
convert_BsmtExp_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("No", 1, ranking)
  ranking = gsub("Mn", 2, ranking)
  ranking = gsub("Av", 3, ranking)
  ranking = gsub("Gd", 4, ranking)
  ranking = as.integer(ranking)
  return(ranking)
}
cleaned_train_data$BsmtExposure = convert_BsmtExp_factor (cleaned_train_data$BsmtExposure)
cleaned_test_data$BsmtExposure = convert_BsmtExp_factor (cleaned_test_data$BsmtExposure)


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
cleaned_train_data$BldgType = convert_buildtype_factor(cleaned_train_data$BldgType)
cleaned_test_data$BldgType = convert_buildtype_factor(cleaned_test_data$BldgType)

#Convert Masonry Type text into ranking
convert_masonry_factor <- function(Qual) {
  ranking = ifelse(is.na(Qual), 0, Qual)
  ranking = gsub("None", 0, ranking)
  ranking = gsub("BrkCmn", 1, ranking)
  ranking = gsub("BrkFace", 2, ranking)
  ranking = gsub("Stone", 3, ranking)
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
cleaned_train_data$Functional = convert_functional_factor(cleaned_train_data$Functional)
cleaned_test_data$Functional = convert_functional_factor(cleaned_test_data$Functional)


#Save prepped data set for XGB
write.csv(cleaned_train_data, "./Data/prepped_train.csv", row.names = FALSE)
write.csv(cleaned_target, "./Data/prepped_target.csv", row.names = FALSE)
write.csv(cleaned_test_data, "./Data/prepped_test.csv", row.names = FALSE)


#==============================================================================
#Check missing values as % for columns
colMeans(is.na(cleaned_train_data)) * 100
colMeans(is.na(cleaned_test_data)) * 100
