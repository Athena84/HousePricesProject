
#Read the training data
train_data <- read.csv("./Data/train.csv", stringsAsFactors = TRUE)
test_data <- read.csv("./Data/test.csv", stringsAsFactors = TRUE)

#head(train_data)
#head(test_data)
#summary(train_data)


#Check missing values as % for columns
#colMeans(is.na(train_data)) * 100

selected_cols <- c("Id", "MSSubClass", "LotArea", "BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "BedroomAbvGr", "FullBath", "GrLivArea", "MoSold", "YrSold", "SaleType", "SaleCondition")
cleaned_train_data <- train_data[selected_cols]
cleaned_test_data <- test_data[selected_cols]
cleaned_train_target <- train_data["SalePrice"]

#Save cleaned data set
write.csv(cleaned_train_data, "./Data/cleaned_train.csv", row.names = FALSE)
write.csv(cleaned_train_target, "./Data/cleaned_train_target.csv", row.names = FALSE)
write.csv(cleaned_test_data, "./Data/cleaned_test.csv", row.names = FALSE)

