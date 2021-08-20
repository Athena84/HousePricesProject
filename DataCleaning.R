
#Read the training data
raw_data <- read.csv("./Data/train.csv", stringsAsFactors = TRUE)
#head(raw_data)
summary(raw_data)

#Check missing values as % for columns
colMeans(is.na(raw_data)) * 100

selected_cols <- c("Id", "MSSubClass", "LotArea", "BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "BedroomAbvGr", "FullBath", "GrLivArea", "MoSold", "YrSold", "SaleType", "SaleCondition", "SalePrice")
cleaned_data <- raw_data[selected_cols]

#Save cleaned data set
write.csv(cleaned_data, "./Data/cleaned.csv", row.names = FALSE)

