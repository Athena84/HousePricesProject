
library(tidyverse)

#Read the training data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = TRUE)
cleaned_test_data <- read.csv("./Data/cleaned_test.csv", stringsAsFactors = TRUE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = TRUE)
cleaned_train_data$SalePrice = cleaned_target$SalePrice

#Distribution of house prices
Density_Prices <- ggplot(data = cleaned_train_data, aes(x = SalePrice)) +
  geom_density()
  
Density_Prices

#Boxplots house prices grouped for categorical
cleaned_train_data$OverallQual = as.factor(cleaned_train_data$OverallQual)
OverallQual_boxplot <- ggplot(data = cleaned_train_data, aes(x = OverallQual, y = SalePrice)) +
  geom_boxplot()
OverallQual_boxplot

cleaned_train_data$OverallCond = as.factor(cleaned_train_data$OverallCond)
OverallCond_boxplot <- ggplot(data = cleaned_train_data, aes(x = OverallCond, y = SalePrice)) +
  geom_boxplot()
OverallCond_boxplot

cleaned_train_data$BldgType = as.factor(cleaned_train_data$BldgType)
BldgType_boxplot <- ggplot(data = cleaned_train_data, aes(x = BldgType, y = SalePrice)) +
  geom_boxplot()
BldgType_boxplot


#Proximity general
cleaned_train_data$Condition1 = as.factor(cleaned_train_data$Condition1)
cleaned_train_data$Condition2 = as.factor(cleaned_train_data$Condition2)
Condition_boxplot <- ggplot(data = cleaned_train_data, aes(x = Condition1, y = SalePrice)) +
  geom_boxplot()
Condition_boxplot <- ggplot(data = cleaned_train_data, aes(x = Condition2, y = SalePrice)) +
  geom_boxplot()
Condition_boxplot

#Proximity specific
cleaned_train_data$ProxPos = as.factor(cleaned_train_data$ProxPos)
ProxPos_boxplot <- ggplot(data = cleaned_train_data, aes(x = ProxPos, y = SalePrice)) +
  geom_boxplot()
ProxPos_boxplot

#Works if data as integers (such as toward model)
cleaned_train_data$Neighborhood = as.factor(cleaned_train_data$Neighborhood)
Neighborhood_boxplot <- ggplot(data = cleaned_train_data, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot()
Neighborhood_boxplot

#Works if data still as names
Neighborhood_boxplot <- ggplot(data = cleaned_train_data, aes(x = reorder(Neighborhood, SalePrice, median), y = SalePrice)) +
  geom_boxplot()
Neighborhood_boxplot

cleaned_train_data$GarageCars = as.factor(cleaned_train_data$GarageCars)
GarageCars_boxplot <- ggplot(data = cleaned_train_data, aes(x = GarageCars, y = SalePrice)) +
  geom_boxplot()
GarageCars_boxplot


#Scatter plots house prices vs numerical
AboveGrSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = GrLivArea, y = SalePrice)) +
  geom_point()
BasementSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = TotalBsmtSF, y = SalePrice, color = BasementWeigths)) +
  geom_point()
wBasementSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = WeightedBasementSF, y = SalePrice)) +
  geom_point()
LivingSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = TotLivArea, y = SalePrice)) +
  geom_point()
wLivingSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = WeightedTotLivArea, y = SalePrice)) +
  geom_point()
YrSold_scatterplot <- ggplot(data = cleaned_train_data, aes(x = YrSold, y = SalePrice)) +
  geom_point()
MoSold_scatterplot <- ggplot(data = cleaned_train_data, aes(x = MoSold, y = SalePrice)) +
  geom_point()

AboveGrSpace_scatterplot
BasementSpace_scatterplot
wBasementSpace_scatterplot
LivingSpace_scatterplot
wLivingSpace_scatterplot
YrSold_scatterplot
MoSold_scatterplot
