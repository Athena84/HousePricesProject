
library(tidyverse)
library(ggthemes)
library(scales)
library(car)

old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
                       ) 

#Read the training data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = TRUE)
cleaned_test_data <- read.csv("./Data/cleaned_test.csv", stringsAsFactors = TRUE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = TRUE)
cleaned_train_data$SalePrice = cleaned_target$SalePrice

#Fitting linear model of saleprice by size and quality and taking residuals
model_aggregate_quality = lm(cleaned_target$SalePrice ~ cleaned_train_data$TotLivArea + cleaned_train_data$OverallQual)
residuals <- cleaned_target$SalePrice - predict(model_aggregate_quality, cleaned_train_data)


#Distribution of house prices
Density_Prices <- ggplot(data = cleaned_train_data, aes(x = SalePrice)) +
  geom_density()
  
Density_Prices

#Boxplots house prices grouped for categorical


cleaned_train_data$OverallCond = as.factor(cleaned_train_data$OverallCond)
OverallCond_boxplot <- ggplot(data = cleaned_train_data, aes(x = OverallCond, y = SalePrice)) +
  geom_boxplot()
OverallCond_boxplot

cleaned_train_data$BldgType = as.factor(cleaned_train_data$BldgType)
BldgType_boxplot <- ggplot(data = cleaned_train_data, aes(x = BldgType, y = SalePrice)) +
  geom_boxplot()
BldgType_boxplot

cleaned_train_data$RoofStyle = as.factor(cleaned_train_data$RoofStyle)
RoofStyle_boxplot <- ggplot(data = cleaned_train_data, aes(x = RoofStyle, y = SalePrice)) +
  geom_boxplot()
RoofStyle_boxplot

cleaned_train_data$RoofMatl = as.factor(cleaned_train_data$RoofMatl)
RoofMatl_boxplot <- ggplot(data = cleaned_train_data, aes(x = RoofMatl, y = SalePrice)) +
  geom_boxplot()
RoofMatl_boxplot

cleaned_train_data$Exterior1st = as.factor(cleaned_train_data$Exterior1st)
Exterior1st_boxplot <- ggplot(data = cleaned_train_data, aes(x = Exterior1st, y = SalePrice)) +
  geom_boxplot()
Exterior1st_boxplot

cleaned_train_data$MasVnrType = as.factor(cleaned_train_data$MasVnrType)
MasVnrType_boxplot <- ggplot(data = cleaned_train_data, aes(x = MasVnrType, y = SalePrice)) +
  geom_point()
MasVnrType_boxplot

cleaned_train_data$Foundation = as.factor(cleaned_train_data$Foundation)
Foundation_boxplot <- ggplot(data = cleaned_train_data, aes(x = Foundation, y = SalePrice)) +
  geom_boxplot()
Foundation_boxplot

cleaned_train_data$Electrical = as.factor(cleaned_train_data$Electrical)
Electrical_boxplot <- ggplot(data = cleaned_train_data, aes(x = Electrical, y = SalePrice)) +
  geom_boxplot()
Electrical_boxplot

cleaned_train_data$GarageType = as.factor(cleaned_train_data$GarageType)
GarageType_boxplot <- ggplot(data = cleaned_train_data, aes(x = GarageType, y = SalePrice)) +
  geom_boxplot()
GarageType_boxplot

cleaned_train_data$MSZoning = as.factor(cleaned_train_data$MSZoning)
MSZoning_boxplot <- ggplot(data = cleaned_train_data, aes(x = MSZoning, y = SalePrice)) +
  geom_boxplot()
MSZoning_boxplot

#Works if data still as names:
cleaned_train_data$MSSubClass = as.factor(cleaned_train_data$MSSubClass)
MSSubClass_boxplot <- ggplot(data = cleaned_train_data, aes(x = reorder(MSSubClass, SalePrice, median), y = SalePrice)) +
  geom_boxplot()
MSSubClass_boxplot

#Works if converted to numbers:
cleaned_train_data$MSSubClass = as.factor(cleaned_train_data$MSSubClass)
MSSubClass_boxplot <- ggplot(data = cleaned_train_data, aes(x = MSSubClass, y = SalePrice)) +
  geom_boxplot()
MSSubClass_boxplot

cleaned_train_data$Functional = as.factor(cleaned_train_data$Functional)
Functional_boxplot <- ggplot(data = cleaned_train_data, aes(x = Functional, y = SalePrice)) +
  geom_boxplot()
Functional_boxplot

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
Neighborhood_boxplot <- ggplot(cleaned_train_data, aes(x = cleaned_train_data$Neighborhood, y = residuals)) +
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

cleaned_train_data$FullBath = as.factor(cleaned_train_data$FullBath)
FullBath_boxplot <- ggplot(data = cleaned_train_data, aes(x = FullBath, y = SalePrice)) +
  geom_boxplot()
FullBath_boxplot

cleaned_train_data$HalfBath = as.factor(cleaned_train_data$HalfBath)
HalfBath_boxplot <- ggplot(data = cleaned_train_data, aes(x = HalfBath, y = SalePrice)) +
  geom_boxplot()
HalfBath_boxplot

cleaned_train_data$SaleType= as.factor(cleaned_train_data$SaleType)
SaleType_boxplot <- ggplot(data = cleaned_train_data, aes(x = SaleType, y = SalePrice)) +
  geom_boxplot()
SaleType_boxplot

cleaned_train_data$SaleCondition = as.factor(cleaned_train_data$SaleCondition)
SaleCondition_boxplot <- ggplot(data = cleaned_train_data, aes(x = SaleCondition, y = SalePrice)) +
  geom_boxplot()
SaleCondition_boxplot

cleaned_train_data$LotShape = as.factor(cleaned_train_data$LotShape)
LotShape_boxplot <- ggplot(data = cleaned_train_data, aes(x = LotShape, y = SalePrice)) +
  geom_boxplot()
LotShape_boxplot

cleaned_train_data$LotConfig = as.factor(cleaned_train_data$LotConfig)
LotConfig_boxplot <- ggplot(data = cleaned_train_data, aes(x = LotConfig, y = SalePrice)) +
  geom_boxplot()
LotConfig_boxplot

cleaned_train_data$Fence = as.factor(cleaned_train_data$Fence)
Fence_boxplot <- ggplot(data = cleaned_train_data, aes(x = Fence, y = SalePrice)) +
  geom_boxplot()
Fence_boxplot

cleaned_train_data$PoolQC = as.factor(cleaned_train_data$PoolQC)
PoolQC_boxplot <- ggplot(data = cleaned_train_data, aes(x = PoolQC, y = SalePrice)) +
  geom_boxplot()
PoolQC_boxplot

cleaned_train_data$Fireplaces = as.factor(cleaned_train_data$Fireplaces)
Fireplaces_boxplot <- ggplot(data = cleaned_train_data, aes(x = Fireplaces, y = SalePrice)) +
  geom_boxplot()
Fireplaces_boxplot

cleaned_train_data$KitchenAbvGr = as.factor(cleaned_train_data$KitchenAbvGr)
Kitchens_boxplot <- ggplot(data = cleaned_train_data, aes(x = KitchenAbvGr, y = SalePrice)) +
  geom_boxplot()
Kitchens_boxplot

cleaned_train_data$BedroomAbvGr = as.factor(cleaned_train_data$BedroomAbvGr)
Bedrooms_boxplot <- ggplot(data = cleaned_train_data, aes(x = BedroomAbvGr, y = SalePrice)) +
  geom_boxplot()
Bedrooms_boxplot

cleaned_train_data$CentralAir = as.factor(cleaned_train_data$CentralAir)
CentralAir_boxplot <- ggplot(data = cleaned_train_data, aes(x = CentralAir, y = SalePrice)) +
  geom_boxplot()
CentralAir_boxplot

cleaned_train_data$Heating = as.factor(cleaned_train_data$Heating)
Heating_boxplot <- ggplot(data = cleaned_train_data, aes(x = Heating, y = SalePrice)) +
  geom_boxplot()
Heating_boxplot

cleaned_train_data$BsmtExposure = as.factor(cleaned_train_data$BsmtExposure)
BsmtExposure_boxplot <- ggplot(data = cleaned_train_data, aes(x = BsmtExposure, y = SalePrice)) +
  geom_boxplot()
BsmtExposure_boxplot

cleaned_train_data$BsmtCond = as.factor(cleaned_train_data$BsmtCond)
BsmtCond_boxplot <- ggplot(data = cleaned_train_data, aes(x = BsmtCond, y = SalePrice)) +
  geom_boxplot()
BsmtCond_boxplot

cleaned_train_data$Alley = as.factor(cleaned_train_data$Alley)
Alley_boxplot <- ggplot(data = cleaned_train_data, aes(x = Alley, y = SalePrice)) +
  geom_boxplot()
Alley_boxplot

cleaned_train_data$Street = as.factor(cleaned_train_data$Street)
Street_boxplot <- ggplot(data = cleaned_train_data, aes(x = Street, y = SalePrice)) +
  geom_boxplot()
Street_boxplot


#Scatter plots house prices vs numerical

wLivingSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = WeightedTotLivArea, y = SalePrice)) +
  geom_point()
YrSold_scatterplot <- ggplot(data = cleaned_train_data, aes(x = YrSold, y = SalePrice)) +
  geom_point()
MoSold_scatterplot <- ggplot(data = cleaned_train_data, aes(x = MoSold, y = SalePrice)) +
  geom_point()
Porch_scatterplot <- ggplot(data = cleaned_train_data, aes(x = PorchSF, y = SalePrice)) +
  geom_point()
LowQualFinSF_scatterplot <- ggplot(data = cleaned_train_data, aes(x = LowQualFinSF, y = SalePrice)) +
  geom_point()
LotFrontage_scatterplot <- ggplot(data = cleaned_train_data, aes(x = LotFrontage, y = SalePrice)) +
  geom_point()

AboveGrSpace_scatterplot
BasementSpace_scatterplot
wBasementSpace_scatterplot
LivingSpace_scatterplot
wLivingSpace_scatterplot
YrSold_scatterplot
MoSold_scatterplot
Porch_scatterplot
LowQualFinSF_scatterplot
LotFrontage_scatterplot




#================Presentation selection
cleaned_train_data$OverallQual = as.factor(cleaned_train_data$OverallQual)
OverallQual_boxplot <- ggplot(data = cleaned_train_data, aes(x = OverallQual, y = SalePrice)) +
  geom_boxplot() + 
  labs(title="Distribution of House prices by overall quality rating", x ="Overal qaulity rating", y = "House price ($)") +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
OverallQual_boxplot2 <- ggplot(data = cleaned_train_data, aes(x = OverallQual, y = TotLivArea)) +
  geom_boxplot() + 
  labs(title="Distribution of living area by overall quality rating", x ="Overal qaulity rating", y = "Linving area (sqf)") +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

OverallQual_boxplot
OverallQual_boxplot2

AboveGrSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = GrLivArea, y = SalePrice)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
BasementSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
LivingSpace_scatterplot <- ggplot(data = cleaned_train_data, aes(x = TotLivArea, y = SalePrice)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
LivingSpace_quality_scatterplot <- ggplot(data = cleaned_train_data, aes(x = TotLivArea, y = SalePrice, color = OverallQual)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))

AboveGrSpace_scatterplot
BasementSpace_scatterplot
LivingSpace_scatterplot
LivingSpace_quality_scatterplot


selection <- select(cleaned_train_data, c("SalePrice", "TotLivArea", "OverallQual"))
selection <- selection[(selection$TotLivArea < 4000),]
selection <- selection[(selection$TotLivArea > 1000),]
selection$TotLivArea = as.factor(cut_width(selection$TotLivArea / 1000, width = 0.25, boundary = 1))
grouped <- group_by(selection, TotLivArea, OverallQual) %>%
  summarize(., avSalePrice = mean(SalePrice))
grouped_plot <- ggplot(data = grouped, aes(x = TotLivArea, y = avSalePrice, color = OverallQual)) +
  geom_point() + 
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3))
grouped_plot
