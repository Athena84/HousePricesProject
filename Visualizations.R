library(tidyverse)
library(ggthemes)
library(scales)
library(car)
library(ggpubr)

#Setting chart defaults
old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
                       ) 

#Read the data
cleaned_train_data <- read.csv("./Data/cleaned_train.csv", stringsAsFactors = TRUE)
cleaned_target <- read.csv("./Data/cleaned_target.csv", stringsAsFactors = TRUE)
prepped_train_data <- read.csv("./Data/prepped_train.csv", stringsAsFactors = TRUE)
prepped_target <- read.csv("./Data/prepped_target.csv", stringsAsFactors = FALSE)
cleaned_train_data$SalePrice = cleaned_target$SalePrice
prepped_train_data$SalePrice = cleaned_target$SalePrice
prepped_train_data$LNSalePrice = prepped_target$SalePrice

#Fitting linear model of saleprice by size and quality and taking residuals
model_base = lm(LNSalePrice ~ TotLivArea + OverallQual, data = prepped_train_data)
prepped_train_data$residuals <- prepped_train_data$LNSalePrice - predict(model_base, prepped_train_data)

#Renaming features
improved_colnames <- c("Id", "Lot area", "Lot frontage", "Overall quality", "# Cars garage", "# Rooms", "Overall condition", "# Fireplaces", "Aircon", "Street", "Kitchen quality", "Exterior quality", "Exterior condition", "Heating quality", "Garage quality", "Pool quality", "Basement condition", "Utilities", "Fence type", "Land slope", "Lot shape", "Basement exposure", "Building type", "Masonry type", "Foundation", "Electricity", "Functionality", "Neighborhood", "House class", "Zoning", "Sale condition", "Year sold", "Age built", "Age renovated", "Total living area", "Proximity positive feat", "Proximity road", "Proximity railroad", "Heating type", "Basement quality", "# Full bathrooms", "# Half bathrooms", "Enclosed porch area", "House price")
colnames(cleaned_train_data) <- improved_colnames
improved_colnames <- c(improved_colnames, "Log house price", "residuals")
colnames(prepped_train_data) <- improved_colnames
#colnames(cleaned_train_data)
#apply(cleaned_train_data, 2, unique)
#apply(prepped_train_data, 2, unique)

#Rename neighborhoods
list_rename <- function(col, lstOld, lstNew){
  replacement_col <- col
  for (i in seq_along(lstOld)){
    replacement_col <- gsub(lstOld[i], lstNew[i], replacement_col)  
  }
  return(replacement_col)
}
data_neigh_names <-c("CollgCr", "Crawfor", "NoRidge", "Mitchel", "Somerst", "NWAmes", "OldTown", "BrkSide", "NridgHt", "NAmes", "SawyerW", "IDOTRR", "MeadowV", "Timber", "StoneBr", "ClearCr", "NPkVill", "Blmngtn", "BrDale","SWISU", "Blueste")
full_neigh_names <- c("College Creek", "Crawford", "Northridge", "Mitchell", "Somerset", "Northwest Ames", "Old Town", "Brookside", "Northridge Heights", "North Ames", "Sawyer West", "Iowa DOT and Rail Road", "Meadow Village", "Timberland", "Stone Brook", "Clear Creek", "Northpark Villa", "Bloomington Heights", "Briardale", "South & West of ISU", "Bluestem")
cleaned_train_data$Neighborhood <- list_rename(cleaned_train_data$Neighborhood, data_neigh_names, full_neigh_names)
prepped_train_data$Neighborhood <- list_rename(prepped_train_data$Neighborhood, data_neigh_names, full_neigh_names)

#Distribution of house prices
Density_Prices <- ggplot(data = cleaned_train_data, aes(x = SalePrice)) +
  geom_density() +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title="Distribution of house prices", x ="House prices ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_Prices

Density_LogPrices <- ggplot(data = prepped_train_data, aes(x = LNSalePrice)) +
  geom_density() +
  labs(title="Distribution of log of house prices", x ="Log of house prices ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_LogPrices

#Normality test
shapiro.test(cleaned_train_data$SalePrice)
shapiro.test(log(cleaned_train_data$SalePrice))
shapiro.test(prepped_train_data$LNSalePrice)
qqPlot(cleaned_train_data$SalePrice, id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of House prices")
qqPlot(log(cleaned_train_data$SalePrice, base = exp(1)), id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of Log house prices")
qqPlot(prepped_train_data$LNSalePrice, id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of residuals base model")


#Correlation plots

#Correlation cleaned un-ranked data to saleprice
#Note, there will be missing values as this data wasn't processed by the prep-file yet
selected_cols <- c("Kitchen quality", "Exterior quality", "Exterior condition", "Heating quality", "Garage quality", "Pool quality", "Basement condition", "Utilities", "Fence type", "Land slope", "Lot shape", "Basement exposure", "Building type", "Masonry type", "Foundation", "Electricity", "Functionality", "Neighborhood", "House class", "Zoning", "Sale condition", "Proximity positive feat", "Proximity road", "Proximity railroad", "Heating type", "Basement quality", "# Full bathrooms", "# Half bathrooms", "Enclosed porch area")
for (col in selected_cols) {
  cleaned_train_data[,col] <- as.numeric(cleaned_train_data[,col])
}
selected_cols <- c("House price", "Total living area", "Lot area", "Lot frontage", "Overall quality", "# Cars garage", "# Rooms", "Overall condition", "# Fireplaces","Year sold", "Age built", "Age renovated", selected_cols)


correlations_Price <- data.frame(correlations = abs(cor(cleaned_train_data[selected_cols], method = "spearman")[1,]))
correlations_Price$features <- rownames(correlations_Price)
positions <- arrange(correlations_Price, desc(correlations))$features[2:11]
correlations_Price_plot <- ggplot(data = correlations_Price) +
  labs(title="Ordered Spearman correlation to house prices", x ="", y = "Correlation") +
  geom_bar(aes(x = features, y= correlations),stat="identity") + 
  scale_x_discrete(limits = positions) + 
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

correlations_Price_plot

#Correlation ranked data to log saleprice (first save data through dataprep without lin model)
selected_cols <- c("Log house price", "Lot area", "Lot frontage", "Overall quality", "# Cars garage", "# Rooms", "Overall condition", "# Fireplaces", "Year sold", "Age built", "Age renovated", "Total living area", "Kitchen quality", "Exterior quality", "Exterior condition", "Heating quality", "Garage quality", "Pool quality", "Basement condition", "Fence type", "Land slope", "Lot shape", "Basement exposure", "Building type", "Masonry type", "Foundation", "Electricity", "Functionality", "Neighborhood", "House class", "Zoning", "Sale condition", "Proximity positive feat", "Proximity road", "Proximity railroad", "Heating type", "Basement quality", "# Full bathrooms", "# Half bathrooms", "Enclosed porch area")

correlations_LNPrice <- data.frame(correlations = abs(cor(prepped_train_data[selected_cols], method = "spearman")[1,]))
correlations_LNPrice$features <- rownames(correlations_LNPrice)
positions <- arrange(correlations_LNPrice, desc(correlations))$features[2:11]
correlations_LNPrice_plot <- ggplot(data = correlations_LNPrice) +
  labs(title="Ordered Spearman correlation to log prices", x ="", y = "Correlation") +
  geom_bar(aes(x = features, y= correlations),stat="identity") + 
  scale_x_discrete(limits = positions) + 
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

correlations_LNPrice_plot

#Correlation ranked data to residuals of base model (first save data through dataprep with lin model)
selected_cols <- c("Log house price", "Lot area", "Lot frontage", "Overall quality", "# Cars garage", "# Rooms", "Overall condition", "# Fireplaces", "Year sold", "Age built", "Age renovated", "Total living area", "Kitchen quality", "Exterior quality", "Exterior condition", "Heating quality", "Garage quality", "Pool quality", "Basement condition", "Fence type", "Land slope", "Lot shape", "Basement exposure", "Building type", "Masonry type", "Foundation", "Electricity", "Functionality", "Neighborhood", "House class", "Zoning", "Sale condition", "Proximity positive feat", "Proximity road", "Proximity railroad", "Heating type", "Basement quality", "# Full bathrooms", "# Half bathrooms", "Enclosed porch area")

correlations_residuals <- data.frame(correlations = abs(cor(prepped_train_data[selected_cols], method = "spearman")[1,]))
correlations_residuals$features <- rownames(correlations_residuals)
positions <- arrange(correlations_residuals, desc(correlations))$features[2:11]
correlations_residuals_plot <- ggplot(data = correlations_residuals) +
  labs(title="Ordered Spearman correlation to residuals", x ="", y = "Correlation") +
  geom_bar(aes(x = features, y= correlations),stat="identity") + 
  scale_x_discrete(limits = positions) + 
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

correlations_residuals_plot

#Neighborhood plots
cleaned_train_data$Neighborhood = as.factor(cleaned_train_data$Neighborhood)
Neighborhood_boxplot <- ggplot(cleaned_train_data, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3), limits = c(0,600000)) +
  labs(title="House price distributions", x ="", y = "House prices ($)") +
  coord_flip()
Neighborhood_boxplot

Neighborhood_boxplot_ordered <- ggplot(data = cleaned_train_data, aes(x = reorder(Neighborhood, SalePrice, median), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3), limits = c(0,600000)) +
  labs(title="House price distributions", x ="", y = "House prices ($)") +
  coord_flip()
Neighborhood_boxplot_ordered

prepped_train_data$Neighborhood <- factor(reorder(cleaned_train_data$Neighborhood, prepped_train_data$residuals, median))
Neighborhood_boxplot_residuals <- ggplot(data = prepped_train_data, aes(x = Neighborhood, y = residuals)) +
  geom_boxplot() +
  labs(title="Residual Distributions", x ="", y = "Residuals of linear model of log price") +
  ylim(-0.5, 0.5) +
  coord_flip()
Neighborhood_boxplot_residuals


#Test mean house price per neighborhood
leveneTest(SalePrice ~ Neighborhood, data = cleaned_train_data) #Variances significantly different so one-way instead of ANOVA
oneway.test(SalePrice ~ Neighborhood, data = cleaned_train_data)











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
