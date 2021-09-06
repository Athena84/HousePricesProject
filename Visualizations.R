library(tidyverse)
library(ggthemes)
library(scales)
library(car)
library(ggpubr)
library("scatterplot3d")

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
prepped_train_data$LNSalePrice = log(cleaned_target$SalePrice)

#Fitting linear model of saleprice by size and quality and taking residuals
model_base = lm(LNSalePrice ~ TotLivArea + OverallQual, data = prepped_train_data)
summary(model_base)
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
shapiro.test(prepped_train_data$residuals)
qqPlot(cleaned_train_data$SalePrice, id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of House prices")
qqPlot(log(cleaned_train_data$SalePrice, base = exp(1)), id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of Log house prices")
qqPlot(prepped_train_data$residuals, id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of residuals base model")

residuals_xgb = read.csv("./Data/residuals.csv", stringsAsFactors = FALSE)
qqPlot(residuals_xgb, id = FALSE, ylab = "",
       xlab = "Normal distribution quantiles", main="Q-Q plot of residuals combined model")

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

#Plotting the linear model
scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 20)
scatterplot3d(x= prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 40)
scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 60)
scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 80)

scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 20)$plane3d(model_base)
scatterplot3d(x= prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 40)$plane3d(model_base)
scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 60)$plane3d(model_base)
scatterplot3d(x = prepped_train_data$OverallQual,
              y = prepped_train_data$TotLivArea,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Overall quality rating",
              ylab = "Normalized living area",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 80)$plane3d(model_base)

scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 20)
scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 40)
scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 60)
s3d = scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 80)

scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 20)$plane3d(model_base)
scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 40)$plane3d(model_base)
scatterplot3d(x = prepped_train_data$TotLivArea,
              y = prepped_train_data$OverallQual,
              z = prepped_train_data$LNSalePrice,
              main = "Log house prices by total area and Overall quality",
              xlab = "Normalized living area",
              ylab = "Overall quality rating",
              zlab = "Log house price",
              grid = TRUE, box = TRUE,
              angle = 60)$plane3d(model_base)
s3d = scatterplot3d(x = prepped_train_data$TotLivArea,
                    y = prepped_train_data$OverallQual,
                    z = prepped_train_data$LNSalePrice,
                    main = "Log house prices by total area and Overall quality",
                    xlab = "Normalized living area",
                    ylab = "Overall quality rating",
                    zlab = "Log house price",
                    grid = TRUE, box = TRUE,
                    angle = 80)$plane3d(model_base)


#Feature importance XGBoost
feat_importance <- read.csv("./Data/FeatImportance.csv", sep = ";", stringsAsFactors = FALSE)
positions <- arrange(feat_importance, desc(Importance))$Feature
feat_importance_plot <- ggplot(data = feat_importance) +
  labs(title="Appearance of features in XGBoost", x ="", y = "% appearance") +
  geom_bar(aes(x=Feature, y=Importance), stat = "identity") +
  scale_x_discrete(limits = positions) + 
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

feat_importance_plot

#Distribution plot predictions
predictions = read.csv("./Data/prediction.csv", stringsAsFactors = FALSE)
Density_Predictions <- ggplot(data = predictions, aes(x = SalePrice)) +
  geom_density() +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title="Distribution of predicted house prices", x ="House prices ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_Predictions

predictions$LNSalePrice = log(predictions$SalePrice)
Density_LNPredictions <- ggplot(data = predictions, aes(x = LNSalePrice)) +
  geom_density() +
  labs(title="Distribution of log of predicted house prices", x ="Log of house prices ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_LNPredictions

#Investment opportunity plots
selection <- cleaned_train_data[,c("SalePrice", "TotLivArea", "OverallQual", "OverallCond")]
selection <- selection[(selection$TotLivArea < 4000),]
selection <- selection[(selection$TotLivArea > 1000),]
selection$TotLivArea = as.factor(cut_width(selection$TotLivArea / 1000, width = 0.25, boundary = 1))

Qualgrouped <- group_by(selection, TotLivArea, OverallQual) %>%
  summarise(., avSalePrice = mean(SalePrice))
Qualgrouped_plot <- ggplot(data = Qualgrouped, aes(x = avSalePrice, y = TotLivArea, color = OverallQual)) +
  geom_point() + 
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title = "Average house prices per area and quality", x ="Average house price ($)", y = "Total living area (k sqf)", colour = "Quality rating")
Qualgrouped_plot

Condgrouped <- group_by(selection, TotLivArea, OverallCond) %>%
  summarise(., avSalePrice = mean(SalePrice))
Condgrouped_plot <- ggplot(data = Condgrouped, aes(x = avSalePrice, y = TotLivArea, color = OverallCond)) +
  geom_point() + 
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title = "Average house prices per area and condition", x ="Average house price ($)", y = "Total living area (k sqf)", colour = "Condition rating")
Condgrouped_plot


