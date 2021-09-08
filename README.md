This project was executed as final assignment of a module of the NYCDSA Data Science Bootcamp.

The project assignment is to create a model based on the AMES house prices dataset as provided by the Kaggle competition with the same name, it includes 1460 observations of house prices with 79 explaining features.
The model is supposed to forecast the house prices for a similar dataset of 1459 observations of the same features in years afterwards.

The file DataCleaning.R reads the data supplied by the Kaggle competition and applies the basic feature engineering required such as combining and splitting features, treating missing values.

The file DataPrepForXGB fits a 2 variable linear model and transforms the features for XGB modelling on its residuals, such as ranking categorical data in a different order, standardization of numerical features, log transformation of the target variable etc.

The file PredModel.ipynb fits an XGB model through a 2-step grid search, applies it to the test data set and combines the result with the results of the basic linear model.

The files Visualizations.R and LinModels.R were used to visualize the results towards a presentation and for internal analysis and model selection respectively.