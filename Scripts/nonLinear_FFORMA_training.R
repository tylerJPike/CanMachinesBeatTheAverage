# File: nonLinear_FFORMA_training.R
# Author: Tyler Pike
# Date: 10/1/2019
# Note(s): Script to train FFORMA model. Currently configured to only use data from 1970 through 2000.   

# set working directory
setwd('/scratch/m1tjp01/Paco/Nonlinear/ForecastCombination/')

# load libraries
library(M4metalearning)
library(M4comp2018)
library(tsfeatures)
library(xgboost)
library(zoo)

# set random seed
set.seed(1116)

# time process 
startTime = Sys.time()

# set sample data to only include dates from 1975 to 1980
# 17406 series meet this criteria
M4_train = list()
j = 0
for(i in 1:100000){ #length(M4)
  if(1970 %in% zoo::index(M4[[i]]$x) & 
     2000 %in% zoo::index(M4[[i]]$x)){
    
    j = j + 1
    M4_train[[j]] = M4[[i]]
    M4_train[[j]]$x = M4_train[[j]]$x[index(M4_train[[j]]$x) >= 1970 &
                                          index(M4_train[[j]]$x) < 2000]
  }
}

M4_train = temp_holdout(M4_train)


# Calculate individual time series forecasting models
# first we the historical mean and AR(12) forecast methods
forecastList = forec_methods()
meanForecast = function(x,h){return(forecast::forecast(forecast::meanf(y=x,h))$mean)}
arForecast = function(x,h){return(forecast::forecast(forecast::Arima(y=x,order = c(12,0,0)), h = h )$mean)}
forecastList = append(forecastList,'meanForecast')
forecastList = append(forecastList, 'arForecast')
# then we calculate forecasts
M4_train <- calc_forecasts(M4_train, forecastList, n.cores=20)

# once we have the forecasts, we can calculate the errors
M4_train <- calc_errors(M4_train)
M4_train <- THA_features(M4_train, n.cores = 20)
train_data <- create_feat_classif_problem(M4_train)

# train the meta model
FFORMA_model <- train_selection_ensemble(train_data$data, train_data$errors)

# time process
endTime = Sys.time()
print(endTime - startTime)

# save the trained model
saveRDS(FFORMA_model, file = './Data/Models/FFORMA_model_1970_2000.rds')



