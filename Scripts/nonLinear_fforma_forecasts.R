# File: nonLinear_metaLearner_functions_v0.R
# Author: Tyler Pike
# Section: MA-MFA
# Date: 7/30/2019
# Note(s): Use FFORMA to forecast US macro-series

# required libraries
# dplyr, lubridate, forecasts, foreach, parallel

# required scripts
# nonLinear_data_functions

print('RUNNING FFORMA ESTIMATION SCRIPT')

startTime = Sys.time()

generateWeights = T

#----------------------------------------------------
# Functions to calculate individual forecasts
#----------------------------------------------------
# individual forecast methods
meanForecast = function(x,h){return(forecast::forecast(forecast::meanf(y=x,h))$mean)}
arForecast = function(x,h){return(forecast::forecast(forecast::Arima(y=x,order = c(12,0,0)), h = h )$mean)}

# get forecast horizon for FFORMA
getHorizon = function(){return(Horizon)}
getFreq    = function(){return(Freq)}

#----------------------------------------------------
# Machine learning helper functions
#----------------------------------------------------
# import trained FFORMA model
FFORMA_model = readRDS(file = './Data/Models/FFORMA_model.rds')
getFFORMA = function(){return(FFORMA_model)}

# ML estimation controls 
getControl = function(){
  return(trainControl(method = "cv", 
                      number = 5,
                      allowParallel = TRUE,
                      savePredictions = TRUE))
}

# ML methods
getEngineMethod = function(x){
  if(x == 'RF'){return('rf')}
  if(x == 'GBM'){return('gbm')}
  if(x == 'NN'){return('avNNet')}
}

# ML tuning grids
getTuningGrid = function(x){
  # GBM tuning grid
  tuneGridGBM <- expand.grid(n.minobsinnode = c(1), 
                             shrinkage = c(.1,.01), 
                             n.trees = c(200,400,600),
                             interaction.depth = c(1,2)) 
  # RF tuning grid
  tuneGridRF <- expand.grid(mtry = c(1:4))
  # NN tuning grid
  tuneGridNN <- expand.grid(size = seq(2,10,1),
                            decay = c(.01,.001),
                            bag = c(500))
  # return correct grid
  if(x == 'RF'){return(tuneGridRF)}
  if(x == 'GBM'){return(tuneGridGBM)}
  if(x == 'NN'){return(tuneGridNN)}
}

#----------------------------------------------------
# Function to calculate forecast combinations
#----------------------------------------------------
# Out of sample estimation
outSampleFforma = function(target,
                            Forecasts,            # function name for master data set
                            startDate,
                            Horizon){
  
  # some of this is legacy from previous function set ups in the project
  # alphabetically order columns
  Forecasts = Forecasts[,order(colnames(Forecasts))]
  
  # clean Forecast data 
  colnames(Forecasts) = str_remove(colnames(Forecasts), paste0(target,'.'))
  train = Forecasts %>% arrange(date)
  
  # matrix to store forecasts
  forecasts = vector(length = nrow(Forecasts))*NA 
  
  # set up initial data to use 
  startIndex = which(Forecasts$date == min(Forecasts$date[Forecasts$date >= as.Date(startDate)]))

  #iterate through time 
  for(iteration in startIndex:nrow(Forecasts)){
    print(Forecasts$date[iteration])
    
    # establish X and Y
    X = train %>% 
        arrange(date) %>%
        filter(date <= Forecasts$date[iteration]) %>% 
        select(-date)
    
    # prepare forecast for ensemble
    forecastList =  M4metalearning::forec_methods()
    forecastList = append(forecastList,'meanForecast')
    forecastList = append(forecastList, 'arForecast')
    # prepare data for processing
    rawData = list(list(x = ts(data = X, frequency = 12),
                    n = dim(X)[1],
                    type = 'Macro',
                    period = getFreq(),
                    h = Horizon + 1))     # calc_forecast functions breaks if H < 2
    # calculate individual forecasts
    rawData =  M4metalearning::calc_forecasts(rawData, forecastList, n.cores=12) 
    # calculate time series features
    rawData =  M4metalearning::THA_features(rawData, n.cores=10)
    # cast as classification problem
    classData = M4metalearning::create_feat_classif_problem(rawData)
    # fit model
    preds =  M4metalearning::predict_selection_ensemble(getFFORMA(), classData$data)
    # estimate ensemble forecast
    final =  M4metalearning::ensemble_forecast(preds, rawData)
    # store predictions
    forecasts[iteration] = final[[1]]$y_hat[Horizon]
    
  }

  # merge in date   
  results = data.frame(date = Forecasts$date, forecasts) %>% 
    select(date,forecasts)
  # correct months in forecast matrix
  month(results$date) = month(results$date) + Horizon
  
  # return data
  return(results)
}

#----------------------------------------------------
# Parallel wrapper functions 
#----------------------------------------------------
generateFformaForecasts = function(target, 
                                   Horizon, 
                                   startDate = '1975-03-01'){
  # import and prep data 
  masterData = createMacroData()[[target]]
  colnames(masterData)[colnames(masterData) != 'date'] = target
  masterData[[target]] = masterData[[target]] + 100                   # we have to add 100 becuase of bug in FFORMA
  
 # calculate forecasts
  Forecasts = outSampleFforma(target = target, 
                                Forecasts = masterData,
                                startDate = startDate,
                                Horizon = Horizon)
    
  # save forecasts
  filename = paste0('./Data/Forecasts/derivedForecasts_uncond_FFORMA_H',Horizon,'_',target,'_v',version,'.csv')
  print(filename)
  write.csv(Forecasts, file = filename, row.names = F)
  
}


#----------------------------------------------------
# Create model averaging weights (Parallel) 
#----------------------------------------------------
if(generateWeights == T){
  # which variables to estimate
  runWeekVar = F
  runMthVar = T
  runQtrVar = F
  
  # set up parallel backend
  doParallel::registerDoParallel(30)
  
  # Monthly Variables #
    # list of components
    Targets = c('emp','itp') 
    Horizons = c(1,6,12,24)
    Freq = 'Monthly'
    
    # generate all TS forecasts
    if(runMthVar == T){
      foreach(i = 1:length(Targets), .combine = rbind) %:%
          foreach(k = 1:length(Horizons), .combine = rbind) %do%
            generateFformaForecasts(target = Targets[i], 
                                        Horizon = Horizons[k])
    }
    
    # turn off parallel backend 
    doParallel::stopImplicitCluster()
}

print(Sys.time() - startTime )
