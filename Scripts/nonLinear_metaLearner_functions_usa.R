# File: nonLinear_metaLearner_functions_v0.R
# Author: Tyler Pike
# Section: MA-MFA
# Date: 7/30/2019
# Note(s):  Create forecast combinations for USA macro series

# required libraries
# dplyr, lubridate, forecasts, foreach, parallel

# required scripts
# nonLinear_data_functions

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
outSampleWeigths = function(target,
                            Forecasts,
                            Engine, 
                            startDate,
                            bootstrap = F,
                            Horizon){
  
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
      filter(date < Forecasts$date[iteration]) %>% 
      select(-date)
    variableList = colnames(X)
    
    # bootstrap if called for
    if(bootstrap == T){
      # create bootstraps 
      X.extend.list = lapply(X, forecast::bld.mbb.bootstrap, num = 3)
      # stack bootstraps ontop of each other by variable
      vectorizeList = function(X){return(c(t(reduce(X,cbind))))}
      X.extend.list = lapply(X.extend.list, vectorizeList)
      # combine variables into dataframe
      X = reduce(t(X.extend.list), cbind)
      colnames(X) = variableList
      X = data.frame(X)
    }
    
    # estimate models
    if(Engine == 'GBM' | Engine == 'RF' | Engine == 'NN'){
      model = train(Observed~.,
                    data = X,
                    method = getEngineMethod(Engine),
                    trControl = getControl(),
                    tuneGrid = getTuningGrid(Engine),
                    metric = 'RMSE',
                    na.action = na.omit)
      
    } else if(Engine == 'peLasso'){
      # stage 1, shrink to 0, 
      # y-f -> eLasso to select subset of regressors 
      x = as.matrix(select(X, -Observed))
      y = X$Observed - rowMeans(x)
      model = cv.glmnet(x,y, alpha = 1, intercept = F, parallel = T)
      covariates = colnames(x)[which(as.vector(coef(model, s = 'lambda.min')) != 0)-1]
      if(length(covariates) > 1){
        # stage 2, shrink to 1/k, 
        # y-f -> eRidge to shrink subset of regressors to uniform weights
        model = cv.glmnet(x[,covariates],y, alpha = 0, intercept = F)
      }else{
        covariates = colnames(x)
      }

    } else if(Engine == 'Lasso'){
      x = as.matrix(select(X, -Observed))
      y = X$Observed - rowMeans(x)
      model = cv.glmnet(x,y, alpha = 0, intercept = F, parallel = T)
      covariates = colnames(x)[which(as.vector(coef(model, s = 'lambda.min')) != 0)-1]
      
    } else if(Engine == 'FFORMA'){
      # prepare forecast for ensemble
      forecastList =  M4metalearning::forec_methods()
      forecastList = append(forecastList,'meanForecast')
      forecastList = append(forecastList, 'arForecast')
      # prepare data for processing
      rawData = list(list(x = ts(data = filter(train, date < Forecasts$date[iteration])$Observed, 
                                 frequency = 12),
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
      
    }
    
    # create ensemble forecast
    XX = train %>% 
      filter(date == Forecasts$date[iteration]) %>% 
      select(-date,-Observed) 
    
    # create predictions
    if(Engine == 'peLasso' |  Engine == 'Lasso'){
      forecasts[iteration] = predict(model, newx = as.matrix(XX[,covariates]), s = 'lambda.min')
    } else if(Engine == 'FFORMA'){
      forecasts[iteration] = final[[1]]$y_hat[Horizon]
    }else{
      forecasts[iteration] = predict(model,newdata = XX)
    }
   
  }
  
  # merge in date   
  results = data.frame(date = Forecasts$date, forecasts) %>% 
    select(date,forecasts)
  
  # return data
  return(results)
}

#----------------------------------------------------
# Parallel wrapper functions 
#----------------------------------------------------
generateForecastWeights = function(target, Engine, Horizon, 
                                   startDate = '1975-03-01', 
                                   repeats = 9,
                                   bootstrap = F){
  # import and prep data 
  forecastData = getMacroForecastsData(Horizon)[[target]] %>% 
                 distinct() %>% na.omit() %>%
                 mutate(Observed = Observed + 100)
   
 # calculate forecasts
  Forecasts = outSampleWeigths(target = target, 
                                Forecasts = forecastData,
                                Engine = Engine, 
                                bootstrap = bootstrap,
                                startDate = startDate,
                                Horizon = Horizon)
  
  # repeat forecast estimation if called for
  if(repeats != 0){
    for(rep in 1:repeats){
      Forecastss.uncon  = outSampleWeigths(target = target, 
                                        Forecasts = forecastData,
                                        Engine = Engine, 
                                        startDate = startDate,
                                        bootstrap = bootstrap)
      Forecasts = full_join(Forecasts, Forecastss.uncon, by = 'date')
      Forecasts = Forecasts[,!duplicated(colnames(Forecasts))]
      
    }
  }
  
  # save forecasts
  filename = paste0('./Data/Forecasts/US/version_',version,'/derivedForecasts_uncond_',Engine,'_H',Horizon,'_',target,'_v',version,'.csv')
  print(filename)
  write.csv(Forecasts, file = filename, row.names = F)
  
}


#----------------------------------------------------
# Create model averaging weights (Parallel) 
#----------------------------------------------------
if(generateWeights == T){
  # which variables to estimate
  runWeekVar = F
  runMthVar = F
  runQtrVar = F
  
  # set up parallel backend
  doParallel::registerDoParallel(30)
  
  # common parameters
  Engines = c('NN','RF' ,'GBM','peLasso','Lasso') 
  
  # Quarterly Variables #
    # list of components
    Targets = c('p','rgdp') 
    Horizons = c(1,2,4,8)
    Freq = 'Quarterly'

    # generate all TS forecasts
    if(runQtrVar == T){
      foreach(i = 1:length(Targets), .combine = rbind) %:%
        foreach(j = 1:length(Engines), .combine = rbind) %:%
          foreach(k = 1:length(Horizons), .combine = rbind) %dopar%
             generateForecastWeights(target = Targets[i], Engine = Engines[j],
                                     Horizon = Horizons[k], repeats = 99)
    }
    
  # Monthly Variables #
    # list of components
    Targets = c('itp','emp') 
    Horizons = c(1,6,12,24
    Freq = 'Monthly'
    
    # generate all TS forecasts
    if(runMthVar == T){
      foreach(i = 1:length(Targets), .combine = rbind) %:%
        foreach(j = 1:length(Engines), .combine = rbind) %:%
          foreach(k = 1:length(Horizons), .combine = rbind) %dopar%
              generateForecastWeights(target = Targets[i], Engine = Engines[j],
                                      Horizon = Horizons[k], repeats = 99)
    }
    
  # Weekly Variables #
    Targets = c('treas','oil')
    Horizons = c(1,4,8,12)
    Freq = 'weekly'

    # generate all TS forecasts
    if(runWeekVar == T){
      foreach(i = 1:length(Targets), .combine = rbind) %:%
        foreach(j = 1:length(Engines), .combine = rbind) %:%
          foreach(k = 1:length(Horizons), .combine = rbind) %dopar%
            generateForecastWeights(target = Targets[i], Engine = Engines[j],
                                    Horizon = Horizons[k], freq = 'weekly', repeats = 99)
    }

  
    # turn off parallel backend 
    doParallel::stopImplicitCluster()
}

print(Sys.time() - startTime )
