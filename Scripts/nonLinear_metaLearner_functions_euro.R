# File: nonLinear_metaLearner_functions_v0.R
# Author: Tyler Pike
# Date: 7/30/2019
# Note(s):  Create forecast combinations for Eurozone macro series

# required libraries
# dplyr, lubridate, forecasts, foreach, parallel

# required scripts
# nonLinear_data_functions

startTime = Sys.time()

generateWeights = T

#----------------------------------------------------
# Machine learning helper functions
#----------------------------------------------------
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
                            bootstrap = F){
  
  # alphabetically order columns
  Forecasts = Forecasts[,order(colnames(Forecasts))]
  
  # clean Forecast data 
  colnames(Forecasts) = str_remove(colnames(Forecasts), paste0(target,'.'))
  train = Forecasts
  
  colnames(train)[-which(colnames(train) == 'Observed' |  colnames(train) == 'date')] <- 
    paste0('F.',colnames(train)[-which(colnames(train) == 'Observed' |  colnames(train) == 'date')])
  
  # matrix to store forecasts
  forecasts = vector(length = nrow(Forecasts))*NA 
  
  # set up initial data to use 
  startIndex = which(Forecasts$date == min(Forecasts$date[Forecasts$date >= as.Date(startDate)]))

  #iterate through time 
  for(iteration in startIndex:nrow(Forecasts)){
    print(Forecasts$date[iteration])
    
    # establish X and Y
    XX = train %>% 
          arrange(date) 
    Date = XX$date
    
    # remove forecasts with fewer than 5 responses and impute missing X with average response
    XX = XX[, apply(select(XX,-date), 2, function(x) sum(!is.na(x)) > 4 )]
    XX = apply(select(XX,-date), 2, function(x) ifelse(is.na(x), mean(x, na.rm = T), x))
    XX = data.frame(XX)
    XX$date = Date
    
    # training X and Y
    X = XX %>% 
         arrange(date) %>%
         filter(date < Forecasts$date[iteration]) %>%
         select(-date)
    variableList = colnames(X)
  
    # create current data
    XX = XX %>% 
      filter(date == Forecasts$date[iteration]) %>% 
      select(-date,-Observed) 
    
    # bootstrap data if called for
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
    
    # estimate combination models
    if(Engine == 'GBM' | Engine == 'RF' | Engine == 'NN'){
       model = train(Observed~.,
                      data = X,
                      method = getEngineMethod(Engine),
                      trControl = getControl(),
                      tuneGrid = getTuningGrid(Engine),
                      metric = 'RMSE',
                      na.action = na.omit)
    }else if(Engine == 'peLasso'){
      # stage 1, shrink to 0, 
      # y-f -> eLasso to select subset of regressors 
      x = as.matrix(select(X, -Observed))
      y = X$Observed - rowMeans(x)
      model = cv.glmnet(x,y, alpha = 1, intercept = F, parallel = T, nfolds = 5)
      covariates = colnames(x)[which(as.vector(coef(model, s = 'lambda.min')) != 0)-1]
      if(length(covariates) > 1){
        # stage 2, shrink to 1/k, 
        # y-f -> eRidge to shrink subset of regressors to uniform weights
        model = cv.glmnet(x[,covariates],y, alpha = 0, intercept = F, nfolds = 5)
      }else{
        covariates = colnames(x)
      }
    } else if(Engine == 'Lasso'){
      x = as.matrix(select(X, -Observed))
      y = X$Observed - rowMeans(x)
      model = cv.glmnet(x,y, alpha = 0, intercept = F, parallel = T)
      model.coef = coef(model, s = "lambda.min")[-1] %>% Matrix()
    }
      
    # create predictions
    if(Engine == 'peLasso'){
      forecasts[iteration] = predict(model, newx = as.matrix(XX[,covariates]))
    }else if(Engine == 'Lasso'){
      #forecasts[iteration] = as.matrix(XX) %*% as.matrix(model.coef/sum(model.coef))
      forecasts[iteration] = predict(model, newx = as.matrix(XX), s = 'lambda.min')
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
                                   startDate = '2005-01-01', 
                                   repeats = 9,
                                   bootstrap = F){
  
  # import and prep forecast data 
  forecastData = createEuroSPFData()[[target]] 
  # import and prep observed data
  observedData = createEuroMacroData()[[target]] %>% 
                  distinct() %>% 
                  na.omit() %>% 
                  filter(date >= as.Date('1999-01-01')) %>%
                  rename(Observed = tolower(target))
  # merge data 
  forecastData = inner_join(forecastData, observedData, by = 'date')
  
  # calculate forecasts
  Forecasts = outSampleWeigths(target = target, 
                                Forecasts = forecastData,
                                Engine = Engine, 
                                bootstrap = bootstrap,
                                startDate = startDate)
  
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
  filename = paste0('./Data/Forecasts/Euro/derivedForecasts_uncond_',Engine,'_',target,'_v',version,'.csv')
  print(filename)
  write.csv(Forecasts, file =filename, row.names = F)
  
}


#----------------------------------------------------
# Create model averaging weights (Parallel) 
#----------------------------------------------------
if(generateWeights == T){
  
  # set up parallel backend 
  n = parallel::detectCores()
  cl =  parallel::makeCluster(n-2)
  doParallel::registerDoParallel(cl)
  
  # package dependencies
  libraries = 
    c('forecast', 'caret', 'glmnet', 'pROC','tsfeatures','readxl','tidyverse','lubridate','doParallel', 'foreach')
  
  # parameters
  Engines = c('RF' ,'GBM','Lasso','peLasso')
  Targets = c('HICP') #'URATE','GDP'

  # estimate forecast combinations
  foreach(i = 1:length(Targets), .combine = rbind, .packages = libraries) %:%
    foreach(j = 1:length(Engines), .combine = rbind, .packages = libraries) %dopar%
       generateForecastWeights(target = Targets[i], Engine = Engines[j], repeats = 9)

  # turn off parallel backend 
  doParallel::stopImplicitCluster()
}

print(Sys.time() - startTime )
