# File: nonLinear_timeSeries_forecasts_v0.R
# Author: Tyler Pike
# Section: MA-MFA
# Date: 7/30/2019
# Note(s): Create time series forecasts for US macro series

# required libraries
# dplyr, lubridate, forecasts, foreach, parallel

# required scripts
# nonLinear_data_functions

generateForecasts = T
#----------------------------------------------------
# Function to calculate oos forecasts
#----------------------------------------------------
# function that takes in a data frame with a 'date' column and a column to be forecasted; 
#   returns a dataframe with one date column and ten different forecast values for the target 
outSampleForecast = function(masterData,
                             target,
                             startDate,
                             freq,          # monthly or quarterly
                             H){            # given in periods ahead 
  
  # set Y
  #if(is.numeric(target)){target = colnames(masterData)[target]}
  masterData = masterData %>% rename(target = target) %>% select(target, date)
  
  # create Forecast matrix
  Forecasts = matrix(ncol = 11, nrow = nrow(masterData))
  models = c('mean','naive','dnaive','snaive','ets','theta','tbats','stlm','arima', 'nnet','ar')
  models = paste(target,models,sep = '.')
  colnames(Forecasts) = models
  
  # set up initial data to use 
  startDate = as.Date(startDate)
  startIndex = which(masterData$date == min(filter(masterData,date >= as.Date(startDate))$date))
  
  # catch any series with NA's 
  if(any(is.na(masterData %>% filter(date <= startDate) %>% select(target)))){
    return(Forecasts)
  }
  
  for(iteration in startIndex:nrow(masterData)){
    print(masterData$date[iteration])
    # set time series
    Y = ts(masterData$target[1:iteration],
           start = decimal_date(masterData$date[1]),
           frequency = 4)
    
    # create benchmark model 
    if(freq == 'quarterly'){
      model.ar = arima(Y, order = c(4,0,0), method = 'ML') 
    }else if(freq == 'monthly'){
      model.ar = arima(Y, order = c(12,0,0), method = 'ML') 
    }else if(freq == 'weekly'){
      model.ar = arima(Y, order = c(7,0,0), method = 'ML') 
    }
    
    # create stable of forecasting models
    model.mean   = meanf(Y, h = H)               # sample mean
    model.naive  = rwf(Y, h = H)                 # random walk
    model.dnaive = rwf(Y, h = H, drift = T)      # random walk with drift
    model.snaive = snaive(Y, h = H)              # seasonally adjusted random walk
    model.ets    = ets(Y)                        # simple exponential smoothing
    model.theta  = thetaf(Y, h = H)              # theta method (ses with drift)
    model.tbats  = tbats(Y)                      # TBATS
    model.stlm   = stlm(Y,
                        modelfunction = stats::ar)  # STLM
    model.arima  = auto.arima(Y)                 # ARIMA
    model.nnet   = nnetar(Y)                     # autoregressive nueral network
    
    # create forecasts 
    Forecasts[iteration,1]   = forecast(model.mean, h = H)$mean[H]
    Forecasts[iteration,2]   = forecast(model.naive, h = H)$mean[H]
    Forecasts[iteration,3]  = forecast(model.dnaive, h = H)$mean[H]
    Forecasts[iteration,4]  = forecast(model.snaive, h = H)$mean[H]
    Forecasts[iteration,5]     = forecast(model.ets, h = H)$mean[H]
    Forecasts[iteration,6]   = forecast(model.theta, h = H)$mean[H]
    Forecasts[iteration,7]   = forecast(model.tbats, h = H)$mean[H]
    Forecasts[iteration,8]   =  forecast(model.stlm, h = H)$mean[H]
    Forecasts[iteration,9]   = forecast(model.arima, h = H)$mean[H]
    Forecasts[iteration,10]   = forecast(model.nnet, h = H)$mean[H]
    Forecasts[iteration,11]  = forecast(model.ar, h = H)$mean[H]
    
  }
  
  # merge in date   
  Forecasts = data.frame(date = masterData$date, Forecasts) 
  if(freq == 'quarterly'){
    month(Forecasts$date) = month(Forecasts$date) + H*3
  }else if(freq == 'monthly'){
    month(Forecasts$date) = month(Forecasts$date) + H
  }else if(freq == 'weekly'){
    week(Forecasts$date) = week(Forecasts$date) + H
  }
  
  Forecasts = full_join(Forecasts, masterData, by = 'date') %>% rename(Observed = target)
  
  # return data
  return(Forecasts)
}

#----------------------------------------------------
# Parallel wrapper functions 
#----------------------------------------------------
generateTSForecats = function(target, Horizon, frequency, startDate = '1970-03-01'){
  # call data
  masterData = createMacroData()[[target]]
  colnames(masterData)[colnames(masterData) != 'date'] = target
  # take annualized MoM % difference of industrial production 
  #if(target == 'ipt'){masterData = masterData %>% mutate(ipt = 12*((ipt-lag(ipt))/lag(ipt)))}
  # generate forecasts 
  forecasts = outSampleForecast(masterData,target, H = Horizon, freq = frequency, startDate = startDate)
  # save forecasts
  filename = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_',target,'_H',Horizon,'.csv')
  write.csv(forecasts, file = filename, row.names = F)
  # return forecasts
  return(c(1))
}

#----------------------------------------------------
# Parallel loop to calculate oos forecasts
#----------------------------------------------------
if(generateForecasts == T){
  # set up parallel backend 
  registerDoParallel(25)

  # # generate all quarterly TS forecasts
   Targets = c('p', 'rgdp')
   Horizons = c(1,2,4,8)
   foreach(i = 1:length(Targets), .combine = rbind) %:%
     foreach(j = 1:length(Horizons), .combine = rbind) %dopar%
       generateTSForecats(target = Targets[i], Horizon = Horizons[j], freq = 'quarterly')
   
  # # generate all monthly TS forecasts
   Targets = c('ipt','emp')
   Horizons = c(1,6,12,24)
   foreach(i = 1:length(Targets), .combine = rbind) %:%
     foreach(j = 1:length(Horizons), .combine = rbind) %dopar%
       generateTSForecats(target = Targets[i], Horizon = Horizons[j], freq = 'monthly')
   
  # generate all weekly TS forecasts
   Targets = c('treas','oil')
   Horizons = c(1,4,8,12)
   foreach(i = 1:length(Targets), .combine = rbind) %:%
     foreach(j = 1:length(Horizons), .combine = rbind) %dopar%
     generateTSForecats(target = Targets[i], Horizon = Horizons[j], freq = 'weekly')

  # turn off parallel backend 
  stopImplicitCluster()
}
