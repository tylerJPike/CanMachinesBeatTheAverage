# File: nonLinear_metaLearner_functions_v0.R
# Author: Tyler Pike
# Date: 10/1/2019
# Note(s): Create US forecasting appendix

# required libraries
# dplyr, lubridate, quantmod

# required scripts  
# nonLinear_data_functions

getRecDates = function(){
  quantmod::getSymbols.FRED(c('USREC'), env = globalenv())
  recession_m = data.frame(USREC, date = index(USREC)) %>% 
    filter(USREC >= 1)
  return(recession_m$date)
}

#----------------------------------------------------
# Function to Analyze Forecasts
#----------------------------------------------------
# accuracy metrics
MAE = function(X, obs = Forecasts$Observed){return(mean(abs((X-obs)), na.rm = T))}
RMSE = function(X, obs = Forecasts$Observed){return(sqrt(mean((X-obs)^2, na.rm = T)))}


# create matrix of scores
# requires an 'Observed' column in forecast data frame
'%!in%' <- function(x,y)!('%in%'(x,y))
recDates = getRecDates()
evaluateForecasts = function(Forecasts, metric, includeRec = T){
  if(includeRec == F){
    Forecasts = Forecasts %>% filter(date %!in% recDates)
  }
  if(metric == 'mae'){
    return(
      scores.mae = apply(select(Forecasts,-date, -Observed), MARGIN = 2, FUN = MAE, obs = Forecasts$Observed)
    )
  }else if(metric == 'rmse'){
    return(
      scores.rmse = apply(select(Forecasts,-date, -Observed), MARGIN = 2, FUN = RMSE, obs = Forecasts$Observed)
    )
  }
}

# White's reality test
whiteRealityTest = function(alternativeErrors, nullErrors, N = 100){
  # pairwise match errors in dataframe for bootstrapping
  f = nullErrors^2 - alternativeErrors^2
  # time series bootstrap
  bootseries = forecast::bld.mbb.bootstrap(errorsDelta, num = N, block_size = 12)
  bootseries = reduce(bootseries, rbind)
  # bootstrapped mean
  f_star = mean(bootseries)
  # create order statistic
  V = f[order(f)]
  # find percentile m equal to or one element before mean(f)
  M = which(V == max(V[V <= 0]))
  # calc p-value
  p = 1-(M/length(V))
  return(p)
}


#----------------------------------------------------
# Function to create recent best forecasts
#----------------------------------------------------
recentNBest = function(forecasts, window = NA, nMax){
  # calculate rolling forecast errors
  errors = abs(select(arrange(forecasts,date), -date, -Observed) - forecasts$Observed)
  rollRMSE = function(X){return(sqrt(mean((X)^2, na.rm = T)))}
  rollingErrors = zoo::rollapply(data = errors, width = seq_along(errors[,1]), FUN = rollRMSE, align = 'right', fill = NA)
  rm(rollRMSE)
  
  # create rolling N-best forecasts 
  X = select(forecasts, -date, -Observed) %>% as.matrix()
  nBest = matrix(nrow = nrow(X), ncol = nMax)
  for(row in 1:nrow(X)){
    for(column in 1:nMax){
      if(column == 4){
        #print(forecasts$date[row])
        #print(colnames(X[,order(rollingErrors[row,])[1:column]]))
      }
      nBest[row,column] = mean(X[row,order(rollingErrors[row,])[1:column]])
    }
  }
  
  colnames(nBest) = paste0('N',c(1:nMax))
  nBest = data.frame(date = forecasts$date, nBest)
  
  # # choose best rolling forecast
  # test = abs(select(nBest, -date) - forecasts$Observed)
  # colMeans(test, na.rm = T)
  
  return(nBest)
}

#----------------------------------------------------
# Function to Get and Generate Forecast Data 
#----------------------------------------------------
getForecastData = function(Target, Horizon, Version){
  
  # pull in raw forecasts 
  TS = getMacroForecastsData(Horizon)[[Target]] 
  
  # create simple forecast combinations (niave forecasts (NF))
  NF = TS %>% 
    mutate(mean = apply(select(TS, -date, -Observed), MARGIN = 1, mean, na.rm = T)) %>%
    na.omit()
  
  Forecasts =  NF %>%
    arrange(date) %>% na.omit() %>%
    filter(date >= as.Date('1980-01-01'))

  return(Forecasts)
}


#----------------------------------------------------
# Evaluate  forecasts with AR Ratios
#----------------------------------------------------
# Monthly
freq = 'monthly'
Targets = c('emp','itp') 
Horizons = c(1,6,12,24)
metric = 'rmse'

forecastPerformance = matrix(ncol = 1, nrow = 12)
for(Target in Targets){
  for(Horizon in Horizons){
    forecastData = getForecastData(Version = version,
                                    Target = Target,
                                    Horizon = Horizon)
    temp = evaluateForecasts(forecastData, metric) %>% as.matrix()
    colnames(temp) = paste0(Target,'_H',Horizon,'_',metric)
    forecastPerformance = cbind(forecastPerformance, temp)
  }
}
forecastPerformance = forecastPerformance[,-1]

# adjust to be ratio with AR
forecastPerformance = t(t(forecastPerformance) / forecastPerformance[c('mean'),])

# write the latex table
table.path = paste0('./Evaluation/UsaComparison/forecastPerformance_timeSeriesAppendix_',freq,'_',metric,'_v',version,'.tex')
print(xtable::xtable(forecastPerformance, type = "latex"), file = table.path, row.names = T)
