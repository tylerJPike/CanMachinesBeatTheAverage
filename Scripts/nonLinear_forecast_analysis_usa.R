# File: nonLinear_forecast_analysis_usa.R
# Author: Tyler Pike
# Date: 10/1/2019
# Note(s): Create USA forecast chart and results comparison table

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
  # pull in forecast combinations via ML
  # Random Forest
  RF = getMacroForecastsComboData(Target,Horizon, Engine = 'RF',Version)
  RF$rf = rowMeans(select(RF, -date))  
  RF = select(RF, date, rf) 
  # Boosted Tree
  GBM = getMacroForecastsComboData(Target,Horizon, Engine = 'GBM',Version) 
  GBM$gbm = rowMeans(select(GBM, -date))  
  GBM = select(GBM, date, gbm)
  # Neural Network
  NN = getMacroForecastsComboData(Target,Horizon, Engine = 'NN',Version) 
  NN$nn = rowMeans(select(NN, -date))  
  NN = select(NN, date, nn)
  # peLasso
  PL = getMacroForecastsComboData(Target,Horizon, Engine = 'peLasso',Version) 
  PL$pl = rowMeans(select(PL, -date))  
  PL = select(PL, date, pl)
  # lasso
  LA = getMacroForecastsComboData(Target,Horizon, Engine = 'Lasso',Version) 
  LA$lasso = rowMeans(select(LA, -date))  
  LA = select(LA, date, lasso)
  # FFOMRA
  FF = getMacroForecastsComboData(Target,Horizon, Engine = 'FFORMA',Version) 
  FF = select(FF, date, fforma = weights) %>% mutate(fforma = fforma - 100)
  
  # pull in raw forecasts 
  TS = getMacroForecastsData(Horizon)[[Target]] 
  
  # create N-Best forecasts (baed on five years)
  if(Target == 'oil' | Target == 'treas'){
    NBEST = recentNBest(TS, nMax = 4)
  }else if(Target == 'emp' | Target == 'itp'){
    NBEST = recentNBest(TS, window = 60, nMax = 4)
  }else if(Target == 'rgdp' | Target == 'q'){
    NBEST = recentNBest(TS, window = 20, nMax = 4)
  }
 
  # create simple forecast combinations (niave forecasts (NF))
  NF = TS %>% 
    mutate(mean = apply(select(TS, -date, -Observed), MARGIN = 1, mean, na.rm = T),
           median = apply(select(TS, -date, -Observed), MARGIN = 1, median, na.rm = T)) %>%
    select(date, mean, median) %>%
    na.omit()
  
  # merge forecasts 
  Forecasts =  
    purrr::reduce(list(RF, NN, GBM, LA, FF, TS, NF, PL, NBEST), full_join, by = 'date') %>%
    arrange(date) %>% na.omit() %>%
    filter(date >= as.Date('1980-01-01'))

  return(Forecasts)
}

#----------------------------------------------------
# Evaluate robustness of forecast error differences 
#----------------------------------------------------
# option to include recession
includeRec = T

# what forecast to compare ML with
baseline = 'mean'

# what horizon to evaluate
Horizons = c(1,6,12,24)

# Y , H , Model , relative MSE , DM , CW , White  
evaluationTable = matrix(nrow = 88, ncol = 5)
colnames(evaluationTable) = c('Horizon','Target','Model','Relative MSE','DM (alt.median)')

# target series 
Targets = c('emp','itp')

count = 0
for(Horizon in Horizons){
  for(Target in Targets){
    
    # prepare forecast combination data
    forecastData = getForecastData(Version = version,
                                   Target = Target,
                                   Horizon = Horizon) %>%
                    mutate(rf = rf - Observed, 
                           gbm = gbm - Observed, 
                           nn = nn - Observed,
                           median = median - Observed,
                           mean = mean - Observed,
                           lasso = lasso - Observed,
                           pl = pl - Observed,
                           N1 = N1 - Observed,
                           N2 = N2 - Observed,
                           N3 = N3 - Observed,
                           N4 = N4 - Observed) %>% 
                    select(-Observed, -contains('ipt'),-contains('emp')) %>% 
                    na.omit()
    
    # remove recessions if desired
    if(includeRec == T){
      forecastData = forecastData %>% select(-date)
    }else{
      forecastData = forecastData %>% filter(date %!in% recDates) %>% select(-date)
    }
    
    # record results
    for(comparison in names(select(forecastData, -baseline))){
      count = count + 1
      evaluationTable[count, 1] = Horizon
      evaluationTable[count, 2] = Target
      evaluationTable[count, 3] = comparison
      evaluationTable[count, 4] = round(mean((forecastData[[comparison]])^2)/mean((forecastData[[baseline]])^2),2)
      evaluationTable[count, 5] = round(dm.test(forecastData[[baseline]],
                                                forecastData[[comparison]],
                                                alternative = 'greater',
                                                h = 4,
                                                power = 2)$p.value, 
                                        2)
     
    }
  } 
}

# write the latex table
table.path = paste0('./Evaluation/UsaComparison/ForecastTables/forecastPerformance_',freq,'_',metric,'_exRec_v',version,'.tex')
print(xtable::xtable(evaluationTable, type = "latex"), file = table.path, row.names = T)

#----------------------------------------------------
# Chart forecasts and observed data
#----------------------------------------------------
combinations = c('mean', 'median', 'lasso', 'pl', 'N1', 'N2', 'N3', 'N4', 'rf', 'gbm','fforma')
range = c('minRow','maxRow')

forecastData.emp = getForecastData(Version = version,
                                   Horizon = 12,
                                   Target = 'emp') 
              
forecastData.itp = getForecastData(Version = version,
                                   Horizon = 12,
                                   Target = 'itp') 

forecastData.emp$minRow = apply(select(forecastData.emp, combinations), 1, FUN = min, na.rm = T)
forecastData.emp$maxRow = apply(select(forecastData.emp, combinations), 1, FUN = max, na.rm = T)
forecastData.itp$minRow = apply(select(forecastData.itp, combinations), 1, FUN = min, na.rm = T)
forecastData.itp$maxRow = apply(select(forecastData.itp, combinations), 1, FUN = max, na.rm = T)


pdf('./Evaluation/UsaComparison/ForecastCharts/usaForecastChart_shaded.pdf', width = 8.5)
# employment
par(fig = c(0,1,.5,1), mar = c(2,2,2,2))
chart = rplot.line(select(forecastData.emp, Observed, date),
                   Title = 'Payroll Employment Growth', 
                   Y2lab = 'Percent',
                   Y1lab = 'Monthly',
                   Enddatelab = F,
                   Zeroline = T,
                   Lty = c(1, 1, rep(1, 11)),
                   Lwd = 1,
                   Y2lim = c(-15,15),
                   Y2int = 2,
                   Xlim = c(1978, 2021),
                   legend = T,
                   legend.x.loc = 2005,
                   legend.text = c('Observed','Range of forecast combinations'),
                   legend.text.col = 'black',
                   legend.col = c('black','gray'),
                   legend.lwd = c(2,10))
polygon(x = c(decimal_date(forecastData.emp$date), rev(decimal_date(forecastData.emp$date))),
        y = c(forecastData.emp$maxRow, rev(forecastData.emp$minRow)),
        density = 100, col = 'gray')
lines(decimal_date(forecastData.emp$date), forecastData.emp$Observed,
      type = 'l', col = 'black', lwd = 1)
abline(h=0)


# IP chart
par(fig = c(0,1,0,.5), new = T)
chart = rplot.line(select(forecastData.itp, date, Observed),               
                   Title = 'Industrial Produciton ', 
                   Y2lab = 'Percent',
                   Y1lab = 'Monthly',
                   Enddatelab = F,
                   Zeroline = T,
                   Lty = c(1, 1, rep(1, 10)),
                   Lwd = c(1,1, rep(1,10)),
                   Y2lim = c(-40,30),
                   Xlim = c(1978, 2021),
                   legend = F,
                   legend.text = c('Observed','Range of forecast combinations'),
                   legend.text.col = 'black',
                   legend.col = c('black','gray'),
                   legend.lwd = c(2,10))
polygon(x = c(decimal_date(forecastData.itp$date), rev(decimal_date(forecastData.itp$date))),
        y = c(forecastData.itp$maxRow, rev(forecastData.itp$minRow)),
        density = 100, col = 'gray')
lines(decimal_date(forecastData.itp$date), forecastData.itp$Observed,
      type = 'l', col = 'black', lwd = 1)
abline(h=0)

dev.off()




