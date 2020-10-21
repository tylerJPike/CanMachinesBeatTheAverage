# File: nonLinear_forecast_analyis_euro
# Author: Tyler Pike
# Section: MA-MFA
# Date: 10/1/2019
# Note(s): Create Euro forecast chart and results comparison table

# required libraries
# dplyr, lubridate, quantmod

# required scripts  
# nonLinear_data_functions

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

#----------------------------------------------------
# Function to create recent best forecasts
#----------------------------------------------------
recentNBest = function(forecasts, nMax){
  
  rollRMSE = function(X){return(sqrt(mean((X)^2, na.rm = T)))}
  
  # create rolling N-best forecasts 
  X = select(forecasts, -date, -Observed) %>% as.matrix()
  nBest = matrix(nrow = nrow(X), ncol = nMax)
  for(row in 5:nrow(X)){
    # drop columns with fewer than five observations up to this point
    XX = X[,apply(X[1:row,], 2, function(x) sum(!is.na(x)) > 4 )]
    # convert missing observations to mean forecast
    XX = apply(XX, 2, function(x) ifelse(is.na(x), mean(x[1:row], na.rm = T), x))
    XX = data.frame(XX)
    # calculate rolling forecast errors
    errors = abs(arrange(XX) - forecasts$Observed)[1:row,]
    rollingErrors = zoo::rollapply(data = errors, width = seq_along(errors[,1]), FUN = rollRMSE, align = 'right', fill = NA)
    # select n-best
    if(ncol(XX) < nMax){
      nmax = ncol(XX)
    }else{
      nmax = nMax
    }
    for(column in 1:nmax){
      nBest[row,column] = mean(as.numeric(XX[row,order(rollingErrors[row,])[1:column]]))
    }
  }
  
  colnames(nBest) = paste0('N',c(1:nMax))
  nBest = data.frame(date = forecasts$date, nBest)

  return(nBest)
}

#----------------------------------------------------
# Function to Get and Generate Forecast Data 
#----------------------------------------------------
getForecastData = function(Target, Version){
  # import and prep observed data
  observedData = createEuroMacroData()[[Target]] %>% 
    distinct() %>% 
    na.omit() %>% 
    filter(date >= as.Date('1999-01-01')) %>%
    rename(Observed = tolower(Target))
  
  # pull in forecast combinations per ML
  # random forest
  RF = getEuroComboData(Target, Engine = 'RF',Version)
  RF$rf = rowMeans(select(RF, -date))  
  RF = select(RF, date, rf) 
  # gradient boosting machine
  GBM = getEuroComboData(Target, Engine = 'GBM',Version) 
  GBM$gbm = rowMeans(select(GBM, -date))  
  GBM = select(GBM, date, gbm)
  # PE lasso
  PL = getEuroComboData(Target, Engine = 'peLasso',Version) 
  PL$pl = rowMeans(select(PL, -date))  
  PL = select(PL, date, pl)
  # lasso
  LA = getEuroComboData(Target, Engine = 'Lasso',Version) 
  LA$lasso = rowMeans(select(LA, -date))  
  LA = select(LA, date, lasso)
  
  # pull in raw forecasts 
  TS = createEuroSPFData()[[Target]] 
  TS = inner_join(observedData,TS, by = 'date')
  
  # create N-Best forecasts
  NBEST = recentNBest(TS, nMax = 4)
  
  # create simple forecast combinations (niave forecasts (NF))
  NF = TS %>% 
       mutate(mean = apply(select(TS, -date, -Observed), MARGIN = 1, mean, na.rm = T),
              median = apply(select(TS, -date, -Observed), MARGIN = 1, median, na.rm = T)) %>%
       select(date, mean, median) %>%
       na.omit()
  
  # merge data 
  Forecasts = data.frame(date = RF$date, rf = RF$rf, gbm = GBM$gbm) %>%
    full_join(observedData, by = 'date') %>%
    full_join(NF, by = 'date') %>%
    full_join(LA, by = 'date') %>%
    full_join(PL, by = 'date') %>%
    full_join(NBEST, by = 'date') %>%
    arrange(date) 
   # na.omit() %>%
  #  filter(date >= as.Date('1980-01-01'))

  return(Forecasts)
}


#----------------------------------------------------
# Evaluate  forecasts with AR Ratios
#----------------------------------------------------
# metric to use 
metrics = c('rmse','mae')
# Target series
Targets = c('GDP','URATE','HICP')

for(metric in metrics){

  forecastPerformance = matrix(ncol = 1, nrow = 10)
  
  for(Target in Targets){
    forecastData = getForecastData(Version = version,
                                   Target = Target)
    temp = evaluateForecasts(forecastData, metric) %>% as.matrix()
    colnames(temp) = paste0(Target,'_',metric)
    forecastPerformance = cbind(forecastPerformance, temp)
  }
  forecastPerformance = forecastPerformance[,-1]

  # adjust to be ratio with mean
  forecastPerformance = t(t(forecastPerformance) / forecastPerformance[3,])

  # write the latex table
  table.path = paste0('./Evaluation/EuroComparison/forecastPerformance_',metric,'_v',version,'.tex')
  print(xtable::xtable(forecastPerformance, type = "latex"), file = table.path, row.names = T)
  
}

#----------------------------------------------------
# Evaluate robustness of forecast error differences 
#----------------------------------------------------
# option to include recession
includeRec = T

# what forecast to compare ML with
baseline = 'mean'

# Y , H , Model , relative MSE , DM , CW , White  
evaluationTable = matrix(nrow = 27, ncol = 5)
colnames(evaluationTable) = c('Target','Model','Relative MSE','DM (alt.median)','DM (alt.ml)')

# target series 
Targets = c('GDP','URATE','HICP')

count = 0
for(Target in Targets){

    # prepare forecast combination data
    forecastData = getForecastData(Version = version,
                                   Target = Target) %>%
                    mutate(rf = rf - Observed, 
                           gbm = gbm - Observed, 
                           median = median - Observed,
                           mean = mean - Observed,
                           lasso = lasso - Observed,
                           pl = pl - Observed,
                           N1 = N1 - Observed,
                           N2 = N2 - Observed,
                           N3 = N3 - Observed,
                           N4 = N4 - Observed) %>% 
                    select(-Observed) %>% na.omit()
    
    # remove recessions if desired
    if(includeRec == T){
      forecastData = forecastData %>% select(-date)
    }else{
      forecastData = forecastData %>% filter(date %!in% recDates) %>% select(-date)
    }
    
    # record results
    for(comparison in names(select(forecastData, -baseline))){
      count = count + 1
      evaluationTable[count, 1] = Target
      evaluationTable[count, 2] = comparison
      evaluationTable[count, 3] = round(mean((forecastData[[comparison]])^2)/mean((forecastData[[baseline]])^2),2)
      evaluationTable[count, 4] = round(dm.test(forecastData[[baseline]],
                                                forecastData[[comparison]],
                                                alternative = 'greater',
                                                h = 4,
                                                power = 2)$p.value, 
                                        2)
      evaluationTable[count, 5] = round(dm.test(forecastData[[comparison]],
                                                forecastData[[baseline]],
                                                alternative = 'greater',
                                                h = 4,
                                                power = 2)$p.value,
                                        2)
      
    }
}   

table.path = paste0('./Evaluation/EuroComparison/ForecastTables/forecastEvaluationTable_',baseline,'_v',version,'.tex')
print(xtable::xtable(evaluationTable, type = "latex"), file = table.path, row.names = F)


#----------------------------------------------------
# Chart forecasts and observed data
#----------------------------------------------------
forecastData.hicp = getForecastData(Version = version,
                                    Target = 'HICP')%>% 
                    filter(date >= as.Date('2005-01-01'))

forecastData.gdp = getForecastData(Version = version,
                                   Target = 'GDP') %>% 
                    filter(date >= as.Date('2005-01-01'))

# shaded chart
combinations = c('mean', 'median', 'lasso', 'pl', 'N1', 'N2', 'N3', 'N4', 'rf', 'gbm')
range = c('minRow','maxRow')
forecastData.gdp$minRow = apply(select(forecastData.gdp, combinations), 1, FUN = min, na.rm = T)
forecastData.gdp$maxRow = apply(select(forecastData.gdp, combinations), 1, FUN = max, na.rm = T)
forecastData.hicp$minRow = apply(select(forecastData.hicp, combinations), 1, FUN = min, na.rm = T)
forecastData.hicp$maxRow = apply(select(forecastData.hicp, combinations), 1, FUN = max, na.rm = T)


pdf('./Evaluation/EuroComparison/ForecastCharts/euroForecastChart_shaded.pdf', width = 8.5)
# real GDP growth chart 
par(fig = c(0,1,.5,1), mar = c(2,2,2,2))
chart = rplot.line(select(forecastData.gdp, Observed, date),                    
                   Title = 'Real GDP Growth', 
                   Y2lab = 'Percent',
                   Enddatelab = F,
                   Zeroline = T,
                   Lty = c(1, 1, rep(1, 10)),
                   Lwd = 1,
                   Y2lim = c(-7,10),
                   Y2int = 2,
                   Xlim = c(2004, 2021),
                   legend = T,
                   legend.x.loc = 2013,
                   legend.text = c('Observed','Range of forecast combinations'),
                   legend.text.col = 'black',
                   legend.col = c('black','gray'),
                   legend.lwd = c(2,10))
polygon(x = c(decimal_date(forecastData.gdp$date), rev(decimal_date(forecastData.gdp$date))),
        y = c(forecastData.gdp$maxRow, rev(forecastData.gdp$minRow)),
        density = 100, col = 'gray')
lines(decimal_date(forecastData.gdp$date), forecastData.gdp$Observed,
      type = 'l', col = 'black', lwd = 1)
abline(h = 0)

# HICP chart
par(fig = c(0,1,0,.5), new = T)
chart = rplot.line(select(forecastData.hicp, Observed, date),               
                   Title = 'HICP', 
                   Y2lab = 'Percent',
                   Enddatelab = F,
                   Xlim = c(2004, 2021),
                   Zeroline = T,
                   Lty = c(1, 1, rep(1, 10)),
                   Lwd = 1,
                   Y2lim = c(-2,4))
polygon(x = c(decimal_date(forecastData.hicp$date), rev(decimal_date(forecastData.hicp$date))),
        y = c(forecastData.hicp$maxRow, rev(forecastData.hicp$minRow)),
        density = 100, col = 'gray')
lines(decimal_date(forecastData.hicp$date), forecastData.hicp$Observed,
      type = 'l', col = 'black', lwd = 1)
abline(h = 0)

dev.off()
