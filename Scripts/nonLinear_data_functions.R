# File: nonLinear_data_functions_v0.R
# Author: Tyler Pike
# Date: 7/30/2019
# Note(s): Data getter and setter functions

#----------------------------------------------------
# Functions to download and prep data
# United States recession dates
#----------------------------------------------------
getRecDates = function(){
  quantmod::getSymbols.FRED(c('USREC'), env = globalenv())
  recession_m = data.frame(USREC, date = zoo::index(USREC)) %>% 
    filter(USREC >= 1)
  return(recession_m$date)
}

#----------------------------------------------------
# Functions to download and prep data
# United States Macro Series and Forecasts
#----------------------------------------------------
# --- create data to be forecasted --- #
createMacroData = function(){
  
  # real gross domestic product
  rgdp = read_xlsx(path = './Data/RealTimeMacro/routput_first_second_third.xlsx', sheet = 2, skip = 4) %>% 
    mutate(date = ymd(paste0(substr(Date,1,4),'-',as.numeric(substr(Date,7,7))*3,'-01'))) %>%
    select(date, First) %>% mutate() %>%
    mutate(First = as.numeric(First),
           First = if_else(is.na(First), 0, First))
  
  # industrial production
  ipt = read_xlsx(path = './Data/RealTimeMacro/ipt_first_second_third.xlsx', sheet = 2, skip = 4) %>%  
    mutate(date = ymd(paste0(Date,':01'))) %>%
    select(date, First)%>%
    mutate(First = as.numeric(First),
           First = if_else(is.na(First), 0, First))
  
  # annualized m/m % change in payroll employment
  emp = read_xlsx(path = './Data/RealTimeMacro/employ_pct_chg_first_second_third.xlsx', sheet = 2, skip = 4) %>%  
    mutate(date = ymd(paste0(Date,':01'))) %>%
    select(date, First)%>%
    mutate(First = as.numeric(First),
           First = if_else(is.na(First), 0, First))
  
  # gdp/gnp deflator (p)
  p = read_xlsx(path = './Data/RealTimeMacro/p_first_second_third.xlsx', sheet = 2, skip = 4) %>%  
    mutate(date = ymd(paste0(substr(Date,1,4),'-',as.numeric(substr(Date,7,7))*3,'-01'))) %>%
    select(date, First)%>%
    mutate(First = as.numeric(First),
           First = if_else(is.na(First), 0, First))
  
  # 10-year treasury rate 
  quantmod::getSymbols.FRED('WGS10YR', env = globalenv())
  treas = data.frame(First = WGS10YR, date = zoo::index(WGS10YR))

  # oil
  quantmod::getSymbols.FRED('WCOILWTICO', env = globalenv())
  oil = data.frame(First = WCOILWTICO, date = zoo::index(WCOILWTICO))

  # package and return 
  list(rgdp = rgdp,
       itp = ipt,
       emp = emp,
       p = p,
       treas = treas,
       oil = oil) %>% return()
  
}

# --- get forecasts --- #
getMacroForecastsData = function(Horizon){
  emp = ipt = p = rgdp = treas = oil = NULL 
  
  # weekly data  
  if(Horizon == 1 | Horizon == 4 | Horizon == 8 | Horizon == 12){
    treas = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_treas_H',Horizon,'.csv'), stringsAsFactors = F) %>%
      mutate(date = ymd(date))
    
    oil = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_oil_H',Horizon,'.csv'), stringsAsFactors = F) %>% 
      mutate(date = ymd(date)) 
  }
  # monthly data 
   if(Horizon == 1 | Horizon == 6 | Horizon == 12 | Horizon == 24){
      emp = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_emp_H',Horizon,'.csv'), stringsAsFactors = F) %>% 
             mutate(date = ymd(date)) 
      
      ipt = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_ipt_H',Horizon,'.csv'), stringsAsFactors = F) %>% 
        mutate(date = ymd(date)) 
   }
  # quarterly data  
   if(Horizon == 1 | Horizon == 2 | Horizon == 4 | Horizon == 8){
      rgdp = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_rgdp_H',Horizon,'.csv'), stringsAsFactors = F) %>% 
        mutate(date = ymd(date)) 
      
      p = read.csv(file = paste0('./Data/Forecasts/US/TimeSeries/timeSeriesForecast_p_H',Horizon,'.csv'), stringsAsFactors = F) %>% 
        mutate(date = mdy(date)) 
   }
  
  # package and return 
  list(rgdp = rgdp,
       itp = ipt,
       emp = emp,
       p = p,
       treas = treas,
       oil = oil) %>% return()
}


# --- get forecast combinations --- #
getMacroForecastsComboData = function(Target,Horizon,Engine,Version){
  fileName = paste0('./Data/Forecasts/US/version_',Version,'/derivedForecasts_','uncond','_',Engine,'_H',Horizon,'_',Target,'_v',Version,'.csv')
  Data = read.csv(file = fileName, stringsAsFactors = F) %>% 
          mutate(date = ymd(date)) 
  colnames(Data)[2] = 'weights'
  return(Data)
}

#----------------------------------------------------
# Functions to download and prep data
# European Survey of Professional Forecasters 
#----------------------------------------------------
# --- create data to be forecasted --- #
createEuroMacroData = function(){
  # import data
  # Harmonized Index of Consumer Prices: All Items for Euro area (19 countries)
  quantmod::getSymbols.FRED('CP0000EZ19M086NEST', env = globalenv())
  hicp = data.frame(First = CP0000EZ19M086NEST, date = zoo::index(CP0000EZ19M086NEST))
  hicp = hicp %>% 
         rename(hicp = CP0000EZ19M086NEST) %>%
         mutate(hicp = 100*(hicp - lag(hicp,12))/lag(hicp,12)) %>% 
         group_by(quarter = lubridate::quarter(date), year = lubridate::year(date)) %>%
         summarize(hicp = mean(hicp, na.rm = T)) %>% 
         ungroup() %>%
         mutate(date = dym(paste0('01',year,quarter*3))) %>%
         select(-quarter, -year)
  
  # Real Gross Domestic Product (Euro/ECU series) for Euro area (19 countries)
  quantmod::getSymbols.FRED('CLVMEURSCAB1GQEA19', env = globalenv())
  gdp = data.frame(First = CLVMEURSCAB1GQEA19, date = zoo::index(CLVMEURSCAB1GQEA19))
  gdp = gdp %>% 
        rename(gdp = CLVMEURSCAB1GQEA19) %>% 
        mutate(gdp = 100*(gdp - lag(gdp,4))/lag(gdp,4))
  
  # Harmonized Unemployment Rate: Total: All Persons for the Euro Area 
  quantmod::getSymbols.FRED('LRHUTTTTEZQ156S', env = globalenv())
  urate = data.frame(First = LRHUTTTTEZQ156S, date = zoo::index(LRHUTTTTEZQ156S))
  urate = urate %>% 
          rename(urate = LRHUTTTTEZQ156S) 
  month(urate$date) = month(urate$date) + 1
  
  # return data
  return(
    list(
      HICP  = hicp,
      GDP   = gdp,
      URATE = urate
    ) 
  )
}

# --- get forecasts --- #
createEuroSPFData = function(){
  Files = list.files('./Data/EuroSPF/')
  HICP = GDP = URATE = list()
  # loop through files
  for(file in Files){
    # import quarter data
    filename = paste0('./Data/EuroSPF/',file)
    rawTest = read.csv(filename, stringsAsFactors = F, header = F, fill = T) %>%
      rename(date = V1, forecaster = V2, point_forecast= V3) %>% 
      select(date, forecaster, point_forecast) %>% 
      mutate(point_forecast = as.numeric(point_forecast)) %>% 
      filter(str_detect(date,'[[:alpha:]]'))
    # define table rows
    hicpStart  = which(apply(rawTest,1, MARGIN = 2, FUN = grepl, pattern = 'HICP'))
    cinfStart  = which(apply(rawTest,1, MARGIN = 2, FUN = grepl, pattern = 'CORE'))-1
    gdpStart   = which(apply(rawTest,1, MARGIN = 2, FUN = grepl, pattern = 'REAL GDP'))-1
    urateStart = which(apply(rawTest,1, MARGIN = 2, FUN = grepl, pattern = 'UNEMPLOYMENT RATE'))-1
    # subset tables
    hicp  = rawTest[(hicpStart+2):(cinfStart-2),] 
    gdp   = rawTest[(gdpStart+3):(urateStart-2),]
    urate = rawTest[(urateStart+3):(nrow(rawTest)-1),]
    # choose correct forecast horizon
    hicp  = hicp[!duplicated(hicp[,2]),]
    gdp   = gdp[!duplicated(gdp[,2]),]
    urate = urate[!duplicated(urate[,2]),]
    # create date objects
    hicp = mutate(hicp, date = lubridate::ymd(paste0(date,'01')))
    gdp = mutate(gdp, date = as.Date(zoo::as.yearqtr(gdp$date, format = '%YQ%q')))
    urate = mutate(urate, date = lubridate::ymd(paste0(date,'01')))
    # remove NA's 
    hicp = na.omit(hicp)
    gdp = na.omit(gdp)
    urate = na.omit(urate)
    # cast from long to wide
    hicp = reshape2::dcast(hicp, form = date ~ forecaster, value.var = 'point_forecast')
    gdp = reshape2::dcast(gdp, form = date ~ forecaster, value.var = 'point_forecast')
    urate = reshape2::dcast(urate, form = date ~ forecaster, value.var = 'point_forecast')
    # place dataframes in lists
    HICP[[file]] = hicp
    GDP[[file]] = gdp
    URATE[[file]] = urate
  }
  # return data
  return(
    list(
      HICP  = reduce(HICP, bind_rows),
      GDP   = reduce(GDP, bind_rows),
      URATE = reduce(URATE, bind_rows)
    ) 
  )
}


# --- get forecast combinations --- #
getEuroComboData = function(Target,Engine,Version){
  fileName = paste0('./Data/Forecasts/Euro/derivedForecasts_uncond_',Engine,'_',Target,'_v',Version,'.csv')
  Data = read.csv(file = fileName, stringsAsFactors = F) %>% 
    mutate(date = ymd(date)) 
  colnames(Data)[2] = 'weights'
  return(Data)
}

