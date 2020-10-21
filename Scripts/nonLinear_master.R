# File: nonLinear_master_wrapper_v0.R
# Author: Tyler Pike
# Section: MA-MFA
# Date: 7/30/2019
# Note(s): Master script to set commong experiment enviroment and control what scripts are run

#reset space 
rm(list = ls())

#set working directory 
setwd('/scratch/m1tjp01/Paco/Nonlinear/ForecastCombination/')

#load in libraries
library(policyPlot)  # charting (FRB specific library)
library(forecast)    # time series foreasting 
library(caret)       # ML estimation 
library(glmnet)      # Lasso estimation 
library(pROC)        # AUC
library(tsfeatures)  # time series characteristics  
library(readxl)      # read excel files 
library(tidyverse)   # general cleaning
library(lubridate)   # date cleaning
library(doParallel)  # parallel backend
library(foreach)     # parallel backend

# set random seed
set.seed(11161995)

# version number 
version = '5'

# declare which experiments to run 
# USA variables
createTSmodels_usa   = FALSE
createFFORMA_usa     = FALSE
createCombo_usa      = FALSE
createAnalysis_usa   = FALSE
createTsAppendix_usa = FALSE

# EU variables
createCombo_euro     = FALSE
createAnalysis_euro  = FALSE

#----------------------------------------
# create forecast version folder 
#----------------------------------------
# instantiate version folder if missing
directory = paste0('./Data/Forecasts/US/version_',version)
if(!dir.exists(directory)){
  dir.create(directory)
}

#----------------------------------------
# create data helper functions 
# (must be run)
#----------------------------------------
# instantiate data functions
source('./Scripts/nonLinear_data_functions.R')

#----------------------------------------
# create time series forecasts
#----------------------------------------
# desciption: generates several deterministic time-series model forecasts of US macroeconomic variables 
# notes: this should only need to be done once, as they are deterministic. 
#        all forecasts begin in 1970:Q1 with data starting in ~1965:Q3
# input: pulls source data from FRED to forecast
# output: produces a forecast csv in ./Data/Forecasts, per target 
if(createTSmodels_usa == TRUE){
  system('Rscript ./Scripts/nonLinear_timeSeries_forecasts.R')
}

#----------------------------------------
# train FFORMA machine
#----------------------------------------
# desciption: trains and saves an instance of the FFORMA machine on a subset of M4 competition data
# notes: this should only need to be done once; very computationally expensive
# input: pulls source data from FRED to forecast
# output: produces a forecast csv in ./Data/Forecasts, per target 
if(createFFORMA_usa == TRUE){
  source('./Scripts/nonLinear_FFORMA_training.R')
}

#----------------------------------------
# create forecast combinations
#----------------------------------------
# description: creates a time series of forecast combinations based on US macro series
# input: a forecast csv in ./Data/Forecasts, per target (see createTSmodels_usa)
#        a pre-trained FFORMA model (see FFORMA_training.R)
# output: produces new file for each new version declared
#         csv of weighted forecaasts in ./Data/Forecasts/USA/ called derivedForecasts,
if(createCombo_usa == TRUE){
  source('./Scripts/nonLinear_metaLearner_functions_usa.R')
  source('./Scripts/nonLinear_fforma_forecasts.R')
}

# description: creates a time series of forecast combinations based on Euro SPF 
# input: takes in a csv of the euro spf individual forecasts, found in ./Data/EuroSPF/
# output: produces new file for each new version declared
#         csv of weighted forecaasts in ./Data/Forecasts/Euro called derivedForecasts,
if(createCombo_euro == TRUE){
  source('./Scripts/nonLinear_metaLearner_functions_euro.R')
}

#-----------------------------------------------
# analyze unconditional forecasts 
#-----------------------------------------------
# description; generate tables and charts of forecast combination results for US macro series
# input: takes in a csv for the individual forecast combination techniques at each specific horizon
# output: a forecast combinations chart as /Evaluation/UsaComparison/ForecastCharts/usaForecastChart_shaded.pdf
#         a series of result tables in /Evaluation/UsaComparison/
if(createAnalysis_usa == TRUE){
  source('./Scripts/nonLinear_forecast_analysis_usa.R')
}

# description; generates a table of individual time series forecsating model results for US macro series
# input: takes in a csv for the time series forecasts at each specific horizon
# output: a single results table in /Evaluation/UsaComparison/
if(createTsAppendix_usa == TRUE){
  source('./Scripts/nonLinear_timeSeriesforecast_appendix_usa.R')
}

# description; generate tables and charts of forecast combination results for Euro SPF series
# input: takes in a csv for the individual forecast combination techniques at each specific horizon
# output: a forecast combinations chart as /Evaluation/EuroComparison/ForecastCharts/euroForecastChart_shaded.pdf
#         a series of result tables in /Evaluation/EuroComparison/
if(createAnalysis_euro == TRUE){
  source('./Scripts/nonLinear_forecast_analysis_euro.R')
}





