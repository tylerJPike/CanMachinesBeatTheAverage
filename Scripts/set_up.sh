#!/bin/bash

# Run this script to set up the required folder structure to replicate the project
#  (with the assumption of operating in a unix enviroment)

# Create data folders for forecast output
mkdir ./Data/Forecasts
mkdir ./Data/Forecasts/Euro
mkdir ./Data/Forecasts/US
mkdir ./Data/Forecasts/US/TimeSeries

# Create evaluation folders for analysis output
mkdir ./Evaluation
mkdir ./Evaluation/EuroComparison
mkdir ./Evaluation/EuroComparison/ForecastTables
mkdir ./Evaluation/EuroComparison/ForecastCharts
mkdir ./Evaluation/USComparison
mkdir ./Evaluation/USComparison/ForecastTables
mkdir ./Evaluation/USComparison/ForecastCharts