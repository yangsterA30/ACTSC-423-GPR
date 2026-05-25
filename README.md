# ACTSC-423-GPR
A replication and empirical research project for ACTSC 423 (Topics in Financial Econometrics) focused on analyzing delta-hedged option returns and testing predictability in option markets using ML techniques.

## Overview
This project explores the use of machine learning models to predict delta-hedged option returns using stock and option related variables and compare their effectiveness in constructing profitable portfolios consisting of long/short position of options. 

The models analyzed are:
* Ridge Regression
* Lasso Regression
* Random Forest
* XGBoost
* Neural Network (MLP/FNN)

## Data
The dataset was obtained from Philipp Dubach’s publicly available options database:
* Historical options data for 104 U.S. equities and ETFs
* Time period: 2008–2025
* Over 20 million observations
* MIT Licensed
  
For computational efficiency, this project uses data from:
* Training: Jan 1, 2024 – Jul 31, 2024
* Testing: Aug 1, 2024 – Sep 30, 2024
* Out-of-Sample Evaluation: Oct 1, 2024 – Oct 31, 2024
  
Selected equities include:
* Apple (AAPL)
* Tesla (TSLA)
* Amazon (AMZN)
* NVIDIA (NVDA)
* Microsoft (MSFT)
* JPMorgan (JPM)

## Recommended Environment
* R
* Python 3.10 (recommended for TensorFlow/Keras compatibility)


