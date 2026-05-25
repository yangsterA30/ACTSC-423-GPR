suppressPackageStartupMessages({
  if(!require(tidyverse)){install.packages("tidyverse")}
  if(!require(tidyquant)){install.packages("tidyquant")}
  if(!require(tibbletime)){install.packages("tibbletime")}
  if(!require(quantmod)){install.packages("quantmod")}
  if(!require(timetk)){install.packages("timetk")}
  if(!require(broom)){install.packages("broom")}
  if(!require(highcharter)){install.packages("highcharter")}
  if(!require(IRdisplay)){install.packages("IRdisplay")}
  if(!require(arrow)){install.packages("arrow")}
  if(!require(glmnet)){install.packages("glmnet")}
  if(!require(keras)){install.packages("keras")}
  if(!require(randomForest)){install.packages("randomForest")}
  if(!require(adabag)){install.packages("adabag")}
  if(!require(xgboost)){install.packages("xgboost")}
  if(!require(tensorflow)){install.packages("tensorflow")}
  if(!require(keras)){install.packages("keras")}
  if(!require(quadprog)){install.packages("quadprog")}
  library(tidyverse)
  library(tidyquant)
  library(tibbletime)
  library(quantmod)
  library(timetk)
  library(broom)
  library(highcharter)
  library(IRdisplay)
  library(arrow)
  library(httr)
  library(data.table)
  library(dplyr)
  library(arrow)
  library(glmnet)
  library(MASS)
  library(lars)
  library(ggplot2)
  library(randomForest)
  library(adabag)
  library(xgboost)
  library(tensorflow)
  library(keras)
  library(quadprog)
})


tickers <- c("aapl", "amzn", "googl", "jpm", "meta", "msft",
             "nvda", "orcl", "tsla", "uber")


# Downloading Option Data from Github
base_url <- "https://static.philippdubach.com/data/options/%s/options.parquet"
for(t in tickers){
  
  url <- sprintf(base_url, t)
  dest <- paste0(t, "_options.parquet")
  
  download.file(url, destfile = dest, mode = "wb")
  
  cat("Downloaded:", dest, "\n")
}


# Downloading Underlying Info from Github
base_url <- "https://static.philippdubach.com/data/options/%s/underlying.parquet"
for(t in tickers){
  
  url <- sprintf(base_url, t)
  dest <- paste0(t, "_underlying.parquet")
  
  download.file(url, destfile = dest, mode = "wb")
  
  cat("Downloaded:", dest, "\n")
}



########################
## Data Preprocessing ##
########################

# Importing the data in parquet file stored in desktop
ds <- open_dataset("~/Desktop/OptionDataFile")
dt <- open_dataset("~/Desktop/OptionUnderlying")

# Preprocessing (Only keep option issued from 2024-01-01 to 2025-01-01)
options <- ds %>%
  filter(!is.na(last),
         !is.na(implied_volatility),
         !is.na(delta),
         !is.na(gamma),
         !is.na(vega),
         !is.na(theta),
         !is.na(rho),
         bid != 0,
         last != 0) %>%
  filter(date > "2024-01-01 19:00:00",
         date < "2025-01-01 19:00:00") %>%
  collect() %>%
  arrange(contract_id, date)
  

# Getting Stock Data from Underlying Asset in the Same Parquet files from GitHub
underlying <- dt %>%
  filter(date > "2024-01-01 19:00:00",
         date < "2025-01-01 19:00:00") %>%
  collect()


# Merging the underlying data to the options data
options <- options %>%
  left_join(underlying, by = c("symbol", "date")) %>%
  arrange(contract_id, date)


# Extracting contract ids
contract_id <- unique(options$contract_id)


# Call options
calls <- subset(options, options$type == "call")
calls <- split(calls, calls$contract_id)


# Put Options
puts <- subset(options, options$type == "put")


# Checking the Boundaries of American Put Options
mispriced_idx_puts <- which(puts$last > puts$strike)      # Put Option price must be <= Strike
## Comment: No put options violate the bound


# Checking the Boundaries for American Call Options
bad_call_contracts <- subset(options, options$last > options$close & 
                             options$type == "call")
bad_call_contracts <- unique(bad_call_contracts$contract_id)
options <- options %>%
  filter(!contract_id %in% bad_call_contracts) # Filter out the bad call contracts


# Grouping options into list by contract
options_split <- options %>%
  arrange(contract_id, date) %>%
  split(.$contract_id)


# Check if there are options with zero volume over at least 7 calendar days in a row
zero_vol <- function(contract) {
  a <- contract$volume.x
  r <- rle(a)
  r_0 <- r$lengths[r$values == 0]
  if (any(r_0 >= 7)) {
    return(FALSE)
  }
  return(TRUE)
}

# Remove option contracts with zero volume over at least 7 calendar days in a row
zero_vol_bool <- lapply(options_split, zero_vol)
zero_vol_names <- names(which(zero_vol_bool == FALSE))
options <- options %>%
  filter(!contract_id %in% zero_vol_names) # Filter out the bad contracts

options_split <- options %>%
  arrange(contract_id, date) %>%
  split(.$contract_id)


compute_delta_hedge_daily <- function(contract_1, rf = 0.046) {
  
  contract_1 <- contract_1[order(contract_1$date), ]
  
  S <- contract_1$close
  V <- contract_1$last
  delta <- contract_1$delta
  id <- contract_1$contract_id
  
  n <- length(S)
  
  if(n < 2 || anyNA(S) || anyNA(V) || anyNA(delta)) return(NULL)
  
  dS <- S[-1] - S[-n]
  dV <- V[-1] - V[-n]
  
  rf_daily <- rf / 365
  
  Pi <- dV - delta[-n] * dS - rf_daily * (V[-n] - delta[-n] * S[-n])
  
  initial <- abs(delta[-n] * S[-n] - V[-n])
  initial[initial < 1e-4] <- NA
  
  r_daily <- Pi / initial
  
  return(data.frame(
    contract_id = id[-1],
    date = contract_1$date[-1],
    dh_return_daily = r_daily
  ))
}

returns_df <- do.call(rbind, lapply(split(options, options$contract_id),
                                    compute_delta_hedge_daily))


options <- merge(options, returns_df, 
                 by = c("contract_id", "date"), 
                 all.x = TRUE)

options_clean <- options %>%
  group_by(contract_id) %>%
  filter(!(row_number() == 1 & is.na(dh_return_daily))) %>%
  ungroup()


# Apply the compute_delta_hedge onto the split option data
return_vec <- lapply(options_split, compute_delta_hedge_daily)

# Unlist it to make it back to numeric vectors
return_vec <- unlist(return_vec)

# Checking if there is contract if there is missing stock value
which(NA %in% return_vec)

# Append the values with contract_id sorted
options$dh_return_daily <- return_vec

# Check that Delta-Hedged Return is 0 at Expiration Dates
subdat <- subset(options, options$expiration == options$date)
which(subdat$dh_return != 0)

# Add the predictor moneyness (i.e., S_t/K for all t)
options$moneyness <- options$open/options$strike

# Checking for Uniformity
options %>%
  select(-dh_return) %>% # Remove returns
  select(-contract_id) %>%
  select(-symbol) %>%
  select(-type) %>%
  select(-date) %>%
  select(-expiration) %>%
  pivot_longer(cols = c("delta", "gamma", "theta", "vega", "rho"),
               names_to = "Attribute", values_to = "Value") %>% # Convert to 'compact' column mode
  ggplot(aes(x = Value, fill = Attribute)) +
  geom_histogram() + theme_light() + # Plot histograms
  facet_grid(Attribute~., scales = "free") 

options %>%
  select(-dh_return) %>% # Remove returns
  select(-contract_id) %>%
  select(-symbol) %>%
  select(-type) %>%
  select(-date) %>%
  select(-expiration) %>%
  pivot_longer(cols = c("implied_volatility", "strike", "last", "mark", "bid"),
               names_to = "Attribute", values_to = "Value") %>% # Convert to 'compact' column mode
  ggplot(aes(x = Value, fill = Attribute)) +
  geom_histogram() + theme_light() + # Plot histograms
  facet_grid(Attribute~., scales = "free") 

options %>%
  select(-dh_return) %>% # Remove returns
  select(-contract_id) %>%
  select(-symbol) %>%
  select(-type) %>%
  select(-date) %>%
  select(-expiration) %>%
  pivot_longer(cols = c("bid_size", "ask", "ask_size", "volume.x", "open_interest"),
               names_to = "Attribute", values_to = "Value") %>% # Convert to 'compact' column mode
  ggplot(aes(x = Value, fill = Attribute)) +
  geom_histogram() + theme_light() + # Plot histograms
  facet_grid(Attribute~., scales = "free") 

options %>%
  select(-dh_return) %>% # Remove returns
  select(-contract_id) %>%
  select(-symbol) %>%
  select(-type) %>%
  select(-date) %>%
  select(-expiration) %>%
  pivot_longer(cols = c("open", "high", "low", "close", "adjusted_close"),
               names_to = "Attribute", values_to = "Value") %>% # Convert to 'compact' column mode
  ggplot(aes(x = Value, fill = Attribute)) +
  geom_histogram() + theme_light() + # Plot histograms
  facet_grid(Attribute~., scales = "free") 

options %>%
  select(-dh_return) %>% # Remove returns
  select(-contract_id) %>%
  select(-symbol) %>%
  select(-type) %>%
  select(-date) %>%
  select(-expiration) %>%
  pivot_longer(cols = c("volume.y", "moneyness"),
               names_to = "Attribute", values_to = "Value") %>% # Convert to 'compact' column mode
  ggplot(aes(x = Value, fill = Attribute)) +
  geom_histogram() + theme_light() + # Plot histograms
  facet_grid(Attribute~., scales = "free") 

# Print out summary of options data
summary(options)


# Investigate observations with Delta-Hedged return > 1
max <- subset(options, options$dh_return > 1)
max_call <- subset(max, max$type == "call")
## Comments: For call options, Delta-Hedged option returns are > 1 with strike
##           being much greater than the closing price of the stock (max return : 13.73592)
##           These huge Delta-Hedged returns all have moneyness below the mean/median. All delta is
##           greater than 0.

max_put <- subset(max, max$type == "put")
## Comments: For put options, Delta-Hedged option returns are > 1 with some > 100 with 
##           strike being much lesser than the closing price of the stock (max return: 813.5231)
##           These huge Delta-Hedged returns all have moneyness above the median and with some
##           below the mean but most are much larger than the mean and median moneyness. Another notable
##           thing is all of the deltas are below 0 for this

## Comments: These could also be due to the volatility effects (i.e., Gamma)


# Investigate observations with Delta-Hedged return < -1
min <- subset(options, options$dh_return < -1)
min_call <- subset(min, min$type == "call")

# Downloading the Processed Data in csv
write.csv(options, file = "processed_options.csv")

# All of the characteristics of the options requires uniformization!
# Proceed with uniformization
norm_unif <- function (v) {
  v <- v %>% as.matrix()
  return(ecdf(v)(v))
}

predictors <- c("delta", "gamma", "theta", "vega", "rho", "implied_volatility", 
                "strike", "last", "mark", "bid", "bid_size", "ask", "ask_size", 
                "volume.x", "open_interest", "open", "high", "low", "close", 
                "adjusted_close", "volume.y", "moneyness")

for (i in 1:length(predictors)) {
  options[, predictors[i]] <- norm_unif(options[, predictors[i]])
}

summary(options)


# Change the contract types (Call or Put) into binary variable
options <- options %>%
  mutate(type_binary = ifelse(type == "call", 1, 0))

# Export the data: write.csv(options, file = "uniformized_options.csv")

contract_1 <- subset(options, options$contract_id == "AAPL240405C00160000")
write.csv(contract_1, file = "contract.csv")


###################
## Model Fitting ##
###################
options <- read.csv("processed_options.csv")

compute_delta_hedge_daily <- function(contract_1, rf = 0.046) {
  
  contract_1 <- contract_1[order(contract_1$date), ]
  
  S <- contract_1$close
  V <- contract_1$last
  delta <- contract_1$delta
  id <- contract_1$contract_id
  
  n <- length(S)
  
  if(n < 2 || anyNA(S) || anyNA(V) || anyNA(delta)) return(NULL)
  
  dS <- S[-1] - S[-n]
  dV <- V[-1] - V[-n]
  
  rf_daily <- rf / 365
  
  Pi <- dV - delta[-n] * dS - rf_daily * (V[-n] - delta[-n] * S[-n])
  
  initial <- abs(delta[-n] * S[-n] - V[-n])
  initial[initial < 1e-4] <- NA
  
  r_daily <- Pi / initial
  
  return(data.frame(
    contract_id = id[-1],
    date = contract_1$date[-1],
    dh_return_daily = r_daily
  ))
}

returns_df <- do.call(rbind, lapply(split(options, options$contract_id),
                                    compute_delta_hedge_daily))


options <- merge(options, returns_df, 
                 by = c("contract_id", "date"), 
                 all.x = TRUE)

options$moneyness <- options$open/options$strike

options <- options %>%
  arrange(contract_id, date) %>%
  group_by(contract_id) %>%
  mutate(
    delta_lag = lag(delta, 1),
    gamma_lag = lag(gamma, 1),
    vega_lag  = lag(vega, 1),
    theta_lag = lag(theta, 1),
    rho_lag = lag(rho, 1),
    implied_volatility_lag = lag(implied_volatility, 1),
    high_lag = lag(high, 1),
    low_lag = lag(low, 1),
    close_lag = lag(close, 1),
    adjusted_close_lag = lag(adjusted_close, 1),
    volume.x_lag = lag(volume.x, 1),
    volume.y_lag = lag(volume.y, 1),
    bid_size_lag = lag(bid_size, 1),
    ask_size_lag = lag(ask_size, 1),
    bid_lag = lag(bid, 1),
    ask_lag = lag(ask, 1),
    open_interest_lag = lag(open_interest, 1)
  ) %>%
  ungroup()

options <- options %>%
  arrange(contract_id, date) %>%
  group_by(contract_id) %>%
  mutate(
    mid_price = (bid_lag + ask_lag)/2, 
    spread = ask_lag - bid_lag,
    rel_spread = spread/mid_price,
    depth_imb = (bid_size_lag - ask_size_lag)/(bid_size_lag + ask_size_lag)
  ) %>%
  ungroup()

options_clean <- options %>%
  group_by(contract_id) %>%
  filter(!(row_number() == 1 & is.na(dh_return_daily))) %>%
  ungroup()


options_clean <- options_clean %>%
  mutate(type_binary = ifelse(type == "call", 1, 0))




# Split the dataset into Training, Validation and Out-of-Sample Testing Data
training_data <- options_clean %>%
  mutate(date = as.Date(date)) %>%
  mutate(expiration = as.Date(expiration)) %>%
  filter(date >= "2024-01-01",
         date < "2024-08-01") 

testing_data <- options_clean %>%
  mutate(date = as.Date(date)) %>%
  mutate(expiration = as.Date(expiration)) %>%
  filter(date >= "2024-08-01",
         date < "2024-10-01")

out_of_sample <- options_clean %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  mutate(expiration = as.Date(expiration)) %>%
  filter(date >= "2024-10-01",
         date < "2024-11-01")

training_data <- training_data[order(training_data$date), ]
testing_data <- testing_data[order(testing_data$date), ]
out_of_sample <- out_of_sample[order(out_of_sample$date), ]

# Numeric Issue Dates and Expiration Dates
training_data$date <- as.numeric(training_data$date)
training_data$expiration <- as.numeric(training_data$expiration)
testing_data$date <- as.numeric(testing_data$date)
testing_data$expiration <- as.numeric(testing_data$expiration)
out_of_sample$date <- as.numeric(out_of_sample$date)
out_of_sample$expiration <- as.numeric(out_of_sample$expiration)


# Constructing Time to Maturity Variable
training_data$ttm <- training_data$expiration - training_data$date
testing_data$ttm <- testing_data$expiration - testing_data$date
out_of_sample$ttm <- out_of_sample$expiration - out_of_sample$date

training_data$moneyness <- training_data$open/training_data$strike
testing_data$moneyness <- testing_data$open/testing_data$strike
out_of_sample$moneyness <- out_of_sample$open/out_of_sample$strike

winsorize <- function(x, lower, upper) {
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

lower_bound <- quantile(training_data$dh_return_daily, 0.01, na.rm = TRUE)
upper_bound <- quantile(training_data$dh_return_daily, 0.999, na.rm = TRUE)

training_data$dh_return_daily <- winsorize(training_data$dh_return_daily,
                                           lower_bound, upper_bound)

testing_data$dh_return_daily <- winsorize(testing_data$dh_return_daily,
                                          lower_bound, upper_bound)

out_of_sample$dh_return_daily <- winsorize(out_of_sample$dh_return_daily,
                                           lower_bound, upper_bound)


features <- c("delta_lag", "gamma_lag", "rho_lag", "vega_lag", "theta_lag", 
              "implied_volatility_lag", "strike", "open_interest_lag", "open", 
              "moneyness", "type_binary", "ttm", "high_lag", "low_lag", "close_lag",
              "adjusted_close_lag", "volume.x_lag", "volume.y_lag", "bid_size_lag",
              "ask_size_lag", "bid_lag", "ask_lag", "mid_price", "spread", "rel_spread",
              "depth_imb") 

ecdf_list <- list()

# Step 1: fit on training data
for (i in 1:(length(features))) {
  if (all(is.na(training_data[[features[i]]]))) {
    message("Feature ", i)
    break
  }
  f <- ecdf(training_data[[features[i]]])
  ecdf_list[[features[i]]] <- f
  
  training_data[[features[i]]] <- f(training_data[[features[i]]])
}

for (i in 1:(length(features))) {
  f <- ecdf_list[[features[i]]]
  
  testing_data[[features[i]]] <- f(testing_data[[features[i]]])
  out_of_sample[[features[i]]] <- f(out_of_sample[[features[i]]])
}


y <- training_data$dh_return_daily
x <- as.matrix(training_data[, features]) # Removes dividend amount, type, dh_return and X column from csv
new_x <- as.matrix(testing_data[, features])
out_of_sample_x <- as.matrix(out_of_sample[, features])


## Ridge Regression
set.seed(123)
CV.ridge <- cv.glmnet(x, y, alpha = 0) # Use CV method to find the best lambda
fit.ridge <- glmnet(x, y, lambda = CV.ridge$lambda.min, alpha = 0)  # Fits Ridge Regression
fit.ridge$beta  # Checks the coefficient

mean((predict(fit.ridge, new_x) - testing_data$dh_return_daily)^2) # PMSE: 0.01629578
mean(predict(fit.ridge, new_x) * testing_data$dh_return_daily > 0) # Hit Ratio: 0.4567195

predict(fit.ridge, out_of_sample_x)

## Lasso Regression
CV.lasso <- cv.glmnet(x, y, alpha = 1) # Use CV method to find the best lambda
fit.lasso <- glmnet(x, y, lambda = CV.ridge$lambda.min, alpha = 1)  # Fits Lasso Regression
fit.lasso$beta  # Checks the coefficient


mean((predict(fit.lasso, new_x) - testing_data$dh_return_daily)^2) # PMSE: 0.0162808
mean(predict(fit.lasso, new_x) * testing_data$dh_return_daily > 0) # Hit Ratio: 0.4422864


predicted_return <- predict(fit.lasso, out_of_sample_x)
out_of_sample$predicted_return <- predicted_return



## Random Forest
set.seed(123)
features <- c("delta", "gamma", "theta", "vega", "rho", 
              "implied_volatility", "strike", "open_interest", "open", 
              "moneyness", "type_binary", "ttm")

formula <- paste("dh_return_daily ~", paste(features, collapse = " + ")) # Defines the model 
formula <- as.formula(formula)                                   # Forcing formula object


trees <- c(10, 20, 40, 80, 160)

models <- lapply(trees, function (t){
  randomForest(formula,             # Same formula as for simple trees!
               data = training_data,    # Data source: training sample
               sampsize = 30000,          # Size of (random) sample for each tree
               replace = FALSE,           # Is the sampling done with replacement?
               nodesize = 250,            # Minimum size of terminal cluster
               ntree = t,                # Nb of random trees
               mtry = 5                  # Nb of predictive variables for each tree
  )
})

MSE <- lapply(models, function(m) {
  mean((predict(m, testing_data) - testing_data$dh_return)^2) # MSE
})

MSE

# [[1]]
# [1] 0.01314853
# 
# [[2]]
# [1] 0.01323233
# 
# [[3]]
# [1] 0.01289695
# 
# [[4]]
# [1] 0.01308392
# 
# [[5]]
# [1] 0.01302768
# 
# [[6]]
# [1] 0.01297946
# 
# [[7]]
# [1] 0.01296376
# 
# [[8]]
# [1] 0.01298328




hit_ratios <- lapply(models, function(m) {
  mean(predict(m, testing_data) * testing_data$dh_return > 0) # Hit ratio
})

hit_ratios

# [[1]]
# [1] 0.4949686
# 
# [[2]]
# [1] 0.49306
# 
# [[3]]
# [1] 0.4959406
# 
# [[4]]
# [1] 0.4959837
# 
# [[5]]
# [1] 0.4961995
# 
# [[6]]
# [1] 0.4926185
# 
# [[7]]
# [1] 0.4975826
# 
# [[8]]
# [1] 0.4959406


# Comment: Here we will choose tree size of 40 based on its performance on testing data
rf_mod <- models[[3]]

rf_mod <- randomForest(formula,             # Same formula as for simple trees!
                       data = training_data,    # Data source: training sample
                       sampsize = 30000,          # Size of (random) sample for each tree
                       replace = FALSE,           # Is the sampling done with replacement?
                       nodesize = 250,            # Minimum size of terminal cluster
                       ntree = 40,                # Nb of random trees
                       mtry = 5                  # Nb of predictive variables for each tree
                      )

predicted_return <- predict(rf_mod, out_of_sample_x)
out_of_sample$predicted_return <- predicted_return



## XGBoost
features <- c("delta", "gamma", "theta", "vega", "rho", 
              "implied_volatility", "strike", "open_interest", "open", 
              "moneyness", "type_binary", "ttm")

training_data <- training_data[order(training_data$date), ]

train_features_xgb <- training_data %>% 
  dplyr::select(all_of(features)) %>% as.matrix()       # Independent variable

train_label_xgb <- training_data %>%
  dplyr::select(dh_return_daily) %>% as.matrix()                      # Dependent variable

train_matrix_xgb <- xgb.DMatrix(data = train_features_xgb, 
                                label = train_label_xgb)        # XGB format!

fit_xgb <- xgb.train(data = train_matrix_xgb,     # Data source 
                     eta = 0.3,                          # Learning rate
                     objective = "reg:linear",           # Objective function (Squared Error Loss)
                     max_depth = 4,                      # Maximum depth of trees
                     lambda = 1,                         # Penalisation of leaf values
                     gamma = 0.1,                        # Penalisation of number of leaves
                     nrounds = 30,                       # Number of trees used (rather low here)
                     verbose = 0                         # No comment from the algo 
)

xgb_test <- testing_data %>%                                # Test sample => XGB format
  dplyr::select(all_of(features)) %>% 
  as.matrix() 

mean((predict(fit_xgb, xgb_test) - testing_data$dh_return_daily)^2) # PMSE: 0.01641125

mean(predict(fit_xgb, xgb_test) * testing_data$dh_return_daily > 0) # Hit ratio: 0.5107722


xgb_out <- out_of_sample %>%                                # Out of sample => XGB format
  dplyr::ungroup() %>%
  dplyr::select(all_of(features)) %>% 
  as.matrix() 

predicted_return <- predict(fit_xgb, xgb_out)
out_of_sample$predicted_return <- predicted_return
predicted_return_orig <- xts(predicted_return, order.by = out_of_sample$date)

# # Walk-Forward Loop 
# training_data <- training_data[order(training_data$date), ]
# # Define window parameters
# train_size <- 252 * 2 # 2 years of daily data for training
# test_size <- 21 # Test on the next month (approx 21 trading days)
# total_rows <- nrow(training_data)
# predictions <- numeric(total_rows)
# 
# 
# # 2. Start the Walk-Forward Loop
# 
# # We 'walk forward' by the test_size at each step
# for (start in seq(1, (total_rows - train_size), by = test_size)) {
#   # Define indices for current fold
#   train_idx <- start:(start + train_size - 1)
#   test_idx <- (start + train_size):min(start + train_size + test_size - 1,
#                                        total_rows)
#   if (length(test_idx) == 0) break
#   
#   # Split data
#   dtrain <- xgb.DMatrix(data = as.matrix(training_data[train_idx, features]),
#                         label = training_data[train_idx, "dh_return"])
#   dtest <- as.matrix(training_data[test_idx, features])
#   
#   # Train model (using info only up to train_idx)
#   model <- xgb.train(params = list(objective = "reg:squarederror"),
#                      data = dtrain, nrounds = 100, verbose = 0)
#   
#   # Predict on 'unseen' future data
#   predictions[test_idx] <- predict(model, dtest)
#   # message(paste("Testing on:", training_data$date[test_idx[1]], "to",
#   #               training_data$date[tail(test_idx, 1)]))
# }
# 
# 
# # 3. Add these "clean" deltas back to your dataset for hedging
# training_data$delta_clean <- predictions


# Max Drawdown Comparison
# library(PerformanceAnalytics)
# mdd_orig <- maxDrawdown(predicted_return_orig, invert = FALSE)
# mdd_wf <- maxDrawdown(returns_wf, invert = FALSE)
# 
# cat("original XGB Max Drawdown:", mdd_orig * 100, "%\n")
# cat('Walk-Forward XGB Max Drawdown:', mdd_wf * 100, "%\n")





## Feed-Forward Neural Network
# install_keras()

NN_train_features <- dplyr::select(training_data, features) %>%    # Training features
  as.matrix()                                                      # Matrix = important
NN_train_labels <- training_data$dh_return_daily                   # Training labels
NN_test_features <- dplyr::select(testing_data, features) %>%      # Testing features
  as.matrix()                                                      # Matrix = important
NN_test_labels <- testing_data$dh_return_daily                      # Testing labels
NN_out_of_sample <- out_of_sample %>%                                # Out of sample => XGB format
  dplyr::ungroup() %>%
  dplyr::select(all_of(features)) %>% 
  as.matrix()                            

model <- keras_model_sequential()
model %>%   # This defines the structure of the network, i.e. how layers are organized
  layer_dense(units = 32, activation = 'relu', input_shape = ncol(NN_train_features)) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1) # No activation means linear activation: f(x) = x.

model %>%
  layer_dense(units = 16, activation = 'relu',
              input_shape = ncol(NN_train_features),
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 8, activation = 'relu',
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 1)



model %>% compile(                             # Model specification
  loss = 'mean_squared_error',               # Loss function
  optimizer = optimizer_rmsprop(),           # Optimisation method (weight updating)
  metrics = c('mean_absolute_error')         # Output metric
)
summary(model)                                 # Model architecture

fit_NN <- model %>% 
  fit(NN_train_features,                                       # Training features
      NN_train_labels,                                         # Training labels
      epochs = 20, batch_size = 512,                           # Training parameters
      validation_data = list(NN_test_features, NN_test_labels) # Test data
  ) 
plot(fit_NN)                                                     # Plot, evidently!

predicted_return <- predict(model, NN_out_of_sample)
out_of_sample$predicted_return <- predicted_return



###############
### Summary ###
###############

MSE_summary <- c(mean((predict(fit.ridge, new_x) - testing_data$dh_return_daily)^2),
                 mean((predict(fit.lasso, new_x) - testing_data$dh_return_daily)^2),
                 mean((predict(rf_mod, testing_data) - testing_data$dh_return_daily)^2), 
                 mean((predict(fit_xgb, xgb_test) - testing_data$dh_return_daily)^2), 
                 mean((predict(model, NN_test_features) - testing_data$dh_return_daily)^2))
MSE_summary




# [1] 0.01593562 0.01589819 0.01913066 0.11520516 0.01590702



hit_ridge <- mean(predict(fit.ridge, new_x) * testing_data$dh_return_daily > 0)
hit_lasso <- mean(predict(fit.lasso, new_x) * testing_data$dh_return_daily > 0)
hit_rand <- mean(predict(rf_mod, testing_data) * testing_data$dh_return_daily > 0)
hit_xgb <- mean(predict(fit_xgb, xgb_test) * testing_data$dh_return_daily > 0)
hit_NN <- mean(predict(model, NN_test_features) * testing_data$dh_return_daily > 0)

hit_summary <- c(mean(predict(fit.ridge, new_x) * testing_data$dh_return_daily > 0),
                 mean(predict(fit.lasso, new_x) * testing_data$dh_return_daily > 0),
                 mean(predict(rf_mod, testing_data) * testing_data$dh_return_daily > 0),
                 mean(predict(fit_xgb, xgb_test) * testing_data$dh_return_daily > 0),
                mean(predict(model, NN_test_features) * testing_data$dh_return_daily > 0))
hit_summary




# [1] 0.5138297 0.5193873 0.4982097 0.5278039 0.4522442

predicted_return <- list(
  as.numeric(predict(fit.ridge, out_of_sample_x)),
  as.numeric(predict(fit.lasso, out_of_sample_x)),
  as.numeric(predict(rf_mod, out_of_sample_x)),
  as.numeric(predict(fit_xgb, xgb_out)),
  as.numeric(predict(model, NN_out_of_sample))
)
mean_ls <- rep(0, 5)
sharpe_ls <- rep(0, 5)

out_of_sample$date <- as.Date(out_of_sample$date)
out_of_sample$expiration <- as.Date(out_of_sample$expiration)
ls_portfolios <- list()
ls_portfolio_return <- list()
long_short_portfolio <- list()

library(dplyr)
library(xts)
library(PerformanceAnalytics)

mean_ls <- rep(NA, 5)
sharpe_ls <- rep(NA, 5)

# Ensure proper date format
out_of_sample$date <- as.Date(out_of_sample$date)


for (i in 1:5) {
  message(i)
  # Attach predictions
  preds_i <- predicted_return[[i]]
  df <- out_of_sample %>%
    ungroup() %>%
    arrange(contract_id, date) %>%
    mutate(predicted_return = preds_i) %>%
    
    # Lag the signal (no look-ahead)
    group_by(contract_id) %>%
    mutate(predicted_return = lag(predicted_return, 1)) %>%
    ungroup()
  
  # Remove NA introduced by lag
  df <- df %>%
    filter(!is.na(predicted_return), !is.na(dh_return_daily))
  
  # Construct deciles cross-sectionally
  df <- df %>%
    group_by(date) %>%
    mutate(decile = ntile(predicted_return, 10)) %>%
    ungroup()
  
  # Compute portfolio returns
  portfolio_returns_ls <- df %>%
    group_by(date, decile) %>%
    summarise(
      ret = mean(dh_return_daily, na.rm = TRUE),
      .groups = "drop"
    )
  
  ls_portfolio_return[[i]] <- portfolio_returns_ls
  
  # Long-short portfolio
  long_short <- portfolio_returns_ls %>%
    filter(decile %in% c(1, 10)) %>%
    pivot_wider(names_from = decile, values_from = ret) %>%
    mutate(
      long_short = `10` - `1`
    ) %>%
    arrange(date)
  
  ls_portfolios[[i]] <- long_short
  
  # Mean return
  mean_ls[i] <- mean(long_short$long_short, na.rm = TRUE)
  
  # Convert to xts safely
  ls_xts <- xts(long_short$long_short, order.by = as.Date(long_short$date))
  
  # Sharpe ratio
  sharpe_ls[i] <- SharpeRatio.annualized(
    ls_xts,
    Rf = 0.046 / 252,
    scale = 252
  )
}

mean_ls


# [1] -1.483139e-04 -7.305542e-05  2.725396e-05  3.013381e-04  8.246935e-04


sharpe_ls


# [1] -2.085106 -2.693519 -0.682621  0.864267  1.421018

ls_combined <- bind_rows(ls_portfolios, .id = "model")
ls_combined$model <- factor(ls_combined$model,
                            labels = c("Ridge", "LASSO", "RF", "XGB", "NN"))

ggplot(ls_combined, aes(x = as.Date(date), y = long_short, color = model)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Return",
    color = "Model"
  ) +
  theme_minimal()


# Long-Only Portfolio Summary
long_portfolio <- list()
mean_l <- rep(0, 5)
sharpe_l <- rep(0, 5)

for (i in 1:5) {
  preds_i <- predicted_return[[i]]
  df <- out_of_sample %>%
    ungroup() %>%
    arrange(contract_id, date) %>%
    mutate(predicted_return = preds_i) %>%
    group_by(contract_id) %>%
    mutate(predicted_return = lag(predicted_return, 1)) %>%  
    ungroup() %>%
    filter(!is.na(predicted_return), !is.na(dh_return_daily))
  
  df <- df %>%
    group_by(date) %>%
    mutate(decile = ntile(predicted_return, 10)) %>%
    ungroup()
  
  portfolio_returns <- df %>%
    group_by(date, decile) %>%
    summarise(
      ret = mean(dh_return_daily, na.rm = TRUE),
      .groups = "drop"
    )
  
  # === LONG ONLY ===
  long_only <- portfolio_returns %>%
    filter(decile == 10) %>%
    arrange(date)
  
  long_portfolio[[i]] <- long_only
  
  mean_l[i] <- mean(long_only$ret, na.rm = TRUE)
  
  long_xts <- xts(long_only$ret, order.by = as.Date(long_only$date))
  sharpe_l[i] <- SharpeRatio.annualized(long_xts, Rf = 0.046/252, scale = 252)
}

mean_l

# [1] -4.559534e-04 -4.041405e-04 -1.578862e-04 -1.949380e-04 -5.085501e-05

sharpe_l

# [1] -2.3336402 -2.0040015 -1.2419322 -1.2853664 -0.4987878

l_combined <- bind_rows(long_portfolio, .id = "model")
l_combined$model <- factor(l_combined$model,
                            labels = c("Ridge", "LASSO", "RF", "XGB", "NN"))

ggplot(l_combined, aes(x = as.Date(date), y = ret, color = model)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Return",
    color = "Model"
  ) +
  theme_minimal()


# Short-Only Portfolio Summary
short_portfolio <- list()
mean_s <- rep(0, 5)
sharpe_s <- rep(0, 5)

for (i in 1:5) {
  preds_i <- predicted_return[[i]]
  df <- out_of_sample %>%
    ungroup() %>%
    arrange(contract_id, date) %>%
    mutate(predicted_return = preds_i) %>%
    group_by(contract_id) %>%
    mutate(predicted_return = lag(predicted_return, 1)) %>%  
    ungroup() %>%
    filter(!is.na(predicted_return), !is.na(dh_return_daily))
  
  df <- df %>%
    group_by(date) %>%
    mutate(decile = ntile(predicted_return, 10)) %>%
    ungroup()
  
  portfolio_returns <- df %>%
    group_by(date, decile) %>%
    summarise(
      ret = mean(dh_return_daily, na.rm = TRUE),
      .groups = "drop"
    )
  
  # === SHORT ONLY ===
  short_only <- portfolio_returns %>%
    filter(decile == 1) %>%
    arrange(date) %>%
    mutate(ret = -ret)  
  
  short_portfolio[[i]] <- short_only
  
  mean_s[i] <- mean(short_only$ret, na.rm = TRUE)
  
  short_xts <- xts(short_only$ret, order.by = as.Date(short_only$date))
  sharpe_s[i] <- SharpeRatio.annualized(short_xts, Rf = 0.046/252, scale = 252)
}

mean_s

# [1] 0.0003076395 0.0003310851 0.0001851402 0.0004962762 0.0008755485

sharpe_s

# [1]  0.31658514  0.42505752 -0.04170507  1.13241186  2.73623713


s_combined <- bind_rows(short_portfolio, .id = "model")
s_combined$model <- factor(s_combined$model,
                           labels = c("Ridge", "LASSO", "RF", "XGB", "NN"))

ggplot(s_combined, aes(x = as.Date(date), y = ret, color = model)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Return",
    color = "Model"
  ) +
  theme_minimal()



###########################
### Efficiency Frontier ###
###########################

outliers <- subset(out_of_sample, out_of_sample$dh_return > 1)
outliers$predicted_return <- predicted_return[[1]]

# Efficiency Frontier Function
efficient_frontier <- function(mu, Sigma, n_points = 100) {
  
  one <- rep(1, length(mu))
  Sigma_inv <- solve(Sigma)
  
  A <- as.numeric(t(one) %*% Sigma_inv %*% one)
  B <- as.numeric(t(one) %*% Sigma_inv %*% mu)
  C <- as.numeric(t(mu) %*% Sigma_inv %*% mu)
  D <- A * C - B^2
  
  # Wider range
  mu_seq <- seq(min(mu) - 2*sd(mu),
                max(mu) + 2*sd(mu),
                length.out = n_points)
  
  frontier <- data.frame(mean = numeric(n_points),
                         sd = numeric(n_points))
  
  for (i in 1:n_points) {
    target <- mu_seq[i]
    
    w <- ((C - B * target) / D) * (Sigma_inv %*% one) +
      ((A * target - B) / D) * (Sigma_inv %*% mu)
    
    frontier$mean[i] <- sum(w * mu)
    frontier$sd[i] <- sqrt(t(w) %*% Sigma %*% w)
  }
  
  return(frontier)
}


# Long-Short
frontier_list_ls <- list()

library(corpcor)
for (i in seq_along(ls_portfolio_return)) {
  
  portfolio_returns <- ls_portfolio_return[[i]]
  
  returns_wide <- portfolio_returns %>%
    tidyr::pivot_wider(names_from = decile, values_from = ret) %>%
    arrange(date)
  
  returns_matrix <- returns_wide %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  mu <- colMeans(returns_matrix, na.rm = TRUE)
  
  Sigma <- corpcor::cov.shrink(returns_matrix)
  
  frontier <- efficient_frontier(mu, Sigma, n_points = 200)
  
  frontier <- frontier %>%
    arrange(sd) %>%
    mutate(max_mean = cummax(mean)) %>%
    filter(mean >= max_mean - 1e-10)
  
  frontier_list_ls[[i]] <- frontier %>%
    mutate(model = paste0("Model ", i))
}

frontiers_all_ls <- bind_rows(frontier_list_ls)
model_names <- c("Ridge", "Lasso", "RF", "XGB", "NN")
frontiers_all_ls$model <- factor(frontiers_all_ls$model, labels = model_names)


ggplot(frontiers_all_ls, aes(x = sd, y = mean, color = model)) +
  geom_line(linewidth = 1.2) +
  xlim(0.003, 0.007) +
  labs(
    x = "Risk (SD)",
    y = "Expected Return",
    color = "Model"
  ) +
  theme_minimal()




### Gamma Sensitivity
dDelta <- diff(out_of_sample$delta)
dS <- diff(out_of_sample$close)

realized_gamma <- dDelta/dS
realized_gamma[is.infinite(realized_gamma)] <- NA

plot(out_of_sample$close[-1], realized_gamma)


## Checking for Hedge Error 
n <- nrow(out_of_sample)
options_split <- options %>%
  arrange(contract_id, date) %>%
  split(.$contract_id)


contract_1 <- subset(out_of_sample, out_of_sample$contract_id == "AAPL250815C00240000")
contract_1 <- subset(out_of_sample, out_of_sample$contract_id == "AAPL250815C00210000")
contract_1 <- subset(out_of_sample, out_of_sample$contract_id == "AAPL250620C00330000")

contract_id_out <- unique(out_of_sample$contract_id)

contract_1 <- subset(out_of_sample, out_of_sample$contract_id == contract_id_out[2313])

n <- nrow(contract_1)
dV <- contract_1$last[-1] - contract_1$last[-n]
dS <- contract_1$close[-1] - contract_1$close[-n]
delta <- contract_1$delta[-n]
fin <- (0.046 / 252) * (contract_1$last[-n] - delta * contract_1$close[-n])
residuals <- dV - (delta * dS) - fin

plot(dS, residuals,
     main = "Residual Analysis: Hedge Error vs. Stock Move",
     xlab = "Stock Price Change (dS)",
     ylab = "Hedge Error (Residual)",
     pch = 20, col = rgb(0.1, 0.2, 0.8, 0.5))
abline(h = 0, col = "red", lty = 2)
# Add a smoothing line to see systemic bias
lines(lowess(dS, residuals), col = "orange", lwd = 2)


## Max Drawdown
library(PerformanceAnalytics)

# Maximum Drawdown for Long Short
for (i in 1:5) {
  return_month <- ls_portfolios[[i]]
  returns_orig <- xts(return_month$long_short, order.by = return_month$date)
  mdd_orig <- maxDrawdown(returns_orig, invert = FALSE)
  cat("Max Drawdown: ", mdd_orig * 100, "%\n")
}

# In the Order of Ridge, Lasso, RF, XGBoost, NN

# Max Drawdown:  -1.0053 %
# Max Drawdown:  -0.5921202 %
# Max Drawdown:  -1.807707 %
# Max Drawdown:  -0.4757644 %
# Max Drawdown:  -2.381102 %

# Maximum Drawdown for Long Only
for (i in 1:5) {
  return_month <- long_portfolio[[i]]
  returns_orig <- xts(return_month$ret, order.by = return_month$date)
  mdd_orig <- maxDrawdown(returns_orig, invert = FALSE)
  cat("Max Drawdown: ", mdd_orig * 100, "%\n")
}

# In the Order of Ridge, Lasso, RF, XGBoost, NN

# Max Drawdown:  -1.597888 %
# Max Drawdown:  -1.679594 %
# Max Drawdown:  -0.9725774 %
# Max Drawdown:  -1.210067 %
# Max Drawdown:  -3.029782 %

# Maximum Drawdown for Short Only
for (i in 1:5) {
  return_month <- short_portfolio[[i]]
  returns_orig <- xts(return_month$ret, order.by = return_month$date)
  mdd_orig <- maxDrawdown(returns_orig, invert = FALSE)
  cat("Max Drawdown: ", mdd_orig * 100, "%\n")
}

# In the Order of Ridge, Lasso, RF, XGBoost, NN
# Max Drawdown:  -2.35283 %
# Max Drawdown:  -2.185223 %
# Max Drawdown:  -2.859081 %
# Max Drawdown:  -1.626596 %
# Max Drawdown:  -1.062198 %


######################
## Variable Heatmap ##
######################
rf_importance <- importance(rf_mod)
rf_importance <- as.numeric(rf_importance)
names(rf_importance) <- features

xgb_imp <- xgb.importance(model = fit_xgb)

# create full vector aligned with feature_names
xgb_vec <- setNames(rep(0, length(features)), features)
xgb_vec[xgb_imp$Feature] <- xgb_imp$Gain


# extract weights
w <- model$get_weights()

# first layer weights matrix
W1 <- w[[1]]   # shape: (features × hidden_units)
W2 <- w[[2]]  # hidden → output (or next layer)

nn_imp <- rowSums(abs(W1 %*% W2))
names(nn_imp) <- features
nn_imp <- nn_imp / sum(nn_imp)

lasso_coef <- as.matrix(coef(fit.lasso, s = CV.lasso$lambda.min))
lasso_coef <- lasso_coef[-1, , drop = FALSE]   # remove intercept
lasso_coef <- as.vector(lasso_coef)
names(lasso_coef) <- features

ridge_coef <- as.matrix(coef(fit.ridge, s = CV.ridge$lambda.min))
ridge_coef <- ridge_coef[-1, , drop = FALSE]   # remove intercept
ridge_coef <- as.vector(ridge_coef)
names(ridge_coef) <- features

weights <- data.frame(
  feature = features,
  Ridge   = ridge_coef,
  Lasso   = lasso_coef,
  RF      = rf_importance,
  XGBoost = xgb_vec,
  NN      = nn_imp
)

rownames(weights) <- weights$feature

weights_long <- weights %>%
  mutate(feature = rownames(weights)) %>%
  pivot_longer(-feature, names_to = "model", values_to = "weight")

ggplot(weights_long, aes(x = model, y = feature, fill = weight)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "lightblue",
    mid = "white",
    high = "darkblue",
    midpoint = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Model", y = "Feature", fill = "Weight")



##########################
##### Market Analysis ####
##########################

tickers <- c("AAPL", "AMZN", "GOOGL", "JPM", "META", "MSFT",
             "NVDA", "ORCL", "TSLA", "UBER")

risk_premium <- rep(0, length(tickers))
real_v <- rep(0, length(tickers))
impli_v <- rep(0, length(tickers))

for (i in 1:length(tickers)) {
  stock <- subset(out_of_sample, out_of_sample$symbol == tickers[i])
  S <- stock %>%
    dplyr::select(date, close, implied_volatility) %>%
    distinct(date, .keep_all = TRUE) %>%              # remove duplicates properly
    arrange(date)
  
  returns <- diff(log(S$close))
  iv <- mean(stock$implied_volatility)
  rv <- sd(returns) * sqrt(252)
  real_v[i] <- rv
  impli_v[i] <- iv
  risk_premium[i] <- iv - rv 
}



  