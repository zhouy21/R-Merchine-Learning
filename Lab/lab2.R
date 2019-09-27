# Lab2 - Stepwise regression

library(tidyverse) 
library(ggplot2) 
library(ggthemes)
theme_set(theme_bw())

### Data preparation:
dd <- read_tsv("Lab/dataset/cogo-train.tsv")

# Split Train & Test datasets
set.seed(1234)
# Train-70%, Test-30%
dd$train <- sample(c(0, 1), nrow(dd), replace = TRUE, prob = c(.3, .7)) 
dd_test <- dd %>% filter(train == 0) 
dd_train <- dd %>% filter(train == 1)


### Stepwise regression =================================================
# Predictors:
xnames <- colnames(dd)
xnames <- xnames[!xnames %in% c("train", "p_open")]
xnames

## Forward selection:====================================================
# 1. Fit a Bascic model with intercep only
fit_fw <- lm(p_open ~ 1, data = dd_train)

# MSE
yhat_train <- predict(fit_fw, dd_train)
mse_train <- mean((dd_train$p_open - yhat_train) ^ 2)

yhat_test <- predict(fit_fw, dd_test) 
mse_test <- mean((dd_test$p_open - yhat_test) ^ 2)

# store the result:
log_fw <- 
  tibble( 
    xname = "intercept", 
    model = deparse(fit_fw$call), 
    mse_train = mse_train, 
    mse_test = mse_test
  )

# 2. add Predictors and find the smallest MSE
best_mse_train <- NA 
best_mse_test <- NA 
best_fit_fw <- NA 
best_xname <- NA

for (xname in xnames) {
  fit_fw_tmp <- update(fit_fw, as.formula(paste0(" ~ ", xname)))
  
  # compute MSE train
  yhat_train_tmp <- predict(fit_fw_tmp, dd_train)
  mse_train_tmp <- mean((dd_train$p_open - yhat_train_tmp) ^ 2)
  
  # compute MSE test
  yhat_test_tmp <- predict(fit_fw_tmp, dd_test)
  mse_test_tmp <- mean((dd_test$p_open - yhat_test_tmp) ^ 2)
  
  # if this is the first predictor to be examined,
  # or if this predictors yields a lower MSE than the current best
  # then store this predictor as the current best predictor
  if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
    best_xname <- xname
    best_fit_fw <- fit_fw_tmp
    best_mse_train <- mse_train_tmp
    best_mse_test <- mse_test_tmp
  }
}

# store the result:
log_fw <-
  log_fw %>% add_row(
    xname = best_xname,
    model = paste0(deparse(best_fit_fw$call), collapse = ""),
    mse_train = best_mse_train,
    mse_test = best_mse_test
  )
log_fw

# remove best_xname "state" from xnames:
xnames <- xnames[xnames!=best_xname]

# ==============================================================
# 叠加变量: Continue implementing the forward selection algorithm 
# by iteratively adding all predictors in correct order
xnames <- colnames(dd_train)
xnames <- xnames[!xnames %in% c("train", "p_open", "user_id")]

fit_fw <- lm(p_open ~ 1, data = dd_train)

yhat_train <- predict(fit_fw, dd_train)
mse_train <- mean((dd_train$p_open - yhat_train) ^ 2)

yhat_test <- predict(fit_fw, dd_test) 
mse_test <- mean((dd_test$p_open - yhat_test) ^ 2)

xname <- "intercept"

log_fw <-
  tibble(
    xname = xname,
    model = paste0(deparse(fit_fw$call), collapse = ""),
    mse_train = mse_train,
    mse_test = mse_test
  )

# an outer loop considers all potential predictors 
# until all of them have been added to the model
while (length(xnames) > 0) {
  # keep track of which is the next best variable to add
  best_mse_train <- NA
  best_mse_test <- NA
  best_fit_fw <- NA
  best_xname <- NA
  # select the next best predictor
  for (xname in xnames) {
    # fit a model that adds the predictor xname to the current best model
    # to do this you will want to use the update() command which can add a predictor
    # to an existing model
    fit_fw_tmp <- ...
    # compute MSE train
    yhat_train_tmp <- ...
    mse_train_tmp <- ...
    # compute MSE test
    yhat_test_tmp <- ...
    mse_test_tmp <- ...
    # if this is the first predictor to be examined,
    # or if this predictors yields a lower MSE that the current
    # best, then store this predictor as the current best predictor
    if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
      best_xname <- ...
      best_fit_fw <- ...
      best_mse_train <- ...
      best_mse_test <- ...
    }
  }
  # update the log
  log_fw <-
    log_fw %>% add_row(
      ...
    )
  # adopt the best model for the next iteration
  fit_fw <- best_fit_fw
  # remove the current best predictor from the list of predictors
  xnames <- xnames[xnames!=best_xname]
}


# Backward selection: ===================================================

  






