library(tidyverse)
library(tidymodels)

# q5
set.seed(123456)

# q4
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat", "medv")

# q6
# Create training and test sets using initial_split() from rsample
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

# Check dimensions of original training data
dim(housing_train)

# q7
# Create the recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,
            lstat,dis,nox, degree=6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# Check dimensions of prepped training data
dim(housing_train_prepped)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

# q8
# Set up for LASSO model
# Define the model specification with tuning parameter
lasso_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# Define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Create workflow for LASSO
lasso_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(lasso_spec)

# Tune the LASSO model
lasso_res <- lasso_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# Find optimal lambda for LASSO
top_lasso_rmse <- show_best(lasso_res, metric = "rmse")
best_lasso_rmse <- select_best(lasso_res, metric = "rmse")

# Print LASSO results
print("LASSO Results:")
print(top_lasso_rmse[1, ])
print(paste("Optimal lambda for LASSO:", best_lasso_rmse$penalty))

# Finalize the workflow with the best lambda for LASSO
final_lasso <- finalize_workflow(lasso_wf, best_lasso_rmse)

# Fit the LASSO model on the training data
lasso_fit <- fit(final_lasso, data = housing_train_prepped)

# Calculate LASSO in-sample RMSE
lasso_train_pred <- predict(lasso_fit, new_data = housing_train_prepped)
lasso_train_metrics <- bind_cols(
  housing_train_prepped %>% select(medv),
  lasso_train_pred
) %>% 
  metrics(truth = medv, estimate = .pred)

lasso_in_sample_rmse <- lasso_train_metrics %>% 
  filter(.metric == "rmse") %>% 
  pull(.estimate)

# Calculate LASSO out-of-sample RMSE
lasso_test_pred <- predict(lasso_fit, new_data = housing_test_prepped)
lasso_test_metrics <- bind_cols(
  housing_test_prepped %>% select(medv),
  lasso_test_pred
) %>% 
  metrics(truth = medv, estimate = .pred)

lasso_out_sample_rmse <- lasso_test_metrics %>% 
  filter(.metric == "rmse") %>% 
  pull(.estimate)

print(paste("LASSO in-sample RMSE:", lasso_in_sample_rmse))
print(paste("LASSO out-of-sample RMSE:", lasso_out_sample_rmse))

# q9
# RIDGE REGRESSION
# Set up Ridge model
ridge_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 0 = ridge, 1 = lasso
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# Create workflow for Ridge
ridge_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(ridge_spec)

# Tune the Ridge model
ridge_res <- ridge_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# Find optimal lambda for Ridge
top_ridge_rmse <- show_best(ridge_res, metric = "rmse")
best_ridge_rmse <- select_best(ridge_res, metric = "rmse")

# Print Ridge results
print("Ridge Results:")
print(top_ridge_rmse[1, ])
print(paste("Optimal lambda for Ridge:", best_ridge_rmse$penalty))

# Finalize the workflow with the best lambda for Ridge
final_ridge <- finalize_workflow(ridge_wf, best_ridge_rmse)

# Fit the Ridge model on the training data
ridge_fit <- fit(final_ridge, data = housing_train_prepped)

# Calculate Ridge in-sample RMSE
ridge_train_pred <- predict(ridge_fit, new_data = housing_train_prepped)
ridge_train_metrics <- bind_cols(
  housing_train_prepped %>% select(medv),
  ridge_train_pred
) %>% 
  metrics(truth = medv, estimate = .pred)

ridge_in_sample_rmse <- ridge_train_metrics %>% 
  filter(.metric == "rmse") %>% 
  pull(.estimate)

# Calculate Ridge out-of-sample RMSE
ridge_test_pred <- predict(ridge_fit, new_data = housing_test_prepped)
ridge_test_metrics <- bind_cols(
  housing_test_prepped %>% select(medv),
  ridge_test_pred
) %>% 
  metrics(truth = medv, estimate = .pred)

ridge_out_sample_rmse <- ridge_test_metrics %>% 
  filter(.metric == "rmse") %>% 
  pull(.estimate)

print(paste("Ridge in-sample RMSE:", ridge_in_sample_rmse))
print(paste("Ridge out-of-sample RMSE:", ridge_out_sample_rmse))

# Compare LASSO and Ridge performance
print("Model Comparison:")
print(paste("LASSO out-of-sample RMSE:", lasso_out_sample_rmse))
print(paste("Ridge out-of-sample RMSE:", ridge_out_sample_rmse))