#q4
wages_data <- read.csv("wages.csv")

str(wages_data)

#q5

wages_clean <- wages_data[!is.na(wages_data$hgc) & !is.na(wages_data$tenure), ]


#q6

library(modelsummary)


# Calculate the missing rate for logwage
missing_logwage <- sum(is.na(wages_data$logwage)) / nrow(wages_data)
missing_percentage <- missing_logwage * 100
cat("Missing rate for logwage:", round(missing_percentage, 2), "%\n\n")

# Create a summary table of the data frame
datasummary_skim(
  wages_clean,
  output = "latex",
  title = "Summary Statistics of Women's Wages Data (1988)",
)

#q7

library(mice)

# Create tenure squared variable
wages_clean$tenure_sq <- wages_clean$tenure^2

# METHOD 1: Complete cases (listwise deletion)
complete_cases <- wages_clean[!is.na(wages_clean$logwage), ]
model_complete <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = complete_cases)

# METHOD 2: Mean imputation
# Create copy of dataset
wages_mean_imp <- wages_clean
# Calculate mean of logwage
mean_logwage <- mean(wages_clean$logwage, na.rm = TRUE)
# Replace missing values with the mean
wages_mean_imp$logwage[is.na(wages_mean_imp$logwage)] <- mean_logwage
# Fit the model
model_mean <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = wages_mean_imp)

# METHOD 3: Regression imputation
# Fit a model using complete cases
imputation_model <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = complete_cases)
# Create copy of dataset
wages_reg_imp <- wages_clean
# Predict missing values
missing_indices <- which(is.na(wages_clean$logwage))
wages_reg_imp$logwage[missing_indices] <- predict(imputation_model, newdata = wages_clean[missing_indices, ])
# Fit the model with imputed values
model_reg <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = wages_reg_imp)

# METHOD 4: Multiple imputation using mice
# Set seed for reproducibility
set.seed(1236)
# Create multiple imputations
imp <- mice(wages_clean, m = 5, method = "pmm", maxit = 50, print = FALSE)
# Fit models to all imputed datasets
models_mice <- with(imp, lm(logwage ~ hgc + college + tenure + tenure_sq + age + married))
# Pool results
model_mice_pooled <- pool(models_mice)

# Create a list of models to compare
models <- list(
  "Complete Cases" = model_complete,
  "Mean Imputation" = model_mean,
  "Regression Imputation" = model_reg,
  "Multiple Imputation" = model_mice_pooled
)

# Generate regression table in LaTeX format
modelsummary(models,
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             title = "Comparison of Imputation Methods for Missing Logwage Data",
             output = "latex")