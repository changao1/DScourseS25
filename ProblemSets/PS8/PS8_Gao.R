## Q4
set.seed(100)

N <- 100000
K <- 10

X <- matrix(rnorm(N * K), nrow = N, ncol = K)

# Set the first column to all 1's
X[, 1] <- 1

# Create eps
sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)

# Define beta vector with the given values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X %*% beta + eps

## Q5
# Compute the OLS estimate using the closed-form solution
# beta_OLS = (X'X)^(-1)X'Y
beta_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y

# Print the OLS estimate
cat("OLS estimate (beta_OLS):\n")
print(beta_OLS)

# Print the true beta for comparison
cat("\nTrue beta values:\n")
print(beta)

# Calculate the difference between estimated and true beta
cat("\nDifference (beta_OLS - beta):\n")
print(beta_OLS - beta)


## Q6
# Set up step size
alpha <- 0.0000003

# Set up number of iterations
iter <- 10000

# Define the gradient function
# For OLS regression, the loss function is L(β) = ||Y - Xβ||^2
# Its gradient is gradient = -2X'(Y - Xβ), function of gradient is
gradient <- function(X, Y, beta) {
  return(-2 * t(X) %*% (Y - X %*% beta))
}

# Randomly initialize beta value
set.seed(100)
beta_grad <- runif(K, -1, 1)

# Create matrix to store all beta values for all iterations
beta_history <- matrix(0, nrow = iter, ncol = K)

# Gradient descent method to find the minimum
for(i in 1:iter) {
  # Calculate current gradient
  grad <- gradient(X, Y, beta_grad)
  
  # Update beta
  beta_grad <- beta_grad - alpha * grad
  
  # Store current beta value
  beta_history[i, ] <- beta_grad
  
  # Print results every 1000 iterations
  if(i %% 1000 == 0) {
    cat("Beta values after", i, "iterations:\n")
    print(beta_grad)
  }
}

# Print final results
cat("\nOLS estimate using Gradient Descent:\n")
print(beta_grad)

cat("\nOLS estimate using closed-form solution:\n")
print(beta_OLS)

cat("\nTrue beta values:\n")
print(beta)

# Calculate difference from true values
cat("\nDifference (Gradient Descent - True Values):\n")
print(beta_grad - beta)

# Calculate difference between estimators
cat("\nDifference (Gradient Descent - closed-form solution of OLS):\n")
print(beta_grad - beta_OLS)

## Q7
library(nloptr)

# the OLS objective function (sum of squared residuals) is
eval_f <- function(beta) {
  residuals <- Y - X %*% beta
  return(sum(residuals^2))
}

# Gradient of the objective function
eval_grad_f <- function(beta) {
  residuals <- Y - X %*% beta
  return(-2 * t(X) %*% residuals)
}

# Initial values
x0 <- rep(0, K)
x1 <- rep(0, K)

# L-BFGS optimization
opts <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-8)

# Find the optimum
res_lbfgs <- nloptr(x0, eval_f, eval_grad_f=eval_grad_f, opts=opts)

# Nelder-Mead optimization
options <- list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1.0e-8)

# Find the optimum
res_nm <- nloptr(x1, eval_f, opts=options)

cat("L-BFGS Results:\n")
print(res_lbfgs$solution)

cat("Nelder-Mead Results:\n")
print(res_nm$solution)

cat("\nDifference between L-BFGS and Nelder-Mead:\n")
print(res_lbfgs$solution - res_nm$solution)

## Q8

# Define the negative log-likelihood function for normal regression
neg_log_likelihood <- function(theta, Y, X) {
  # Extract beta and sigma from theta
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  
  # Calculate residuals
  residuals <- Y - X %*% beta
  
  # Calculate negative log-likelihood
  n <- length(Y)
  nll <- n * log(sig) + sum(residuals^2) / (2 * sig^2)
  
  return(nll)
}

# Gradient function
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  
  return(grad)
}

# Initial values (beta from OLS and sigma estimated from residuals)
beta_init <- beta_OLS  # Use OLS estimate as starting point
residuals_init <- Y - X %*% beta_init
sigma_init <- sqrt(sum(residuals_init^2) / length(Y))
theta_init <- c(beta_init, sigma_init)

opts <- list(
  "algorithm" = "NLOPT_LD_LBFGS",
  "xtol_rel" = 1.0e-8,
  "maxeval" = 1000
)

# Find the MLE estimate
res_mle <- nloptr(
  x0 = theta_init,
  eval_f = neg_log_likelihood,
  eval_grad_f = gradient,
  opts = opts,
  Y = Y,
  X = X
)

beta_hat_mle <- res_mle$solution[1:K]

# Print results
cat("MLE estimate (beta_hat_MLE):\n")
print(beta_hat_mle)

## Q9

library(modelsummary)
ols_model <- lm(Y ~ X - 1)

modelsummary(
  ols_model,
  output = "PS8_Gao.tex",
  title = "OLS Regression Results"
)

# Compare with true beta values
cat("\nTrue beta values:\n")
print(beta)

# Calculate difference
cat("\nDifference (OLS - True):\n")
print(coef(ols_model) - beta)
