

####### Question 1 #########

# Load necessary libraries
library(stats)

set.seed(123)

# Generating 1000 Cauchy random variables
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# Defining the Kolmogorov-Smirnov test function
ks_test_normal <- function(data) {
  # Creating empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Generating the test statistic
  n <- length(data)
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  
  summed <- numeric(n)  # Creating a numeric vector of length
  for (i in 1:n) {
    summed[i] <- exp((-(2 * i - 1)^2 * pi^2) / ((8 * D)^2))
  }
  pval <- sqrt(2 * pi) / D * sum(summed)
  
  cat("D =", D, "\n")
  cat("p-value =", pval, "\n")
}

# Performing the Kolmogorov-Smirnov test
result <- ks_test_normal(cauchy_data)

# Verifying K-S test with R function
ks.test(cauchy_data, "pnorm")


########## Question 2 ############


# Creating the data

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Writing log-likelihood function for normal distribution

log_likelihood <- function(outcome, input, parameter) {
  # Extracting the number of independent variables
  n <- ncol(input)
  
  # Estimating the standard deviation
  sigma <- sqrt(parameter[n + 1])
  
  # Extracting intercept and coefficients
  beta <- parameter[1:n]
  
  # Calculating the log-likelihood
  -sum(dnorm(outcome, input %*% beta, sigma, log = TRUE))
  
}

results_log <- optim(
  fn = log_likelihood, 
  outcome = data$y, 
  input = cbind(1, data$x), 
  par = c(1, 1, 1), 
  hessian = TRUE
)
# Printing the estimated rounded coefficients (intercept and beta_1)
print(round(results_log$par[1:2], digits = 3))

# Comparing with lm
lm_comparison <- lm(y ~ x, data)
print(coef(lm_comparison))
