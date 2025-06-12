n = 13
sum_xi = 119.68
sum_log_xi = 28.82

xi_mean = sum_xi / n
log_xi_mean <- sum_log_xi / n

# Equation of likelihood (needs to be 0)
score_function = function(alpha) {
  log(alpha) - digamma(alpha) - log(xi_mean) + log_xi_mean
}

# Maximum likelihood estimates
estimated_alpha = uniroot(score_function, interval = c(0.001, 169.8))$root
cat("Maximum likelihood alpha estimate:", estimated_alpha, '\n')
estimated_lambda = estimated_alpha / xi_mean
cat("Maximum likelihood lambda estimate:", estimated_lambda, '\n')

mode = round((estimated_alpha - 1) / estimated_lambda, 2)
cat("Modal length:", mode, '\n')