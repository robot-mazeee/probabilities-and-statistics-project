# Weibull parameters
λ = 30
k = 11

# Calculate X expected value with Gamma
expected_value = λ * gamma(1 + 1/k)
cat("Expected value:", expected_value, '\n')

set.seed(1819)

n = 6500

# Generate sample
sample = rweibull(n, shape = k, scale = λ)

# Calculate Monte Carlo expected value
mean_monte_carlo = mean(sample)

cat("Monte Carlo value:", mean_monte_carlo, '\n')

dif = round(abs(expected_value - mean_monte_carlo), 4)

cat("Difference between expected and Monte Carlo values:", dif, '\n')