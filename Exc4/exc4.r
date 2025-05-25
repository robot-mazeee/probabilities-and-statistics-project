# Weibull parameters
λ = 30
k = 11

# Calculate X expected value with Gamma
expected_value = λ * gamma(1 + 1/k)

set.seed(1819)

n = 6500

# Generate sample
sample = rweibull(n, shape = k, scale = λ)

# Calculate Monte Carlo expected value
mean_monte_carlo = mean(sample)

dif = round(abs(expected_value - mean_monte_carlo), 4)

print(dif)