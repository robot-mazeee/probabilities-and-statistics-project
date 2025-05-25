set.seed(1257)

m = 1800
n = 18

mu = 0.3
sigma = 0.5
gamma = 0.97
alpha = 1-gamma

# Calculate zscore
error = qnorm(1 - alpha/2) * sigma/sqrt(n)

# Generate samples and means
samples <- matrix(rnorm(n * m, mean = mu, sd = sigma), nrow = m, ncol = n)
sample_means <- rowMeans(samples)

# Confidence intervals
ci_lowers = sample_means - error
ci_uppers = sample_means + error

# Proportion of intervals that contain mu
contains_mu = (ci_lowers <= mu) & (ci_uppers >= mu)
proportion <- mean(contains_mu)
cat("Proportion of intervals that contain mu:", proportion, "\n")

# Quotient
quotient = round(proportion / gamma, 4)
cat("Quotient:", quotient, "\n")