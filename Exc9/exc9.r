set.seed(2786)
n = 26
m = 700
mu0 = 3
mu1 = 3.5
alpha = 0.05

# Reject H0 critic value
crit_value = qchisq(1 - alpha, df = 2 * n)

# Generate samples and means
samples = matrix(rexp(n * m, rate = 1 / mu1), nrow = m, ncol = n)
sample_means = rowMeans(samples)

# Test statistic for each sample
T0 = (2 * n * sample_means) / mu0

# Get the samples that do not reject H0
accept_H0 = T0 <= crit_value

# Beta estimate
beta_hat = mean(accept_H0)

# Beta theoretical value
beta = pchisq(crit_value*(mu0/mu1), df = 2*n)

quotient = round(beta_hat / beta, 4)
cat("Quotient between estimate and theoretical beta:", quotient, '\n')