set.seed(2786)
n = 26
m = 700
mu0 = 3
mu1 = 3.5
alpha = 0.05

crit_value = qchisq(1 - alpha, df = 2 * n)

samples = matrix(rexp(n * m, rate = 1 / mu1), nrow = m, ncol = n)

sample_means = rowMeans(samples)

T0 = (2 * n * sample_means) / mu0

accept_H0 = T0 <= crit_value

beta_hat = mean(accept_H0)

beta = pgamma(crit_value, shape = 2 * n, rate = mu0 / mu1)

quociente = round(beta_hat / beta, 4)

cat("Beta estimado (beta^):", round(beta_hat, 4), "\n")
cat("Beta teórico:", round(beta, 4), "\n")
cat("Quociente:", quociente, "\n")
