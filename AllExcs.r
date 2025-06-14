# --------------------------------- exc 4 ---------------------------------

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

# --------------------------------- exc 5 ---------------------------------

n = 32000

set.seed(1149)

# Simulate play
numbers = sample(1:6, 3 * n, replace = TRUE)
data = matrix(numbers, nrow = n, ncol = 3)

# Sum three dice rolls of each play
sums = rowSums(data)

# Get relative ferquency of occurrences of 9s and 10s
freq_rel9 = sum(sums == 9) / n
freq_rel10 = sum(sums == 10) / n

# Calculate difference
diff = freq_rel10 - freq_rel9

# Round to 4 decimal points
print(round(diff, 4))

# --------------------------------- exc 6 ---------------------------------

n = 12
x = 5.75

# Calculate exact pn value

irwinhall = function(x, n) {
  sapply(x, function(xi) {
    if (xi < 0 || xi > n) return(0)
    k_vals = 0:floor(xi)
    sum_term = sum((-1)^k_vals * choose(n, k_vals) * (xi - k_vals)^n)
    return(sum_term / factorial(n))
  })
}

pn = irwinhall(x, n)
cat("Exact value:", pn, "\n")

# Calculate approximate value of pn using Central Limit Theorem (CLT)

irwinhall_clt = function(x, n) {
  mu = n / 2
  sigma = sqrt(n / 12)
  pnorm(x, mean = mu, sd = sigma)
}

pn_clt = irwinhall_clt(x, n)
cat("Approximation using Central Limit Theorem:", pn_clt, "\n")

# Calculate approximate value of pn using Simulation

set.seed(5457)
m = 150

# Create m samples with n uniform values
samples = matrix(runif(m * n), nrow = m, ncol = n)
# Calculate Sn for each sample
Sn = rowSums(samples)
# Calculate the proportion of times where Sn <= x
pn_sim = mean(Sn <= x)

cat("Simulated proportion (P(Sn <= 5.75)):", pn_sim, "\n")

# Calculate absolute deviations

abs_deviation_clt = abs(pn - pn_clt)
cat("Absolute deviation between exact pn and CLT approximation:", abs_deviation_clt, "\n")

abs_deviation_sim = abs(pn - pn_sim)
cat("Absolute deviation between exact pn and sim approximation:", abs_deviation_sim, "\n")

# Calculate absolute deviations quotient
quocient = round(abs_deviation_clt / abs_deviation_sim, 4)
cat("Absolute deviations quotient:", quocient, "\n")

# --------------------------------- exc 7 ---------------------------------

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

# --------------------------------- exc 8 ---------------------------------

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

# --------------------------------- exc 9 ---------------------------------

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

# --------------------------------- exc 10 --------------------------------

set.seed(5070)
n = 160
sigma0 = 3.7
k = 6

# Randomly select n-sized sample with no replacement
full_sample = scan("Exc10/sample.txt", sep = ",")
subsample = sample(full_sample, size = n, replace = FALSE)

# Divide sample in 6 equiprobable classes
probs = seq(1/k, (k-1)/k, by = 1/k)
# Inverted Rayleigh
cutoffs = sigma0 * sqrt(-2 * log(1 - probs))
breaks = c(0, cutoffs, Inf)

# Classify samples
subsample_classes <- cut(subsample, breaks = breaks, include.lowest = TRUE, right = TRUE)

# Absolute frequencies
frequencies <- table(subsample_classes)
print(frequencies)

# Calculate p-value
expected_freq <- rep(n / k, k)
chi_test <- chisq.test(x = as.numeric(frequencies), p = rep(1/k, k))
p_value = chi_test$p.value
cat("p-value of test:", p_value, "\n")

if (p_value < 0.01) {
    print("Reject H0 for insignificance level of 1%.")
} else {
    print("Don't reject H0 for insignificance level of 1%.")
} 
if (p_value < 0.05) {
    print("Reject H0 for insignificance level of 5%.")
} else {
    print("Don't reject H0 for insignificance level of 5%.")
} 
if (p_value < 0.1) {
    print("Reject H0 for insignificance level of 10%.")
} else {
    print("Don't reject H0 for insignificance level of 10%.")
}

#  0.009841374 < 0.01
#  0.009841374 < 0.05
#  0.009841374 < 0.1
#  Reject for all