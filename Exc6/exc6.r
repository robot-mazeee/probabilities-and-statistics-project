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

abs_deviation_clt <- abs(pn - pn_clt)
cat("Absolute deviation between exact pn and CLT approximation:", abs_deviation_clt, "\n")

abs_deviation_sim <- abs(pn - pn_sim)
cat("Absolute deviation between exact pn and sim approximation:", abs_deviation_sim, "\n")

# Calculate absolute deviations quotient
quocient = round(abs_deviation_clt / abs_deviation_sim, 4)
cat("Absolute deviations quotient:", quocient, "\n")
