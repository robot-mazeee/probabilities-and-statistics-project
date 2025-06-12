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