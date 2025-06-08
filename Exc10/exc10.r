set.seed(5070)
n = 160
sigma0 = 3.7
k = 6

# Randomly select n-sized sample with no replacement
full_sample = scan("Exc10/sample.txt", sep = ",")
subsample = sample(full_sample, size = 160, replace = FALSE)

# Divide sample in 6 equiprobable classes
probs = seq(1/6, 5/6, by = 1/6)
# Inverted Rayleigh
cutoffs = sigma0 * sqrt(-2 * log(1 - probs))
breaks = c(0, cutoffs, Inf)

# Classify samples
subsample_classes <- cut(subsample, breaks = breaks, include.lowest = TRUE, right = TRUE)

# Absolute frequencies
frequencies <- table(subsample_classes)
print(frequencies)

# Calculate p-value
expected_freq <- rep(160 / 6, 6)
chi_test <- chisq.test(x = as.numeric(frequencies), p = rep(1/6, 6))
p_value = chi_test$p.value
cat("p-value of test:", p_value, "\n")

#  0.009841374 < 0.01
#  0.009841374 < 0.05
#  0.009841374 < 0.1
#  Reject for all