set.seed(5070)
n = 160
sigma0 = 3.7
k = 6

# Randomly select n-sized sample with no replacement
full_sample = scan("Exc10/sample.txt", sep = ",")
subsample = sample(full_sample, size = 160, replace = FALSE)


