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