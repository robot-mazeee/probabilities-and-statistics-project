n = 13
sum_xi = 119.68
sum_log_xi = 28.82

x_bar = sum_xi / n
rhs = log(x_bar) - sum_log_xi / n

score_function = function(alpha) {
  log(alpha) - digamma(alpha) - rhs
}

estimated_alpha = uniroot(score_function, interval = c(0.001, 169.8))$root

estimated_lambda = estimated_alpha / x_bar

mode = round((estimated_alpha - 1) / estimated_lambda, 2)
print(mode)
