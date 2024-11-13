# 8.18
# Initial sample details
sigma <- 12  # Population standard deviation derived from earlier calculation
target_sd <- 1.2  # Desired SD for the sampling distribution

# Calculate required sample size
n <- (sigma / target_sd)^2
n  # Answer: Required sample size

# 8.23
x_values <- c(4, 5, 6, 7)
probabilities <- c(0.2, 0.4, 0.3, 0.1)

# Part (a): Mean (mu) and Variance (sigma^2) of X
mu <- sum(x_values * probabilities)
E_X2 <- sum((x_values^2) * probabilities)
variance_X <- E_X2 - mu^2
mu  # Mean of X
variance_X  # Variance of X

# Part (b): Mean and Variance of sample mean (X̄) for n = 36
n <- 36
mu_X_bar <- mu
variance_X_bar <- variance_X / n
mu_X_bar  # Mean of sample mean
variance_X_bar  # Variance of sample mean

# Part (c): Probability that mean time in 36 puffs is less than 5.5
std_X_bar <- sqrt(variance_X_bar)
z_score <- (5.5 - mu_X_bar) / std_X_bar
probability_c <- pnorm(z_score)
probability_c  # Probability

# 8.26
mu <- 3.2
sigma <- 1.6
n <- 64
sigma_X_bar <- sigma / sqrt(n)  # SD of sample mean

# Part (a): Probability mean time <= 2.7
z_score_a <- (2.7 - mu) / sigma_X_bar
probability_a <- pnorm(z_score_a)

# Part (b): Probability mean time > 3.5
z_score_b <- (3.5 - mu) / sigma_X_bar
probability_b <- 1 - pnorm(z_score_b)

# Part (c): Probability 3.2 <= mean time < 3.4
z_score_c1 <- (3.2 - mu) / sigma_X_bar
z_score_c2 <- (3.4 - mu) / sigma_X_bar
probability_c <- pnorm(z_score_c2) - pnorm(z_score_c1)

probability_a  # Probability for part (a)
probability_b  # Probability for part (b)
probability_c  # Probability for part (c)

# 8.41
sigma2 <- 6
n <- 25
df <- n - 1

# Part (a): Probability S^2 > 9.1
chi2_value_a <- 4 * 9.1  # Transform to Chi-square
probability_a <- 1 - pchisq(chi2_value_a, df)

# Part (b): Probability 3.462 < S^2 < 10.745
chi2_value_b1 <- 4 * 3.462
chi2_value_b2 <- 4 * 10.745
probability_b <- pchisq(chi2_value_b2, df) - pchisq(chi2_value_b1, df)

probability_a  # Probability for part (a)
probability_b  # Probability for part (b)

# 8.48
mu <- 30
sample_mean <- 27.5
sample_sd <- 5
n <- 16
df <- n - 1

# Compute t statistic
t_statistic <- (sample_mean - mu) / (sample_sd / sqrt(n))

# Critical t-value for two-tailed test, alpha = 0.05
t_critical <- qt(0.975, df)

# Conclusion
within_range <- abs(t_statistic) <= t_critical
within_range  # TRUE if within range, FALSE if outside
