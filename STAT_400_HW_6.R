# Problem 1: Two-sample t-test (Station 1 vs Station 2)
# Data for the two stations
station_1 <- c(5030, 13700, 10730, 11400, 860, 2200, 4250, 15040)
station_2 <- c(4980, 2800, 2810, 4670, 1330, 6890, 7720, 7030, 7330, 1130, 1690, 2190)

# Calculate statistics for Station 1
mean_1 <- mean(station_1)
std_1 <- sd(station_1)  # Sample standard deviation
n1 <- length(station_1)

# Calculate statistics for Station 2
mean_2 <- mean(station_2)
std_2 <- sd(station_2)  # Sample standard deviation
n2 <- length(station_2)

# Compute the test statistic (t-value)
numerator <- mean_1 - mean_2
denominator <- sqrt((std_1^2 / n1) + (std_2^2 / n2))
t_stat <- numerator / denominator

# Compute degrees of freedom (Welch-Satterthwaite formula)
df_numerator <- (std_1^2 / n1 + std_2^2 / n2)^2
df_denominator <- ((std_1^2 / n1)^2 / (n1 - 1)) + ((std_2^2 / n2)^2 / (n2 - 1))
degrees_of_freedom <- df_numerator / df_denominator

# Compute the p-value
p_value <- 2 * (1 - pt(abs(t_stat), df=degrees_of_freedom))
t_stat
degrees_of_freedom
p_value

# Problem 2: Sample size calculation for a t-test
# Known parameters
alpha <- 0.05
beta <- 0.1
sigma <- 1.25
delta <- 0.5

# Critical values
z_alpha_2 <- qnorm(1 - alpha / 2)  # Two-tailed test
z_beta <- qnorm(1 - beta)

# Required sample size formula
n <- ((z_alpha_2 + z_beta) * (sigma / delta))^2
z_alpha_2
z_beta
n

# Problem 3: One-tailed test for proportions
# Given data
n <- 15
x <- 8
p_hat <- x / n  # Sample proportion
p_0 <- 0.4  # Null hypothesis proportion

# Calculate the test statistic
z <- (p_hat - p_0) / sqrt(p_0 * (1 - p_0) / n)

# Compute the p-value for the one-tailed test
p_value <- 1 - pnorm(z)
z
p_value


