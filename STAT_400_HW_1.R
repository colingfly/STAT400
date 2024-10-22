#Question 1: 1.9
no_aging <- c(227,222,218,217,225,218,216,229,228,221)
plot(no_aging,
     main="Aging",
     ylab="Value")

no_aging_mean <- mean(no_aging)
no_aging_sq_diff <- (no_aging - no_aging_mean)^2
no_aging_numerator <- sum(no_aging_sq_diff)
no_aging_var <- var(no_aging)
no_aging_sd <- sd(no_aging)

aging <- c(219,214,215,211,209,218,203,204,201,205)
plot(no_aging,
     main="No Aging",
     ylab="Value")

aging_mean <- mean(aging)
aging_sq_diff <- (aging - aging_mean)^2
aging_numerator <- sum(aging_sq_diff)
aging_var <- var(aging)
aging_sd <- sd(aging)

#Question 2: 1.25
upper_level_income_pct <- c(72.2, 31.9, 26.5, 29.1, 27.3, 8.6, 22.3, 26.5, 20.4, 12.8,
                            25.1, 19.2, 24.1, 58.2, 68.1, 89.2, 55.1, 9.4, 14.5, 13.9,
                            20.7, 17.9, 8.5, 55.4, 38.1, 54.2, 21.5, 26.2, 59.1, 43.3)

#A
sum_ulip <- sum(upper_level_income_pct)
n <- length(upper_level_income_pct)
mean_ulip_manual <- (sum_ulip)/(n)
mean_ulip <- mean(upper_level_income_pct)

#B
sorted_ulip <- sort(upper_level_income_pct)
median_ulip_manual <- (sorted_ulip[n/2] + sorted_ulip[(n/2)+1])*0.5
median_ulip <- median(sorted_ulip)

#C:
hist(upper_level_income_pct,
     main='Histrogram of Upper Level % Income',
     xlab='Values',
     ylab='Frequency',
     col="lightblue",
     border='black')

#D
ten_p <- n*0.1
ulip_10_trimmed_mean <- tail(head(sorted_ulip, -ten_p), -ten_p)

#Mean
sum_ulip_10 <- sum(ulip_10_trimmed_mean)
n_10 <- length(ulip_10_trimmed_mean)
mean_ulip_10_manual <- (sum_ulip_10)/(n_10)
mean_ulip_10 <- mean(ulip_10_trimmed_mean)

#Median 
median_ulip_10_manual <- (ulip_10_trimmed_mean[n_10/2] + ulip_10_trimmed_mean[(n_10/2)+1])*0.5
median_ulip_10 <- median(ulip_10_trimmed_mean)

#Histogram
hist(ulip_10_trimmed_mean,
     main='Histrogram of Upper Level % Income - 10% Trimmed',
     xlab='Values',
     ylab='Frequency',
     col="lightblue",
     border='black')




