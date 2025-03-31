# Lab 4: Continuous Probability Distributions
# This script solves various probability distribution problems

# Task 1: Uniform Distribution [4, 12]
# X ~ U(4, 12) # nolint
# Parameters: a = 4, b = 12

# i) P(X < 7)
# Using punif(x, min, max)
p1_i <- punif(7, min = 4, max = 12)
cat("1(i): P(X < 7) =", p1_i, "\n")

# ii) P(5 < X < 11)
# P(5 < X < 11) = P(X < 11) - P(X < 5)
p1_ii <- punif(11, min = 4, max = 12) - punif(5, min = 4, max = 12)
cat("1(ii): P(5 < X < 11) =", p1_ii, "\n")

# iii) P(X > 10)
# P(X > 10) = 1 - P(X <= 10) # nolint
p1_iii <- 1 - punif(10, min = 4, max = 12)
cat(" 1(iii): P(X > 10) =", p1_iii, "\n")

# iv)  x taki P(X > x) = 0.6
# P(X > x) = 0.6 means P(X <= x) = 0.4
x1_iv <- qunif(0.4, min = 4, max = 12)
cat("Task 1(iv): x such that P(X > x) = 0.6 is", x1_iv, "\n")

# Task 2: exp dla telefonow
# sekundy na minuty

# i) P(T > 30s) = P(T > 0.5min)
p2_i <- 1 - pexp(0.5, rate = 4)
cat("Task 2(i): P(T > 30s) =", p2_i, "\n")

# ii) P(T < 20s) = P(T < 1/3 min)
p2_ii <- pexp(1 / 3, rate = 4)
cat("Task 2(ii): P(T < 20s) =", p2_ii, "\n")

# iii) P(40s < T < 80s) = P(2/3 < T < 4/3)
p2_iii <- pexp(4 / 3, rate = 4) - pexp(2 / 3, rate = 4)
cat("Task 2(iii): P(40s < T < 80s) =", p2_iii, "\n")

# iv) t takie ze P(T > t) = 0.2
# P(T > t) = 0.2 to P(T <= t) = 0.8
t2_iv <- qexp(0.8, rate = 4)
cat("Task 2(iv): t takie ze P(T > t) = 0.2 is", t2_iv, "minut\n")

# v) Plot density function of T
t_values <- seq(0, 3, by = 0.01)
density_values <- dexp(t_values, rate = 4)
plot(t_values, density_values, type = "l",
     xlab = "Time (minutes)", ylab = "Density",
     main = "Density Function of Time Between Calls")

# Task 3: exp dla awarii urzadzen
# T ~ Exp(λ = 1/3 per year)

# i) P(T > 2)
p3_i <- 1 - pexp(2, rate = 1 / 3)
cat("Task 3(i): P(T > 2 lat) =", p3_i, "\n")

# ii) P(T < 4)
p3_ii <- pexp(4, rate = 1 / 3)
cat("Task 3(ii): P(T < 4 lat) =", p3_ii, "\n")

# iii) P(3 < T < 5)
p3_iii <- pexp(5, rate = 1 / 3) - pexp(3, rate = 1 / 3)
cat("Task 3(iii): P(3 < T < 5 lat) =", p3_iii, "\n")

# iv) Find t such that P(T < t) = 0.4
t3_iv <- qexp(0.4, rate = 1 / 3)
cat("Task 3(iv): t takie ze P(T < t) = 0.4 is", t3_iv, "lat\n")

# Task 4:
# X ~ N(μ = 170cm, σ = 12cm)

# dnorm(x, mean, sd) wysokosc krzywej w punkcie x
height_at_170 <- dnorm(170, mean = 170, sd = 12)
cat("Wysokosc krzywej w punkcie x = 170cm:", height_at_170, "\n")

# pnorm(x, mean, sd) daje P(X ≤ x)

# i) P(X > 180)
#  1 - P(X ≤ 180)
p4_i <- 1 - pnorm(180, mean = 170, sd = 12)
cat("Task 4(i): P(X > 180cm) =", p4_i, "\n")

# ii) P(X < 165)
p4_ii <- pnorm(165, mean = 170, sd = 12)
cat("Task 4(ii): P(X < 165cm) =", p4_ii, "\n")

# iii) P(155 < X < 190)
# This is P(X < 190) - P(X < 155)
p4_iii <- pnorm(190, mean = 170, sd = 12) - pnorm(155, mean = 170, sd = 12)
cat("Task 4(iii): P(155 < X < 190cm) =", p4_iii, "\n")

# qnorm - odwrotna dystrybuanta normalna
# qnorm(p, mean, sd) daje x takie ze P(X ≤ x) = p

# Create a sequence of x values
x_values <- seq(130, 210, by = 0.1)
# Calculate density values using dnorm
density_values <- dnorm(x_values, mean = 170, sd = 12)

# # Create the plot
plot(x_values, density_values, type = "l",
     xlab = "Height (cm)", ylab = "Density",
     main = "Normal Distribution of Student Heights",
     col = "blue")
