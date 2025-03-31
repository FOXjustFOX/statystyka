# Zadanie 1: Rozkład dwumianowy dla rzutu monetą 6 razy
n1 <- 6
p1 <- 0.5

# P(X = 5)
prob_1_1 <- dbinom(5, n1, p1)
# P(X >= 3)
prob_1_2 <- pbinom(3,n1,p1)
# P(2 ≤ X ≤ 4)
prob_1_3 <- pbinom(4, n1, p1) - pbinom(2, n1, p1)

# Wykres rozkładu X
x1 <- 0:n1
y1 <- dbinom(x1, n1, p1)
plot(x1, y1, type = "h", lwd = 2, col = "blue", main = "Rozkład liczby reszek w 6 rzutach monetą", xlab = "Liczba reszek (X)", ylab = "Prawdopodobieństwo") # nolint: line_length_linter.

# Zadanie 2: Rozkład Poissona dla sprzedaży samochodów
lambda_2 <- 3 * 2

# P(X = 5)
prob_2_1 <- dpois(5, lambda_2)
# P(X ≥ 4)
prob_2_2 <- ppois(3, lambda_2)
# P(3 ≤ X ≤ 5)
prob_2_3 <- ppois(5, lambda_2) - ppois(2, lambda_2)

# Wykres rozkładu Y
x2 <- 0:30
y2 <- dpois(x2, lambda_2)
plot(x2, y2, type = "h", lwd = 2, col = "red", main = "Rozkład liczby sprzedanych samochodów", xlab = "Liczba sprzedanych samochodów (Y)", ylab = "Prawdopodobieństwo") # nolint: line_length_linter.


# Zadanie 3: Średnia i wariancja 

x <- c(1,2,3,4)
p <- c(0.2, 0.4, 0.3, 0.1)

e_x <- sum(x * p)  # Średnia E(X)
var_x <- sum((x - e_x)^2 * p)  # Wariancja Var(X)


# Zadanie 6: Wykresy rozkładów X i Y
x6 <- 0:10
# p_x6 <- c(0.2, 0.4, 0.3, 0.1, rep(0, 7))
p_x6 <- dbinom(x6, 10, 0.02)

y6 <- 0:10
p_y6 <- dpois(y6, 2)

# Wykres rozkładów X 
plot(x6, p_x6, type="h", lwd=2, col="blue", main="Porównanie rozkładów X i Y", xlab="Wartość zmiennej losowej", ylab="Prawdopodobieństwo")
lines(y6, p_y6, type="h", lwd=2, col="red")
legend("topright", legend=c("X", "Y"), col=c("blue", "red"), lwd=3)

