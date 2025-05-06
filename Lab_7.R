# Zadanie 1
# Wzrost X ~ N(mean = 170, sd = sqrt(144) = 12)

# a) Wyznaczenie prawdopodobieństw [cite: 2]
# i) P(X < 164)
pnorm(164, mean = 170, sd = 12)

# ii) P(X > 188)
1 - pnorm(188, mean = 170, sd = 12)
# lub
pnorm(188, mean = 170, sd = 12, lower.tail = FALSE)

# iii) P(158 < X < 185)
pnorm(185, mean = 170, sd = 12) - pnorm(158, mean = 170, sd = 12)

# b) Wyznaczenie wzrostu k, takiego że P(X > k) = 0.15 [cite: 2]
# P(X > k) = 0.15  <=> P(X <= k) = 1 - 0.15 = 0.85
qnorm(0.85, mean = 170, sd = 12)
# lub
qnorm(0.15, mean = 170, sd = 12, lower.tail = FALSE)


# Zadanie 2
# Generowanie liczb z rozkładu normalnego standardowego (metoda Boxa-Mullera)

# i) Wygenerowanie 10000 realizacji Z [cite: 3]
n_sim <- 10000
u1 <- runif(n_sim)
u2 <- runif(n_sim)
z <- cos(2 * pi * u1) * sqrt(-2 * log(u2))

# ii) Estymator jądrowy gęstości dla Z i porównanie z gęstością N(0,1) [cite: 4]
plot(density(z), main = "Estymator jądrowy gęstości dla Z vs N(0,1)", lwd = 2, col = "blue")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Estymator jądrowy", "Gęstość N(0,1)"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# iii) Niech Y = 100 + 15Z [cite: 4]
y <- 100 + 15 * z

# Estymator jądrowy gęstości dla Y i porównanie z gęstością N(100, 15) [cite: 5]
plot(density(y), main = "Estymator jądrowy gęstości dla Y vs N(100,15)", lwd = 2, col = "blue", xlim=c(40, 160))
curve(dnorm(x, mean = 100, sd = 15), add = TRUE, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Estymator jądrowy", "Gęstość N(100,15)"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)


# Zadanie 3
# Rozkład normalny standardowy

# Parametry rozkładu X ~ N(mean = 170, sd = 12)
mean_x <- 170
sd_x <- 12

# Funkcja do generowania, standaryzacji i rysowania
simulate_and_plot_Z <- function(n_samples) {
  # i) Generowanie realizacji z rozkładu X [cite: 6]
  x_samples <- rnorm(n_samples, mean = mean_x, sd = sd_x)
  
  # ii) Standaryzacja Z = (X - 170) / 12 [cite: 7]
  z_samples <- (x_samples - mean_x) / sd_x
  
  # Estymator jądrowy gęstości dla Z i porównanie z gęstością N(0,1) [cite: 7]
  plot(density(z_samples), main = paste("Estymator gęstości dla Z vs N(0,1) | n =", n_samples),
       lwd = 2, col = "blue", xlim = c(-4, 4))
  curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lty = 2, lwd = 2)
  legend("topright", legend = c("Estymator jądrowy", "Gęstość N(0,1)"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)
}

# Wywołanie dla n = 2000, 500, 100 [cite: 7]
par(mfrow = c(1, 3)) # Układ wykresów 1x3
simulate_and_plot_Z(2000)
simulate_and_plot_Z(500)
simulate_and_plot_Z(100)
par(mfrow = c(1, 1)) # Przywrócenie domyślnego układu wykresów


# Zadanie 4
# Centralne Twierdzenie Graniczne (CTG)

# Parametry rozkładu wykładniczego X_i ~ Exp(lambda = 0.5) [cite: 8]
lambda <- 0.5
mu <- 1 / lambda # Wartość oczekiwana X_i
sigma <- 1 / lambda # Odchylenie standardowe X_i

# Liczba realizacji do wygenerowania dla każdej sumy S_n
n_realizations <- 1000

# Rozmiary prób n
n_values <- c(1, 20, 200)

# Ustawienie układu wykresów
par(mfrow = c(1, length(n_values)))

# Pętla po różnych wartościach n
for (n in n_values) {
  # Generowanie 1000 realizacji S_n = X_1 + ... + X_n [cite: 8]
  # Każda kolumna macierzy `samples` to jedna realizacja (X_1, ..., X_n)
  samples <- matrix(rexp(n_realizations * n, rate = lambda), nrow = n, ncol = n_realizations)
  # Sumowanie kolumn daje realizacje S_n
  s_n <- colSums(samples)
  
  # Obliczenie wartości oczekiwanej i odchylenia standardowego S_n
  e_s_n <- n * mu
  sd_s_n <- sqrt(n) * sigma
  
  # Obliczenie Z_n = (S_n - E(S_n)) / sd(S_n) [cite: 8]
  z_n <- (s_n - e_s_n) / sd_s_n
  
  # ii) Porównanie estymatora jądrowego dla Z_n z gęstością N(0,1) [cite: 9]
  plot(density(z_n), main = paste("Estymator gęstości Z_n vs N(0,1) | n =", n),
       lwd = 2, col = "blue", xlim = c(-4, 4))
  curve(dnorm(x, 0, 1), add = TRUE, col = "red", lty = 2, lwd = 2)
  legend("topright", legend = c("Estymator jądrowy Z_n", "Gęstość N(0,1)"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2, cex=0.7)
}

# Przywrócenie domyślnego układu wykresów
par(mfrow = c(1, 1))


# Zadanie 5
# Przybliżenie rozkładu dwumianowego rozkładem normalnym

# Liczba realizacji
n_binom_realizations <- 10000

# Parametry do przetestowania [cite: 9]
params <- list(
  a = list(n = 30, p = 0.5),
  b = list(n = 1000, p = 0.5),
  c = list(n = 30, p = 0.05),
  d = list(n = 1000, p = 0.05)
)

# Ustawienie układu wykresów
par(mfrow = c(2, 2))

# Pętla po parametrach
for (case_name in names(params)) {
  n <- params[[case_name]]$n
  p <- params[[case_name]]$p
  
  # Generowanie realizacji z rozkładu Bin(n, p) [cite: 9]
  binom_samples <- rbinom(n_binom_realizations, size = n, prob = p)
  
  # Obliczenie średniej i odchylenia standardowego dla przybliżenia normalnego [cite: 10]
  mu_norm <- n * p
  sd_norm <- sqrt(n * p * (1 - p))
  
  # Tworzenie histogramu relatywnych frekwencji
  # Używamy breaks, aby słupki były wyśrodkowane na wartościach całkowitych
  hist_info <- hist(binom_samples,
                    breaks = seq(min(binom_samples) - 0.5, max(binom_samples) + 0.5, by = 1),
                    freq = FALSE, # Chcemy gęstość (relatywne frekwencje)
                    main = paste("Bin(", n, ", ", p, ") vs N(", round(mu_norm, 2), ", ", round(sd_norm, 2), ")", sep = ""),
                    xlab = "x",
                    ylab = "Gęstość / Relatywna frekwencja",
                    col = "lightblue")
  
  # Dodanie krzywej gęstości rozkładu normalnego [cite: 10]
  curve(dnorm(x, mean = mu_norm, sd = sd_norm),
        add = TRUE,
        col = "red",
        lwd = 2)
  
  legend("topright", legend = c("Relatywne frekwencje", "Przybliżenie normalne"),
         fill = c("lightblue", NA), border=c("black", NA), lty = c(NA, 1), lwd=c(NA, 2), col = c(NA, "red"), cex=0.7)
  
}

# Przywrócenie domyślnego układu wykresów
par(mfrow = c(1, 1))