# Zadanie 1
# a) Losowanie 5000 realizacji z rozkładu jednostajnego na [0,1]
set.seed(123) # Ziarno dla reprodukowalności
a_unif <- runif(5000, 0, 1)

# b) Losowanie 3000 realizacji z rozkładu normalnego N(100, 15)
b_norm <- rnorm(3000, mean = 100, sd = 15)

# c) Estymacja gęstości
# i) Histogram
hist_unif <- hist(a_unif, breaks = 30, main = "Hist rozkładu jednostajnego")
hist_norm <- hist(b_norm, breaks = 30, main = "Hist rozkładu normalnego")

# ii) Estymator jądrowy
density_unif <- density(a_unif)
density_norm <- density(b_norm)

# Zadanie 2
# a) i) Symulacja 600 rzutów kostką
rzuty_a <- ceiling(runif(600) * 6)

# ii) Średnia i wariancja próbki
srednia_a <- mean(rzuty_a)
wariancja_a <- var(rzuty_a)
cat("Średnia (zad 2a):", srednia_a, "| Teoretyczna: 3.5\n")
cat("Wariancja (zad 2a):", wariancja_a, "| Teoretyczna:", 35 / 12, "\n")

# iii) Rozkład częstości
czestosci_a <- table(rzuty_a)

# iv) Konwersja do ramki danych i wariancja częstości
df_a <- as.data.frame(czestosci_a)
wariancja_czestosci <- var(df_a$Freq)
print("Ramka danych (zad 2a iv):")
print(df_a)
cat("Wariancja częstości (zad 2a iv):", wariancja_czestosci, "\n")

# Histogram rzutów kostką z zad 2a
barplot(czestosci_a, main="Histogram rzutów kostką (2a)", 
        xlab="Wynik", ylab="Częstość", col="lightblue")

# b) Symulacja 600 rzutów kostką z użyciem sample()
rzuty_b <- sample(1:6, 600, replace = TRUE)

# Histogram rzutów kostką z zad 2b
barplot(table(rzuty_b), main="Histogram rzutów kostką (2b)", 
        xlab="Wynik", ylab="Częstość", col="lightgreen")

# # Zadanie 3: Generowanie z rozkładu dyskretnego
prob <- c(0.15, 0.25, 0.5, 0.1)
x_dyskretne <- sample(0:3, 1000, replace = TRUE, prob = prob)

# Histogram rozkładu dyskretnego
barplot(table(x_dyskretne)/length(x_dyskretne), main="Histogram rozkładu dyskretnego", 
        xlab="Wartość", ylab="Częstość względna", col="coral")


# Zadanie 4
# i) Rozkład Bin(10, 0.3)
binomialne <- rbinom(100, size = 10, prob = 0.3)

# Histogram rozkładu dwumianowego
hist(binomialne, breaks=seq(-0.5, 10.5, by=1), prob=TRUE, 
     main="Histogram rozkładu Bin(10, 0.3)", 
     xlab="Liczba sukcesów", col="lightpink")


# ii) Rozkład Geom(0.4) (liczba prób do pierwszego sukcesu INKLUZYWNIE)
geometryczne <- rgeom(50, prob = 0.4) + 1

# Histogram rozkładu geometrycznego
hist(geometryczne, breaks=seq(0.5, max(geometryczne)+0.5, by=1), prob=TRUE, 
     main="Histogram rozkładu Geom(0.4)", 
     xlab="Liczba prób do sukcesu", col="lightcyan")


# Zadanie 5
# a) Metoda odwracania dystrybuanty
# Dystrybuanta: G(x) = 0.25x² → x = sqrt(4u)
losuj_odwracanie <- function(n) {
  u <- runif(n)
  return(sqrt(4 * u)) # Odwrócenie dystrybuanty
}
x_odwracanie <- losuj_odwracanie(200)

# Histogram metody odwracania dystrybuanty
hist(x_odwracanie, breaks=30, prob=TRUE, 
     main="Histogram metody odwracania dystrybuanty", 
     xlab="Wartość", col="wheat")


# b) Metoda przyjęcia-odrzucenia
losuj_odrzucanie <- function(n) {
  x_accept <- numeric(n)
  licznik <- 1
  while (licznik <= n) {
    x <- runif(1, 0, 2) # Kandydat z U[0,2]
    y <- runif(1) # Losowa liczba z U(0, 1)
    if (y <= 0.5 * x) { # g(x) = 0.5x ≤ 1 dla x ∈ [0,2]
      x_accept[licznik] <- x
      licznik <- licznik + 1
    }
  }
  return(x_accept)
}
x_odrzucanie <- losuj_odrzucanie(200)

# Histogram metody przyjęcia-odrzucenia
hist(x_odrzucanie, breaks=30, prob=TRUE, 
     main="Histogram metody przyjęcia-odrzucenia", 
     xlab="Wartość", col="plum")

