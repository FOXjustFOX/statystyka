# lista11

# Global setup (alpha)
alpha <- 0.05

# Load data for tasks 3, 4, 5
# Use file.choose() if running interactively, or specify the path
# data <- read.csv2(file.choose(), text = gsub(",", ".", readLines(file.choose())), sep = ";")
# Assuming the file is in the same directory or using the original method with text connection:
tryCatch({
  data <- read.csv2(text = gsub(",", ".", readLines("dane/mieszkania.csv")), sep = ";")
}, error = function(e) {
  message("Could not read 'mieszkania.csv'. Please ensure the file is in the correct directory or provide the full path.")
  message("Error details: ", e$message)
  data <- NULL # Set data to NULL if file reading fails
})


#---------
# zadanie 1
#---------

# a) oczekiwana frekwencja , kostka jest symetryczna
observed <- c(171, 200, 168, 213, 226, 222)
n <- sum(observed)
expected_1a <- rep(n/6, 6)

# b) realizacja statyski testowej , test zgodnosci
chi_stat_1b <- sum((observed - expected_1a)^2 / expected_1a)

# c) wyznaczanie p
df_1c <- length(observed) - 1
p_value_1c <- 1 - pchisq(chi_stat_1b, df_1c)

# d) wniosek
wniosek_1d <- ifelse(p_value_1c < alpha, "Odrzucamy H0: Kostka nie jest symetryczna.", "Brak podstaw do odrzucenia H0: Kostka jest symetryczna.")

# e) chisq.test
result_1e <- chisq.test(observed, p = rep(1/6, 6))
stat_1e <- result_1e$statistic
p_value_1e <- result_1e$p.value

cat("------------------\n")
cat("zadanie 1\n")
cat("------------------\n")

cat("a) Oczekiwane frekwencje:\n", expected_1a, "\n")
cat("b) Statystyka testowa (Chi-kwadrat):\n", chi_stat_1b, "\n")
cat("c) Wartość p:\n", p_value_1c, "\n")
cat("d) Wniosek:\n", wniosek_1d, "\n")
cat("e) Wyniki chisq.test:\nStatystyka =", stat_1e, "\np-value =", p_value_1e, "\n")
cat("\n")


#---------
# zadanie 2
#---------

# a) oczekiwana frekwencja , niezaleznosc cech
observed_matrix <- matrix(c(200, 300, 150, 350), nrow = 2, byrow = TRUE)
expected_2a <- outer(rowSums(observed_matrix), colSums(observed_matrix)) / sum(observed_matrix)

# b) realizacja statyski testowej , test niezależnosci Pearsona
chi_stat_2b <- sum((observed_matrix - expected_2a)^2 / expected_2a)

# c) wartosc p
df_2c <- (nrow(observed_matrix)-1)*(ncol(observed_matrix)-1)
p_value_2c <- 1 - pchisq(chi_stat_2b, df_2c)

# d) wniosek
wniosek_2d <- ifelse(p_value_2c < alpha, "Odrzucamy H0: Istnieje zależność między płcią a wykształceniem.", "Brak podstaw do odrzucenia H0: Cechy są niezależne.")

# e) chisq.test
result_2e <- chisq.test(observed_matrix)
stat_2e <- result_2e$statistic
p_value_2e <- result_2e$p.value

# f) dokladny test Fishera  (fisher.test)
fisher_result_2f <- fisher.test(observed_matrix)
p_value_2f <- fisher_result_2f$p.value

cat("------------------\n")
cat(" zadanie 2 \n")
cat("------------------\n")

cat("a) Oczekiwane frekwencje (macierz):\n")
print(expected_2a)
cat("\nb) Statystyka testowa (Chi-kwadrat):\n", chi_stat_2b, "\n")
cat("c) Wartość p:\n", p_value_2c, "\n")
cat("d) Wniosek:\n", wniosek_2d, "\n")
cat("e) Wyniki chisq.test:\nStatystyka =", stat_2e, "\np-value =", p_value_2e, "\n")
cat("f) Wynik Fisher's Exact Test (p-value):\n", p_value_2f, "\n")
cat("\n")


#---------
# zadanie 3
#---------

if (!is.null(data)) {
  # a) rozklad liczby pokoi w zaleznosci od dzielnicy
  table_3a <- table(data$Dzielnica, data$Pokoje)

  # b) jakas smieszna zmienna
  # Stworzenie zmiennej nowa_pokoje: 4 lub więcej pokoi grupowane jako 4
  data$nowa_pokoje <- ifelse(data$Pokoje >= 4, 4, data$Pokoje)

  # c) liczba pokoi niezalezna od dzielnicy (test na nowej zmiennej)
  # Sprawdzenie czy tabela ma wystarczająco danych (unikalne dzielnice i pokoje)
  # Chi-kwadrat wymaga co najmniej 2 wierszy i 2 kolumn w tabeli obserwacji
  table_3c <- table(data$Dzielnica, data$nowa_pokoje)

cat("------------------\n")
cat(" zadanie 3 \n")
cat("------------------\n")
  cat("a) Rozkład liczby pokoi w zależności od dzielnicy:\n")
  print(table_3a)
  cat("\nb) Stworzono zmienną 'nowa_pokoje', grupując >=4 pokoje jako 4.\n")

  if (nrow(table_3c) > 1 && ncol(table_3c) > 1 && all(colSums(table_3c) > 0) && all(rowSums(table_3c) > 0) && sum(table_3c) > 0) {
     chi3 <- chisq.test(table_3c)
     cat("c) Wyniki testu niezależności (nowa_pokoje vs Dzielnica):\nStatystyka =", chi3$statistic, "\np-value =", chi3$p.value, "\n")
  } else {
     cat("c) Nie można wykonać testu chi-kwadrat. Tabela obserwacji dla testu niezależności nie spełnia wymagań (np. za mało kategorii).\n")
     # Print the table that caused the issue for debugging
     # print(table_3c)
  }
  cat("\n")
} else {
  cat("------------------\n")
cat(" zadanie 3 \n")
cat("------------------\n")
  cat("Nie można wykonać zadania 3, ponieważ plik 'mieszkania.csv' nie został wczytany.\n\n")
}


#---------
# zadanie 4
#---------

if (!is.null(data)) {
  # a) zmienna która wskazuje czy cena za m^2 > 6000
  data$cena_m2 <- data$Cena / data$Metraz
  data_clean <- data[!is.infinite(data$cena_m2) & !is.nan(data$cena_m2), ] # Usuń wartości niepoprawne (Inf i NaN)
  data_clean$cena_wysoka <- ifelse(data_clean$cena_m2 > 6000, "Tak", "Nie")
  data_clean <- data_clean[!is.na(data_clean$cena_wysoka), ] # Usuń NA, jeśli by się pojawiły

  # b) czy cena za m^2 jest zalezna od dzielnicy
  table_4b <- table(data_clean$Dzielnica, data_clean$cena_wysoka)

  cat("------------------\n")
cat(" zadanie 4 \n")
cat("------------------\n")
  cat("a) Stworzono zmienną 'cena_wysoka' (Tak/Nie, czy cena za m² > 6000).\n")

  if (nrow(table_4b) > 1 && ncol(table_4b) > 1 && all(colSums(table_4b) > 0) && all(rowSums(table_4b) > 0) && sum(table_4b) > 0) {
      chi4 <- chisq.test(table_4b)
      cat("b) Wyniki testu niezależności (cena_wysoka vs Dzielnica):\nStatystyka =", chi4$statistic, "\np-value =", chi4$p.value, "\n")
  } else {
      cat("b) Nie można wykonać testu chi-kwadrat. Tabela obserwacji dla testu niezależności nie spełnia wymagań (np. za mało kategorii, brak obserwacji w niektórych kategoriach).\n")
      # print(table_4b) # Useful for debugging
  }
  cat("\n")
} else {
   cat("--------- zadanie 4 ---------\n")
   cat("Nie można wykonać zadania 4, ponieważ plik 'mieszkania.csv' nie został wczytany.\n\n")
}


#---------
# zadanie 5
#---------

if (!is.null(data)) {
  # ensure cena_m2 is clean
  data_clean_5 <- data[!is.infinite(data$cena_m2) & !is.nan(data$cena_m2), ]
  cena_m2_clean <- na.omit(data_clean_5$cena_m2)

  cat("------------------\n")
cat(" zadanie 5 \n")
cat("------------------\n")


  # a) Test normalności dla ceny za m²
  if (length(cena_m2_clean) >= 3) {
    shapiro_test_cena_5a <- shapiro.test(cena_m2_clean)
    cat("a) Test normalności Shapiro-Wilka dla ceny za m²:\np-value =", shapiro_test_cena_5a$p.value, "\n")
    # estymator gęstości (plot)
    plot(density(cena_m2_clean), main = "Estymator gęstości dla ceny za m²", xlab = "Cena za m²", col = "blue")
    abline(v = mean(cena_m2_clean), col = "red", lty = 2)
  } else {
    cat("a) Za mało danych do wykonania testu Shapiro-Wilka i rysowania estymatora gęstości dla ceny za m² (wymagane min. 3 obserwacje).\n")
  }
  cat("\n")

  #b) Test normalności dla metrażu mieszkań w Śródmieściu
  srodmiescie_data_5b <- subset(data, Dzielnica == "Śródmieście")
  srodmiescie_metraz_5b <- na.omit(srodmiescie_data_5b$Metraz)

  if (length(srodmiescie_metraz_5b) >= 3) {
    shapiro_test_metraz_5b <- shapiro.test(srodmiescie_metraz_5b)
    cat("b) Test normalności Shapiro-Wilka dla metrażu w Śródmieściu:\np-value =", shapiro_test_metraz_5b$p.value, "\n")

    # Estymator gęstości (plot)
    plot(density(srodmiescie_metraz_5b),
         main = "Estymator gęstości dla metrażu w Śródmieściu",
         xlab = "Metraż",
         col = "darkgreen")
    abline(v = mean(srodmiescie_metraz_5b), col = "red", lty = 2)
    cat("   (Wygenerowano wykres estymatora gęstości dla metrażu w Śródmieściu)\n")

  } else {
    cat("b) Za mało danych do wykonania testu Shapiro-Wilka i rysowania estymatora gęstości dla metrażu w Śródmieściu (wymagane min. 3 obserwacje).\n")
  }
  cat("\n")

} else {
  cat("------------------\n")
cat(" zadanie 5 \n")
cat("------------------\n")
  cat("Nie można wykonać zadania 5, ponieważ plik 'mieszkania.csv' nie został wczytany.\n\n")
}


#---------
# zadanie 6
#---------

set.seed(123) # Ustawienie ziarna dla powtarzalności wyników losowych

# a) 1000 realizacji z rozkładu wykladniczego o wartosci oczekiwanej 1 (czyli rate=1)
exp_sample_6a <- rexp(1000, rate = 1)

# b) Testy zgodności (Kolmogorowa-Smirnowa)
# i) rozkad normalny o sredniej 1 odchylenie 1
ks_normal_6bi <- ks.test(exp_sample_6a, "pnorm", mean = 1, sd = 1)
p_value_6bi <- ks_normal_6bi$p.value

# ii) rozklad wykladniczy lambda=1
ks_exp_6bii <- ks.test(exp_sample_6a, "pexp", rate = 1)
p_value_6bii <- ks_exp_6bii$p.value

# c) 1000 realizacji Gamma ,ksztalt 100 , skala 1 (czyli rate=1/scale=1)
# Srednia rozkladu Gamma to shape/rate = 100/1 = 100.
# Odchylenie standardowe to sqrt(shape)/rate = sqrt(100)/1 = 10.
gamma_sample_6c <- rgamma(1000, shape = 100, rate = 1)

# d) Testy zgodności (Kolmogorowa-Smirnowa) dla próbki Gamma
# i) rozkalad normalny , srednia 100 , odchylenie 10
ks_normal_gamma_6di <- ks.test(gamma_sample_6c, "pnorm", mean = 100, sd = 10)
p_value_6di <- ks_normal_gamma_6di$p.value

# ii) rozklad Gamma , parametry 100 (ksztalt), 1 (rate)
ks_gamma_6dii <- ks.test(gamma_sample_6c, "pgamma", shape = 100, rate = 1)
p_value_6dii <- ks_gamma_6dii$p.value

cat("------------------\n")
cat(" zadanie 6 \n")
cat("------------------\n")

cat("a) Wygenerowano 1000 realizacji z rozkładu wykładniczego (rate=1).\n")
cat("   Pierwsze 10 elementów:", head(exp_sample_6a, 10), "...\n")
cat("b) Testy zgodności dla próbki wykładniczej:\n")
cat("   i) Wartość p (vs N(1,1)):", p_value_6bi, "\n")
cat("   ii) Wartość p (vs Exp(1)):", p_value_6bii, "\n")
cat("c) Wygenerowano 1000 realizacji z rozkładu Gamma (shape=100, rate=1).\n")
cat("   Pierwsze 10 elementów:", head(gamma_sample_6c, 10), "...\n")
cat("d) Testy zgodności dla próbki Gamma:\n")
cat("   i) Wartość p (vs N(100,10)):", p_value_6di, "\n")
cat("   ii) Wartość p (vs Gamma(100,1)):", p_value_6dii, "\n")
cat("\n")