# --- Zadanie 1 ---
print("Zadanie 1")

# Dane wejściowe dla 6 studentów
analiza <- c(28, 18, 23, 26, 14, 12)
algebra <- c(20, 24, 25, 27, 16, 13)

# a) Wyznaczenie współczynników korelacji
wsp_pearson_zad1 <- cor(analiza, algebra, method = "pearson")
wsp_spearman_zad1 <- cor(analiza, algebra, method = "spearman")
wsp_kendall_zad1 <- cor(analiza, algebra, method = "kendall")

print("a) Współczynniki korelacji:")
print(paste("Pearson:", round(wsp_pearson_zad1, 4)))
print(paste("Spearman:", round(wsp_spearman_zad1, 4)))
print(paste("Kendall:", round(wsp_kendall_zad1, 4)))


# b) Weryfikacja hipotezy o nieskorelowaniu wyników
print("b) Testy hipotezy o braku korelacji (H0: rho = 0), alfa = 0.05:")

test_pearson_zad1 <- cor.test(analiza, algebra, method = "pearson")
print("Test Pearsona:")
print(test_pearson_zad1)
if (test_pearson_zad1$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Pearson).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Pearson).")
}

test_spearman_zad1 <- cor.test(analiza, algebra, method = "spearman")
print("Test Spearmana:")
print(test_spearman_zad1)
if (test_spearman_zad1$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Spearman).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Spearman).")
}

test_kendall_zad1 <- cor.test(analiza, algebra, method = "kendall")
print("Test Kendalla:")
print(test_kendall_zad1)
if (test_kendall_zad1$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Kendall).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Kendall).")
}


# --- Zadanie 2 ---
print("Zadanie 2")
set.seed(123) # Dla reproducibility

n_zad2 <- 100
X_zad2 <- rnorm(n_zad2)
Y_zad2 <- rnorm(n_zad2)
dane_zad2 <- data.frame(X = X_zad2, Y = Y_zad2)

# a) Obliczenie współczynników korelacji
wsp_pearson_zad2 <- cor(dane_zad2$X, dane_zad2$Y, method = "pearson")
wsp_spearman_zad2 <- cor(dane_zad2$X, dane_zad2$Y, method = "spearman")
wsp_kendall_zad2 <- cor(dane_zad2$X, dane_zad2$Y, method = "kendall")

print("a) Współczynniki korelacji dla wygenerowanych danych (X,Y):")
print(paste("Pearson:", round(wsp_pearson_zad2, 4)))
print(paste("Spearman:", round(wsp_spearman_zad2, 4)))
print(paste("Kendall:", round(wsp_kendall_zad2, 4)))


# b) Testowanie hipotezy rho=0
print("b) Testy hipotezy rho=0 dla (X,Y), alfa = 0.05:")
test_p_zad2 <- cor.test(dane_zad2$X, dane_zad2$Y, method = "pearson")
print("Test Pearsona dla (X,Y):")
print(test_p_zad2)
if (test_p_zad2$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Pearson).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Pearson).")
}

test_s_zad2 <- cor.test(dane_zad2$X, dane_zad2$Y, method = "spearman", exact = FALSE) # exact=FALSE by uniknąć ostrzeżeń o remisach dla ciągłych danych
print("Test Spearmana dla (X,Y):")
print(test_s_zad2)
if (test_s_zad2$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Spearman).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Spearman).")
}

test_k_zad2 <- cor.test(dane_zad2$X, dane_zad2$Y, method = "kendall", exact = FALSE) # exact=FALSE by uniknąć ostrzeżeń
print("Test Kendalla dla (X,Y):")
print(test_k_zad2)
if (test_k_zad2$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Kendall).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Kendall).")
}


# c) Przedziały ufności dla współczynników korelacji
print("c) Przedziały ufności (metody standardowe):")
ci_pearson_zad2 <- test_p_zad2$conf.int
print(paste("95% przedział ufności dla Pearson rho (X,Y): [", round(ci_pearson_zad2[1], 4), ",", round(ci_pearson_zad2[2], 4), "]"))
print("Dla Spearmana i Kendalla, cor.test standardowo nie zwraca przedziałów ufności (użyj bootstrap - patrz pkt d).")


# d) Bootstrapowe przedziały ufności
print("d) Bootstrapowe przedziały ufności (95%):")
num_bootstrap_samples <- 1000
calculate_bootstrap_ci <- function(data_x, data_y, num_samples, method_type) {
  bootstrap_correlations <- numeric(num_samples)
  n_obs <- length(data_x)
  for (i in 1:num_samples) {
    sample_indices <- sample(1:n_obs, n_obs, replace = TRUE)
    bootstrap_sample_x <- data_x[sample_indices]
    bootstrap_sample_y <- data_y[sample_indices]
    bootstrap_correlations[i] <- cor(bootstrap_sample_x, bootstrap_sample_y, method = method_type)
  }
  ci_lower <- quantile(bootstrap_correlations, 0.025, na.rm = TRUE)
  ci_upper <- quantile(bootstrap_correlations, 0.975, na.rm = TRUE)
  return(c(ci_lower, ci_upper))
}

ci_bootstrap_pearson_zad2 <- calculate_bootstrap_ci(dane_zad2$X, dane_zad2$Y, num_bootstrap_samples, "pearson")
print(paste("Bootstrap CI dla Pearson rho (X,Y): [", round(ci_bootstrap_pearson_zad2[1], 4), ",", round(ci_bootstrap_pearson_zad2[2], 4), "]"))

ci_bootstrap_spearman_zad2 <- calculate_bootstrap_ci(dane_zad2$X, dane_zad2$Y, num_bootstrap_samples, "spearman")
print(paste("Bootstrap CI dla Spearman rho (X,Y): [", round(ci_bootstrap_spearman_zad2[1], 4), ",", round(ci_bootstrap_spearman_zad2[2], 4), "]"))

ci_bootstrap_kendall_zad2 <- calculate_bootstrap_ci(dane_zad2$X, dane_zad2$Y, num_bootstrap_samples, "kendall")
print(paste("Bootstrap CI dla Kendall tau (X,Y): [", round(ci_bootstrap_kendall_zad2[1], 4), ",", round(ci_bootstrap_kendall_zad2[2], 4), "]"))


# e) Wykres rozrzutu zmiennych X i Y
print("e) Wykres rozrzutu dla (X,Y) zostanie wyświetlony.")
plot(dane_zad2$X, dane_zad2$Y, main="Rozrzut zmiennych X i Y (Zad. 2)", xlab="X", ylab="Y", pch=19)
abline(lsfit(dane_zad2$X, dane_zad2$Y), col="red")


# --- Zadanie 3 ---
print("Zadanie 3")
set.seed(456)

n_zad3 <- 100
X_zad3 <- rnorm(n_zad3)
Y_indep_zad3 <- rnorm(n_zad3)
V_zad3 <- 0.2 * X_zad3 + sqrt(0.96) * Y_indep_zad3
dane_zad3 <- data.frame(X = X_zad3, V = V_zad3)

# a) Obliczenie współczynników korelacji dla (X,V)
wsp_pearson_zad3 <- cor(dane_zad3$X, dane_zad3$V, method = "pearson")
wsp_spearman_zad3 <- cor(dane_zad3$X, dane_zad3$V, method = "spearman")
wsp_kendall_zad3 <- cor(dane_zad3$X, dane_zad3$V, method = "kendall")

print("a) Współczynniki korelacji dla wygenerowanych danych (X,V):")
print(paste("Pearson:", round(wsp_pearson_zad3, 4)))
print(paste("Spearman:", round(wsp_spearman_zad3, 4)))
print(paste("Kendall:", round(wsp_kendall_zad3, 4)))
print(paste("Teoretyczna korelacja Pearsona między X i V powinna wynosić 0.2"))


# b) Testowanie hipotezy rho=0 dla (X,V)
print("b) Testy hipotezy rho=0 dla (X,V), alfa = 0.05:")
test_p_zad3 <- cor.test(dane_zad3$X, dane_zad3$V, method = "pearson")
print("Test Pearsona dla (X,V):")
print(test_p_zad3)
if (test_p_zad3$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Pearson).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Pearson).")
}

test_s_zad3 <- cor.test(dane_zad3$X, dane_zad3$V, method = "spearman", exact = FALSE)
print("Test Spearmana dla (X,V):")
print(test_s_zad3)
if (test_s_zad3$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Spearman).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Spearman).")
}

test_k_zad3 <- cor.test(dane_zad3$X, dane_zad3$V, method = "kendall", exact = FALSE)
print("Test Kendalla dla (X,V):")
print(test_k_zad3)
if (test_k_zad3$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Kendall).")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Kendall).")
}


# c) Przedziały ufności dla (X,V) (metody standardowe)
print("c) Przedziały ufności dla (X,V) (metody standardowe):")
ci_pearson_zad3 <- test_p_zad3$conf.int
print(paste("95% przedział ufności dla Pearson rho (X,V): [", round(ci_pearson_zad3[1], 4), ",", round(ci_pearson_zad3[2], 4), "]"))
#      #print("Dla Spearmana i Kendalla, cor.test standardowo nie zwraca przedziałów ufności.")


# d) Bootstrapowe przedziały ufności dla (X,V)
print("d) Bootstrapowe przedziały ufności dla (X,V) (95%):")
ci_bootstrap_pearson_zad3 <- calculate_bootstrap_ci(dane_zad3$X, dane_zad3$V, num_bootstrap_samples, "pearson")
print(paste("Bootstrap CI dla Pearson rho (X,V): [", round(ci_bootstrap_pearson_zad3[1], 4), ",", round(ci_bootstrap_pearson_zad3[2], 4), "]"))

ci_bootstrap_spearman_zad3 <- calculate_bootstrap_ci(dane_zad3$X, dane_zad3$V, num_bootstrap_samples, "spearman")
print(paste("Bootstrap CI dla Spearman rho (X,V): [", round(ci_bootstrap_spearman_zad3[1], 4), ",", round(ci_bootstrap_spearman_zad3[2], 4), "]"))

ci_bootstrap_kendall_zad3 <- calculate_bootstrap_ci(dane_zad3$X, dane_zad3$V, num_bootstrap_samples, "kendall")
print(paste("Bootstrap CI dla Kendall tau (X,V): [", round(ci_bootstrap_kendall_zad3[1], 4), ",", round(ci_bootstrap_kendall_zad3[2], 4), "]"))


# e) Wykres rozrzutu zmiennych X i V
print("e) Wykres rozrzutu dla (X,V) zostanie wyświetlony.")
plot(dane_zad3$X, dane_zad3$V, main="Rozrzut zmiennych X i V (Zad. 3)", xlab="X", ylab="V", pch=19)
abline(lsfit(dane_zad3$X, dane_zad3$V), col="blue")


# --- Zadanie 4 ---
print("Zadanie 4")

sciezka_pliku_mieszkania <- "dane/mieszkania.csv"
mieszkania_df <- read.csv2(sciezka_pliku_mieszkania)

cols_to_convert <- c("Metraz", "Pokoje", "Cena")
for (col_name in cols_to_convert) {
  if (!is.numeric(mieszkania_df[[col_name]])) {
    mieszkania_df[[col_name]] <- gsub("\\s", "", mieszkania_df[[col_name]])
    mieszkania_df[[col_name]] <- gsub(",", ".", mieszkania_df[[col_name]]) # Zawsze zamień przecinek na kropkę
    mieszkania_df[[col_name]] <- as.numeric(mieszkania_df[[col_name]])
  }
}

mieszkania_df$CenaM2 <- mieszkania_df$Cena / mieszkania_df$Metraz
dane_zad4 <- mieszkania_df[, c("Metraz", "Pokoje", "Cena", "CenaM2")]
dane_zad4 <- na.omit(dane_zad4)


# a) Macierz współczynników korelacji Pearsona
print("a) Macierz korelacji Pearsona:")
mat_pearson_zad4 <- cor(dane_zad4, method = "pearson")
print(round(mat_pearson_zad4, 4))


# b) Macierz współczynników korelacji Spearmana
print("b) Macierz korelacji Spearmana:")
mat_spearman_zad4 <- cor(dane_zad4, method = "spearman")
print(round(mat_spearman_zad4, 4))


# c) Macierz współczynników korelacji Kendalla
print("c) Macierz korelacji Kendalla:")
mat_kendall_zad4 <- cor(dane_zad4, method = "kendall")
print(round(mat_kendall_zad4, 4))


# d) Opisanie relacji między zmiennymi
print("d) Opis relacji między zmiennymi:")
print("   Należy zinterpretować wartości i znaki współczynników w powyższych macierzach.")


# e) Test hipotezy o zerowej korelacji między CenąM2 a Metrażem
print("e) Testy hipotezy o korelacji rho(CenaM2, Metraz) = 0, alfa = 0.05:")

test_pearson_cenam2_metraz <- cor.test(dane_zad4$CenaM2, dane_zad4$Metraz, method = "pearson")
print("Test Pearsona dla CenaM2 vs Metraz:")
print(test_pearson_cenam2_metraz)
if (test_pearson_cenam2_metraz$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Pearson) między CenąM2 a Metrażem.")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Pearson) między CenąM2 a Metrażem.")
}

test_spearman_cenam2_metraz <- cor.test(dane_zad4$CenaM2, dane_zad4$Metraz, method = "spearman", exact = FALSE)
print("Test Spearmana dla CenaM2 vs Metraz:")
print(test_spearman_cenam2_metraz)
if (test_spearman_cenam2_metraz$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Spearman) między CenąM2 a Metrażem.")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Spearman) między CenąM2 a Metrażem.")
}

test_kendall_cenam2_metraz <- cor.test(dane_zad4$CenaM2, dane_zad4$Metraz, method = "kendall", exact = FALSE)
print("Test Kendalla dla CenaM2 vs Metraz:")
print(test_kendall_cenam2_metraz)
if (test_kendall_cenam2_metraz$p.value < 0.05) {
  print("P-wartość < 0.05, więc odrzucamy H0. Istnieje statystycznie istotna korelacja (Kendall) między CenąM2 a Metrażem.")
} else {
  print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Nie stwierdzono statystycznie istotnej korelacji (Kendall) między CenąM2 a Metrażem.")
}


# --- Zadanie 5 ---
print("Zadanie 5")
set.seed(789)

n_zad5 <- 100
rho_zad5 <- 0.7
X_zad5 <- rnorm(n_zad5)
Y_indep_zad5 <- rnorm(n_zad5)
V_zad5 <- rho_zad5 * X_zad5 + Y_indep_zad5 * sqrt(1 - rho_zad5^2)
dane_zad5 <- data.frame(X = X_zad5, V = V_zad5)

print("a) Wygenerowano 100 realizacji pary (X,V) z rho=0.7. Oto pierwsze kilka wierszy:")
print(head(dane_zad5))
# print(paste("Korelacja Pearsona w próbce (X,V) z Zad.5:", round(cor(dane_zad5$X, dane_zad5$V),4)))