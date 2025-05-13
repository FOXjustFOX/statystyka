# Statystyka dla Inżynierów - Laboratorium 9
# Testy dla Średniej

# Zadanie 1: Testy dla średniego IQ
cat("--- Zadanie 1: Testy dla średniego IQ ---\n")

# Dane z zadania 1
n <- 100 # rozmiar próby
mean_sample <- 109 # średnia z próby
var_sample <- 225 # wariancja z próby
sd_sample <- sqrt(var_sample) # odchylenie standardowe z próby
mu0_zad1 <- 105 # wartość średniej wg hipotezy zerowej

# a) Test Z
cat("\n--- Zadanie 1a) - Test Z ---\n")
# Statystyka testowa Z
z_statistic_zad1 <- (mean_sample - mu0_zad1) / (sd_sample / sqrt(n))
# Wartość p (dwustronny test)
p_value_z_zad1 <- 2 * (1 - pnorm(abs(z_statistic_zad1)))

cat("Statystyka testowa Z:", z_statistic_zad1, "\n")
cat("Wartość p:", p_value_z_zad1, "\n")

# Porównanie statystyki testowej z wartościami krytycznymi (alpha = 0.05)
alpha <- 0.05
critical_value_z_zad1 <- qnorm(1 - alpha/2)
cat("Wartość krytyczna Z (alpha =", alpha, "):", critical_value_z_zad1, "\n")
if (abs(z_statistic_zad1) > critical_value_z_zad1) {
  cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
} else {
  cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
}

# b) Test Studenta (t-test dla jednej próby)
cat("\n--- Zadanie 1b) - Test Studenta ---\n")
# Statystyka testowa t
t_statistic_zad1 <- (mean_sample - mu0_zad1) / (sd_sample / sqrt(n))
# Stopnie swobody
df_zad1 <- n - 1
# Wartość p (dwustronny test)
p_value_t_zad1 <- 2 * (1 - pt(abs(t_statistic_zad1), df = df_zad1))

cat("Statystyka testowa t:", t_statistic_zad1, "\n")
cat("Wartość p:", p_value_t_zad1, "\n")
cat("Stopnie swobody (df):", df_zad1, "\n")

# Porównanie statystyki testowej z wartościami krytycznymi (alpha = 0.05)
critical_value_t_zad1 <- qt(1 - alpha/2, df = df_zad1)
cat("Wartość krytyczna t (alpha =", alpha, ", df =", df_zad1, "):", critical_value_t_zad1, "\n")
if (abs(t_statistic_zad1) > critical_value_t_zad1) {
  cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
} else {
  cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
}

cat("\n\n")

# --- Zadania 2-5 ---
# Wczytanie danych z pliku waga1.csv
tryCatch({
  data_waga1 <- read.csv("waga1.csv", sep = ";") # Użycie separatora ';'
  cat("Pomyślnie wczytano plik waga1.csv\n")
}, error = function(e) {
  cat("Błąd podczas wczytywania pliku waga1.csv:", e$message, "\n")
  data_waga1 <- NULL # Ustawienie na NULL w przypadku błędu
})

# Sprawdź czy dane zostały wczytane
if (!is.null(data_waga1)) {

  # --- Zadanie 2: Średni wzrost studentów (obu płci) vs 168cm ---
  cat("--- Zadanie 2: Średni wzrost studentów (obu płci) vs 168cm ---\n")
  mu0_zad2 <- 168
  dane_wzrost_zad2 <- data_waga1$Wzrost

  if (!is.null(dane_wzrost_zad2) && length(dane_wzrost_zad2) > 0) {
    cat("Weryfikacja hipotezy: średni wzrost studentów (obu płci) wynosi", mu0_zad2, "cm\n")

    # a) Test Z (przybliżony)
    cat("\n--- Zadanie 2a) - Test Z (przybliżony) ---\n")
    n_z2 <- length(dane_wzrost_zad2)
    if (n_z2 < 2) {
        cat("Zbyt mało danych dla testu Z w Zadaniu 2.\n")
    } else {
        mean_x_z2 <- mean(dane_wzrost_zad2, na.rm = TRUE)
        sd_x_z2 <- sd(dane_wzrost_zad2, na.rm = TRUE)
        z_statistic_zad2 <- (mean_x_z2 - mu0_zad2) / (sd_x_z2 / sqrt(n_z2))
        p_value_z_zad2 <- 2 * (1 - pnorm(abs(z_statistic_zad2)))
        cat("Statystyka testowa Z:", z_statistic_zad2, "\n")
        cat("Wartość p:", p_value_z_zad2, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        alpha <- 0.05
        critical_value_z_zad2 <- qnorm(1 - alpha/2)
        cat("Wartość krytyczna Z (alpha =", alpha, "):", critical_value_z_zad2, "\n")
        if (abs(z_statistic_zad2) > critical_value_z_zad2) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # b) Test Studenta (ręcznie)
    cat("\n--- Zadanie 2b) - Test Studenta (ręcznie) ---\n")
    n_t2 <- length(dane_wzrost_zad2)
    if (n_t2 < 2) {
        cat("Zbyt mało danych dla t-testu w Zadaniu 2.\n")
    } else {
        mean_x_t2 <- mean(dane_wzrost_zad2, na.rm = TRUE)
        sd_x_t2 <- sd(dane_wzrost_zad2, na.rm = TRUE)
        t_statistic_zad2 <- (mean_x_t2 - mu0_zad2) / (sd_x_t2 / sqrt(n_t2))
        df_zad2 <- n_t2 - 1
        p_value_t_manual_zad2 <- 2 * (1 - pt(abs(t_statistic_zad2), df = df_zad2))
        cat("Statystyka testowa t:", t_statistic_zad2, "\n")
        cat("Wartość p:", p_value_t_manual_zad2, "\n")
        cat("Stopnie swobody (df):", df_zad2, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        critical_value_t_zad2 <- qt(1 - alpha/2, df = df_zad2)
        cat("Wartość krytyczna t (alpha =", alpha, ", df =", df_zad2, "):", critical_value_t_zad2, "\n")
         if (abs(t_statistic_zad2) > critical_value_t_zad2) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # c) Polecenie t.test
    cat("\n--- Zadanie 2c) - Polecenie t.test ---\n")
    if (length(dane_wzrost_zad2) >= 2) {
        wynik_t_test_zad2 <- t.test(dane_wzrost_zad2, mu = mu0_zad2)
        print(wynik_t_test_zad2)
    } else {
        cat("Zbyt mało danych dla t.test() w Zadaniu 2.\n")
    }

  } else {
    cat("Brak danych dla Zadania 2 (kolumna 'Wzrost' nie znaleziona lub błąd wczytywania danych).\n")
  }

  cat("\n\n")

  # --- Zadanie 3: Średni wzrost studentów męskich vs 172cm ---
  cat("--- Zadanie 3: Średni wzrost studentów męskich vs 172cm ---\n")
  mu0_zad3 <- 172
  # Dane dla mężczyzn (plec == 0)
  dane_mezczyzni_wzrost_zad3 <- data_waga1$Wzrost[data_waga1$plec == 0]

  if (!is.null(dane_mezczyzni_wzrost_zad3) && length(dane_mezczyzni_wzrost_zad3) > 0) {
    cat("Weryfikacja hipotezy: średni wzrost studentów męskich wynosi", mu0_zad3, "cm\n")

    # a) Test Z (przybliżony)
    cat("\n--- Zadanie 3a) - Test Z (przybliżony) ---\n")
    n_z3 <- length(dane_mezczyzni_wzrost_zad3)
    if (n_z3 < 2) {
        cat("Zbyt mało danych dla testu Z w Zadaniu 3.\n")
    } else {
        mean_x_z3 <- mean(dane_mezczyzni_wzrost_zad3, na.rm = TRUE)
        sd_x_z3 <- sd(dane_mezczyzni_wzrost_zad3, na.rm = TRUE)
        z_statistic_zad3 <- (mean_x_z3 - mu0_zad3) / (sd_x_z3 / sqrt(n_z3))
        p_value_z_zad3 <- 2 * (1 - pnorm(abs(z_statistic_zad3)))
        cat("Statystyka testowa Z:", z_statistic_zad3, "\n")
        cat("Wartość p:", p_value_z_zad3, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        alpha <- 0.05
        critical_value_z_zad3 <- qnorm(1 - alpha/2)
        cat("Wartość krytyczna Z (alpha =", alpha, "):", critical_value_z_zad3, "\n")
        if (abs(z_statistic_zad3) > critical_value_z_zad3) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # b) Test Studenta (ręcznie)
    cat("\n--- Zadanie 3b) - Test Studenta (ręcznie) ---\n")
    n_t3 <- length(dane_mezczyzni_wzrost_zad3)
    if (n_t3 < 2) {
        cat("Zbyt mało danych dla t-testu w Zadaniu 3.\n")
    } else {
        mean_x_t3 <- mean(dane_mezczyzni_wzrost_zad3, na.rm = TRUE)
        sd_x_t3 <- sd(dane_mezczyzni_wzrost_zad3, na.rm = TRUE)
        t_statistic_zad3 <- (mean_x_t3 - mu0_zad3) / (sd_x_t3 / sqrt(n_t3))
        df_zad3 <- n_t3 - 1
        p_value_t_manual_zad3 <- 2 * (1 - pt(abs(t_statistic_zad3), df = df_zad3))
        cat("Statystyka testowa t:", t_statistic_zad3, "\n")
        cat("Wartość p:", p_value_t_manual_zad3, "\n")
        cat("Stopnie swobody (df):", df_zad3, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        critical_value_t_zad3 <- qt(1 - alpha/2, df = df_zad3)
        cat("Wartość krytyczna t (alpha =", alpha, ", df =", df_zad3, "):", critical_value_t_zad3, "\n")
         if (abs(t_statistic_zad3) > critical_value_t_zad3) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # c) Polecenie t.test
    cat("\n--- Zadanie 3c) - Polecenie t.test ---\n")
     if (length(dane_mezczyzni_wzrost_zad3) >= 2) {
        wynik_t_test_zad3 <- t.test(dane_mezczyzni_wzrost_zad3, mu = mu0_zad3)
        print(wynik_t_test_zad3)
    } else {
        cat("Zbyt mało danych dla t.test() w Zadaniu 3.\n")
    }

  } else {
    cat("Brak danych dla Zadania 3 (brak danych dla mężczyzn lub błąd wczytywania danych).\n")
  }

  cat("\n\n")

  # --- Zadanie 4: Średnia zmiana wagi studentów (obu płci) vs 2kg ---
  cat("--- Zadanie 4: Średnia zmiana wagi studentów (obu płci) vs 2kg ---\n")
  mu0_zad4 <- 2
  # Obliczenie zmiany wagi: Waga_po - Waga_przed
  dane_zmiana_wagi_zad4 <- data_waga1$Waga_po - data_waga1$Waga_przed

  if (!is.null(dane_zmiana_wagi_zad4) && length(dane_zmiana_wagi_zad4) > 0) {
    cat("Weryfikacja hipotezy: średnia zmiana wagi studentów (obu płci) wynosi", mu0_zad4, "kg\n")

    # a) Test Z (przybliżony)
    cat("\n--- Zadanie 4a) - Test Z (przybliżony) ---\n")
    n_z4 <- length(dane_zmiana_wagi_zad4)
    if (n_z4 < 2) {
        cat("Zbyt mało danych dla testu Z w Zadaniu 4.\n")
    } else {
        mean_x_z4 <- mean(dane_zmiana_wagi_zad4, na.rm = TRUE)
        sd_x_z4 <- sd(dane_zmiana_wagi_zad4, na.rm = TRUE)
        z_statistic_zad4 <- (mean_x_z4 - mu0_zad4) / (sd_x_z4 / sqrt(n_z4))
        p_value_z_zad4 <- 2 * (1 - pnorm(abs(z_statistic_zad4)))
        cat("Statystyka testowa Z:", z_statistic_zad4, "\n")
        cat("Wartość p:", p_value_z_zad4, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        alpha <- 0.05
        critical_value_z_zad4 <- qnorm(1 - alpha/2)
        cat("Wartość krytyczna Z (alpha =", alpha, "):", critical_value_z_zad4, "\n")
        if (abs(z_statistic_zad4) > critical_value_z_zad4) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # b) Test Studenta (ręcznie)
    cat("\n--- Zadanie 4b) - Test Studenta (ręcznie) ---\n")
    n_t4 <- length(dane_zmiana_wagi_zad4)
    if (n_t4 < 2) {
        cat("Zbyt mało danych dla t-testu w Zadaniu 4.\n")
    } else {
        mean_x_t4 <- mean(dane_zmiana_wagi_zad4, na.rm = TRUE)
        sd_x_t4 <- sd(dane_zmiana_wagi_zad4, na.rm = TRUE)
        t_statistic_zad4 <- (mean_x_t4 - mu0_zad4) / (sd_x_t4 / sqrt(n_t4))
        df_zad4 <- n_t4 - 1
        p_value_t_manual_zad4 <- 2 * (1 - pt(abs(t_statistic_zad4), df = df_zad4))
        cat("Statystyka testowa t:", t_statistic_zad4, "\n")
        cat("Wartość p:", p_value_t_manual_zad4, "\n")
        cat("Stopnie swobody (df):", df_zad4, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        critical_value_t_zad4 <- qt(1 - alpha/2, df = df_zad4)
        cat("Wartość krytyczna t (alpha =", alpha, ", df =", df_zad4, "):", critical_value_t_zad4, "\n")
         if (abs(t_statistic_zad4) > critical_value_t_zad4) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # c) Polecenie t.test
    cat("\n--- Zadanie 4c) - Polecenie t.test ---\n")
    if (length(dane_zmiana_wagi_zad4) >= 2) {
        wynik_t_test_zad4 <- t.test(dane_zmiana_wagi_zad4, mu = mu0_zad4)
        print(wynik_t_test_zad4)
    } else {
        cat("Zbyt mało danych dla t.test() w Zadaniu 4.\n")
    }

  } else {
    cat("Brak danych dla Zadania 4 (kolumny wagi nie znaleziona lub błąd wczytywania danych).\n")
  }

  cat("\n\n")

  # --- Zadanie 5: Średnia zmiana wagi studentów męskich vs 4kg ---
  cat("--- Zadanie 5: Średnia zmiana wagi studentów męskich vs 4kg ---\n")
  mu0_zad5 <- 4
  # Obliczenie zmiany wagi tylko dla mężczyzn (plec == 0)
  dane_mezczyzni_zmiana_wagi_zad5 <- (data_waga1$Waga_po - data_waga1$Waga_przed)[data_waga1$plec == 0]

  if (!is.null(dane_mezczyzni_zmiana_wagi_zad5) && length(dane_mezczyzni_zmiana_wagi_zad5) > 0) {
    cat("Weryfikacja hipotezy: średnia zmiana wagi studentów męskich wynosi", mu0_zad5, "kg\n")

    # a) Test Z (przybliżony)
    cat("\n--- Zadanie 5a) - Test Z (przybliżony) ---\n")
    n_z5 <- length(dane_mezczyzni_zmiana_wagi_zad5)
    if (n_z5 < 2) {
        cat("Zbyt mało danych dla testu Z w Zadaniu 5.\n")
    } else {
        mean_x_z5 <- mean(dane_mezczyzni_zmiana_wagi_zad5, na.rm = TRUE)
        sd_x_z5 <- sd(dane_mezczyzni_zmiana_wagi_zad5, na.rm = TRUE)
        z_statistic_zad5 <- (mean_x_z5 - mu0_zad5) / (sd_x_z5 / sqrt(n_z5))
        p_value_z_zad5 <- 2 * (1 - pnorm(abs(z_statistic_zad5)))
        cat("Statystyka testowa Z:", z_statistic_zad5, "\n")
        cat("Wartość p:", p_value_z_zad5, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        alpha <- 0.05
        critical_value_z_zad5 <- qnorm(1 - alpha/2)
        cat("Wartość krytyczna Z (alpha =", alpha, "):", critical_value_z_zad5, "\n")
        if (abs(z_statistic_zad5) > critical_value_z_zad5) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # b) Test Studenta (ręcznie)
    cat("\n--- Zadanie 5b) - Test Studenta (ręcznie) ---\n")
    n_t5 <- length(dane_mezczyzni_zmiana_wagi_zad5)
    if (n_t5 < 2) {
        cat("Zbyt mało danych dla t-testu w Zadaniu 5.\n")
    } else {
        mean_x_t5 <- mean(dane_mezczyzni_zmiana_wagi_zad5, na.rm = TRUE)
        sd_x_t5 <- sd(dane_mezczyzni_zmiana_wagi_zad5, na.rm = TRUE)
        t_statistic_zad5 <- (mean_x_t5 - mu0_zad5) / (sd_x_t5 / sqrt(n_t5))
        df_zad5 <- n_t5 - 1
        p_value_t_manual_zad5 <- 2 * (1 - pt(abs(t_statistic_zad5), df = df_zad5))
        cat("Statystyka testowa t:", t_statistic_zad5, "\n")
        cat("Wartość p:", p_value_t_manual_zad5, "\n")
        cat("Stopnie swobody (df):", df_zad5, "\n")
        # Porównanie z wartościami krytycznymi (alpha = 0.05)
        critical_value_t_zad5 <- qt(1 - alpha/2, df = df_zad5)
        cat("Wartość krytyczna t (alpha =", alpha, ", df =", df_zad5, "):", critical_value_t_zad5, "\n")
         if (abs(t_statistic_zad5) > critical_value_t_zad5) {
          cat("Wniosek (alpha =", alpha, "): Odrzucamy hipotezę zerową.\n")
        } else {
          cat("Wniosek (alpha =", alpha, "): Nie odrzucamy hipotezy zerowej.\n")
        }
    }

    # c) Polecenie t.test
    cat("\n--- Zadanie 5c) - Polecenie t.test ---\n")
     if (length(dane_mezczyzni_zmiana_wagi_zad5) >= 2) {
        wynik_t_test_zad5 <- t.test(dane_mezczyzni_zmiana_wagi_zad5, mu = mu0_zad5)
        print(wynik_t_test_zad5)
    } else {
        cat("Zbyt mało danych dla t.test() w Zadaniu 5.\n")
    }

  } else {
    cat("Brak danych dla Zadania 5 (brak danych dla mężczyzn lub błąd wczytywania danych).\n")
  }

} else {
  cat("\n--- Błąd wczytywania danych ---\n")
  cat("Nie można wykonać zadań 2-5, ponieważ plik waga1.csv nie został wczytany.\n")
}
