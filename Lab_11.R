# Laboratorium 11 - Testy Niezależności i Zgodności
# Plik mieszkania.csv jest potrzebny do zadań 3-6.
# Oczekiwane nagłówki w CSV: Dzielnica;Metraz;Pokoje;Pietro;Cena

# install.packages("nortest") # Dla testu Andersona-Darlinga, jeśli chcemy go użyć zamiast Shapiro-Wilka dla większych prób
# library(nortest) # ad.test()

# Funkcja pomocnicza do drukowania separatora
print_separator <- function(exercise_number = NULL) {
  if (!is.null(exercise_number)) {
    cat(paste0("\n------------------------------\nZadanie ", exercise_number, "\n------------------------------\n"))
  } else {
    cat("\n------------------------------\n")
  }
}

alpha <- 0.05 # Poziom istotności dla testów

# --- Zadanie 1 ---
print_separator(1)
# Dane: wyniki rzutów kostką
wyniki_kostka <- c(1, 2, 3, 4, 5, 6)
frekwencja_obserwowana <- c(171, 200, 168, 213, 226, 222)
cat("Dane obserwowane (rzuty kostką):\n")
print(data.frame(Wynik = wyniki_kostka, Frekwencja = frekwencja_obserwowana))

# a) Wyznaczyć tablicę oczekiwanych frekwencji przy hipotezie że kostka jest symetryczna.
cat("\na)\n")
cat("  Oczekiwane frekwencje (kostka symetryczna):\n")
n_rzutow <- sum(frekwencja_obserwowana)
p_oczekiwane_kostka <- rep(1/6, 6) 
frekwencja_oczekiwana_kostka <- n_rzutow * p_oczekiwane_kostka
cat("     Liczba rzutów (N):", n_rzutow, "\n")
cat("     Frekwencje oczekiwane:\n")
print(data.frame(Wynik = wyniki_kostka, OczekiwanaFrekwencja = frekwencja_oczekiwana_kostka))

# b) Wyznaczyć realizację statystki testowej dla odpowiedniego testu zgodności.
cat("\nb)\n")
cat("  Statystyka testowa Chi-kwadrat (ręcznie):\n")
statystyka_chi2_kostka <- sum((frekwencja_obserwowana - frekwencja_oczekiwana_kostka)^2 / frekwencja_oczekiwana_kostka)
cat("     Wartość statystyki Chi2 =", statystyka_chi2_kostka, "\n")

# c) Wyznaczyć wartość p dla tego testu.
cat("\nc)\n")
cat("  Wartość p (ręcznie):\n")
df_kostka <- length(wyniki_kostka) - 1 
p_value_chi2_kostka <- pchisq(statystyka_chi2_kostka, df = df_kostka, lower.tail = FALSE)
cat("     Stopnie swobody (df):", df_kostka, "\n")
cat("     p-value =", p_value_chi2_kostka, "\n")

# d) Jaki jest wniosek tego testu?
cat("\nd)\n")
cat("  Wniosek z testu:\n")
if (p_value_chi2_kostka < alpha) {
  cat("     Odrzucamy H0: Kostka nie jest symetryczna (rozkład wyników różni się od równomiernego).\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że kostka nie jest symetryczna.\n")
}

# e) Wykonać ten test za pomocą polecenia chisq.test
cat("\ne)\n")
cat("  Wynik polecenia chisq.test:\n")
wynik_chisq_kostka <- chisq.test(x = frekwencja_obserwowana, p = p_oczekiwane_kostka)
print(wynik_chisq_kostka)
cat("     p-value (z chisq.test) =", wynik_chisq_kostka$p.value, "\n")


# --- Zadanie 2 ---
print_separator(2)
# Dane: poziom wykształcenia według płci
tabela_wyksztalcenie_plec <- matrix(c(200, 150, 300, 350), nrow = 2, byrow = FALSE,
                                   dimnames = list(Plec = c("Kobiety", "Mężczyźni"),
                                                   Wyksztalcenie = c("Wyższe", "Średnie")))
cat("Dane obserwowane (wykształcenie vs płeć):\n")
print(tabela_wyksztalcenie_plec)

# a) Wyznaczyć tablicę oczekiwanych frekwencji przy hipotezie o niezależności tych cech.
cat("\na)\n")
cat("  Oczekiwane frekwencje (niezależność cech):\n")
tabela_oczekiwana_wykszt <- chisq.test(tabela_wyksztalcenie_plec)$expected
cat("     Tablica oczekiwanych frekwencji:\n")
print(tabela_oczekiwana_wykszt)

# b) Wyznaczyć realizację statystki testowej dla testu niezależności Pearsona
cat("\nb)\n")
cat("  Statystyka testowa Chi-kwadrat (ręcznie dla niezależności):\n")
statystyka_chi2_wykszt_recznie <- sum((tabela_wyksztalcenie_plec - tabela_oczekiwana_wykszt)^2 / tabela_oczekiwana_wykszt)
cat("     Wartość statystyki Chi2 (ręcznie) =", statystyka_chi2_wykszt_recznie, "\n")
statystyka_chi2_wykszt_auto <- chisq.test(tabela_wyksztalcenie_plec)$statistic
cat("     Wartość statystyki Chi2 (z chisq.test) =", as.numeric(statystyka_chi2_wykszt_auto), "\n")

# c) Wyznaczyć wartość p dla tego testu.
cat("\nc)\n")
cat("  Wartość p (z testu Chi-kwadrat):\n")
df_wykszt <- (nrow(tabela_wyksztalcenie_plec) - 1) * (ncol(tabela_wyksztalcenie_plec) - 1)
p_value_chi2_wykszt_recznie <- pchisq(statystyka_chi2_wykszt_recznie, df = df_wykszt, lower.tail = FALSE)
cat("     Stopnie swobody (df):", df_wykszt, "\n")
cat("     p-value (ręcznie) =", p_value_chi2_wykszt_recznie, "\n")
p_value_chi2_wykszt_auto <- chisq.test(tabela_wyksztalcenie_plec)$p.value
cat("     p-value (z chisq.test) =", p_value_chi2_wykszt_auto, "\n")

# d) Jaki jest wniosek tego testu?
cat("\nd)\n")
cat("  Wniosek z testu Chi-kwadrat niezależności:\n")
if (p_value_chi2_wykszt_auto < alpha) {
  cat("     Odrzucamy H0: Poziom wykształcenia zależy od płci.\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów na zależność poziomu wykształcenia od płci.\n")
}

# e) Wykonać test ten za pomocą polecenia chisq.test
cat("\ne)\n")
cat("  Wynik polecenia chisq.test (powtórzenie dla jasności):\n")
wynik_chisq_wykszt <- chisq.test(tabela_wyksztalcenie_plec) 
print(wynik_chisq_wykszt)

# f) Wykonać test ten za pomocą dokładnego testu Fishera (fisher.test)
cat("\nf)\n")
cat("  Wynik dokładnego testu Fishera (fisher.test):\n")
wynik_fisher_wykszt <- fisher.test(tabela_wyksztalcenie_plec)
print(wynik_fisher_wykszt)
cat("     p-value (Fisher) =", wynik_fisher_wykszt$p.value, "\n")
cat("  Wniosek z testu Fishera:\n")
if (wynik_fisher_wykszt$p.value < alpha) {
  cat("     Odrzucamy H0: Poziom wykształcenia zależy od płci.\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów na zależność poziomu wykształcenia od płci.\n")
}

# Wczytanie danych z pliku mieszkania.csv
cat("\n--- Wczytywanie danych z pliku mieszkania.csv ---\n")
# Upewnij się, że plik 'mieszkania.csv' jest w katalogu roboczym R lub podaj pełną ścieżkę
dane_mieszkania <- NULL # Inicjalizacja na wypadek błędu wczytywania
tryCatch({
  dane_mieszkania <- read.csv2("dane/mieszkania.csv", stringsAsFactors = TRUE) # stringsAsFactors dla Dzielnicy
  cat("Dane 'mieszkania.csv' wczytane pomyślnie.\n")
  
  # Obliczenie Cena_za_m2
  if ("Cena" %in% names(dane_mieszkania) && "Metraz" %in% names(dane_mieszkania)) {
    dane_mieszkania$Cena_za_m2 <- dane_mieszkania$Cena / dane_mieszkania$Metraz
    # Obsługa potencjalnych Inf lub NaN jeśli Metraz = 0 lub Cena/Metraz jest problematyczne
    dane_mieszkania$Cena_za_m2[is.infinite(dane_mieszkania$Cena_za_m2)] <- NA
    dane_mieszkania$Cena_za_m2[is.nan(dane_mieszkania$Cena_za_m2)] <- NA
    cat("Kolumna 'Cena_za_m2' została obliczona (Cena / Metraz).\n")
  } else {
    cat("OSTRZEŻENIE: Brak kolumn 'Cena' lub 'Metraz' do obliczenia 'Cena_za_m2'.\n")
  }
  
  cat("Pierwsze wiersze danych (po ewentualnym dodaniu Cena_za_m2):\n")
  print(head(dane_mieszkania))
  cat("\nStruktura danych:\n")
  str(dane_mieszkania)

}, error = function(e) {
  cat("BŁĄD: Nie udało się wczytać pliku mieszkania.csv.\n")
  cat("Upewnij się, że plik istnieje w katalogu roboczym lub podałeś poprawną ścieżkę.\n")
  cat("Komunikat błędu R:", conditionMessage(e), "\n")
})

if (!is.null(dane_mieszkania)) {
  # --- Zadanie 3 ---
  print_separator(3)
  # Używane kolumny: 'Pokoje' i 'Dzielnica'
  if (!("Pokoje" %in% names(dane_mieszkania)) || !("Dzielnica" %in% names(dane_mieszkania))) {
    cat("BŁĄD: Brak wymaganych kolumn ('Pokoje', 'Dzielnica') w danych 'mieszkania'. Pomijanie Zadania 3.\n")
  } else {
    # a) Sporządzić tabelę rozdzielczą opisującą rozkład liczby pokoi w zależności od dzielnicy (rejonu).
    cat("a)\n")
    cat("  Tabela rozdzielcza (liczba pokoi vs dzielnica):\n")
    tabela_pokoje_dzielnica <- table(dane_mieszkania$Pokoje, dane_mieszkania$Dzielnica)
    print(tabela_pokoje_dzielnica)

    # b) Stworzyć nową zmienną 'Pokoje_grupowane'
    cat("\nb)\n")
    cat("  Tworzenie nowej zmiennej 'Pokoje_grupowane':\n")
    dane_mieszkania$Pokoje_grupowane <- ifelse(dane_mieszkania$Pokoje >= 4, 4, dane_mieszkania$Pokoje)
    cat("     Pierwsze wartości nowej zmiennej (Pokoje vs Pokoje_grupowane):\n")
    print(head(data.frame(Pokoje_Oryginalne = dane_mieszkania$Pokoje, 
                          Pokoje_Grupowane = dane_mieszkania$Pokoje_grupowane)))
    cat("     Rozkład nowej zmiennej:\n")
    print(table(dane_mieszkania$Pokoje_grupowane))
    
    # c) Przetestować hipotezę, iż liczba pokoi jest niezależna od dzielnicy.
    cat("\nc)\n")
    cat("  Test Chi-kwadrat niezależności (Pokoje_grupowane vs Dzielnica):\n")
    tabela_pokoje_grup_dzielnica <- table(dane_mieszkania$Pokoje_grupowane, dane_mieszkania$Dzielnica)
    cat("     Tabela kontyngencji dla testu:\n")
    print(tabela_pokoje_grup_dzielnica)
    
    test_pokoje_dzielnica <- chisq.test(tabela_pokoje_grup_dzielnica)
    cat("     Wynik testu chisq.test:\n")
    print(test_pokoje_dzielnica)
    if (any(test_pokoje_dzielnica$expected < 5)) {
      cat("     OSTRZEŻENIE: Niektóre oczekiwane liczebności są mniejsze niż 5. Wynik testu Chi-kwadrat może być niedokładny.\n")
    }
    cat("  Wniosek z testu:\n")
    if (test_pokoje_dzielnica$p.value < alpha) {
      cat("     Odrzucamy H0: Liczba pokoi (grupowana) zależy od dzielnicy.\n")
    } else {
      cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów na zależność liczby pokoi (grupowanej) od dzielnicy.\n")
    }
  } 

  # --- Zadanie 4 ---
  print_separator(4)
  # Używane kolumny: 'Cena_za_m2' (obliczona) i 'Dzielnica'
  if (!("Cena_za_m2" %in% names(dane_mieszkania)) || !("Dzielnica" %in% names(dane_mieszkania))) {
    cat("BŁĄD: Brak wymaganych kolumn ('Cena_za_m2', 'Dzielnica') w danych 'mieszkania'. Pomijanie Zadania 4.\n")
  } else {
    # a) Tworzyć zmienną, która wskazuje czy cena za m^2 jest większa niż 6000zł czy nie.
    cat("a)\n")
    cat("  Tworzenie nowej zmiennej 'Cena_m2_powyzej_6000':\n")
    prog_ceny <- 6000
    # Usunięcie NA z Cena_za_m2 przed tworzeniem faktora, aby uniknąć poziomu NA
    dane_mieszkania_filtered_4a <- dane_mieszkania[!is.na(dane_mieszkania$Cena_za_m2), ]
    if(nrow(dane_mieszkania_filtered_4a) > 0) {
        dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000 <- ifelse(dane_mieszkania_filtered_4a$Cena_za_m2 > prog_ceny, 
                                                     paste0(">", prog_ceny), 
                                                     paste0("<=", prog_ceny))
        dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000 <- as.factor(dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000)
        cat("     Pierwsze wartości nowej zmiennej (Cena_za_m2 vs Cena_m2_powyzej_6000):\n")
        print(head(data.frame(Cena_za_m2_Oryginalna = dane_mieszkania_filtered_4a$Cena_za_m2,
                              Cena_m2_Kategoria = dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000)))
        cat("     Rozkład nowej zmiennej:\n")
        print(table(dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000))

        # b) Przetestować hipotezę iż prawdopodobieństwo tego, że cena za m^2 jest większa niż 6000zł
        # zależy od dzielnicy.
        cat("\nb)\n")
        cat("  Test Chi-kwadrat niezależności (Cena_m2_powyzej_6000 vs Dzielnica):\n")
        tabela_cena_dzielnica <- table(dane_mieszkania_filtered_4a$Cena_m2_powyzej_6000, dane_mieszkania_filtered_4a$Dzielnica)
        cat("     Tabela kontyngencji dla testu:\n")
        print(tabela_cena_dzielnica)
        
        if(nrow(tabela_cena_dzielnica) < 2 || ncol(tabela_cena_dzielnica) < 2){
            cat("     OSTRZEŻENIE: Tabela kontyngencji ma mniej niż 2 wiersze lub kolumny. Test Chi-kwadrat nie może być wykonany.\n")
        } else {
            test_cena_dzielnica <- chisq.test(tabela_cena_dzielnica)
            cat("     Wynik testu chisq.test:\n")
            print(test_cena_dzielnica)
            if (any(test_cena_dzielnica$expected < 5)) {
              cat("     OSTRZEŻENIE: Niektóre oczekiwane liczebności są mniejsze niż 5. Wynik testu Chi-kwadrat może być niedokładny.\n")
               if (nrow(tabela_cena_dzielnica) == 2 && ncol(tabela_cena_dzielnica) == 2) {
                   cat("     Wykonuję test Fishera dla tabeli 2x2:\n")
                   print(fisher.test(tabela_cena_dzielnica))
               }
            }
            cat("  Wniosek z testu:\n")
            if (test_cena_dzielnica$p.value < alpha) {
              cat("     Odrzucamy H0: Prawdopodobieństwo, że cena za m^2 jest > 6000zł, zależy od dzielnicy.\n")
            } else {
              cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów na taką zależność.\n")
            }
        }
    } else {
        cat("Brak danych w 'Cena_za_m2' po usunięciu NA. Nie można wykonać Zadania 4.\n")
    }
  } 

  # --- Zadanie 5 ---
  print_separator(5)
  # a) Przetestować hipotezę, iż cena za m^2 ma rozkład normalny.
  cat("a)\n")
  cat("  Test normalności dla 'Cena_za_m2':\n")
  if (!("Cena_za_m2" %in% names(dane_mieszkania))) {
    cat("BŁĄD: Brak kolumny 'Cena_za_m2' w danych 'mieszkania'. Pomijanie Zadania 5a.\n")
  } else {
    cena_m2_do_testu <- na.omit(dane_mieszkania$Cena_za_m2) 
    if (length(cena_m2_do_testu) < 3) {
        cat("     Za mało obserwacji (<3) w 'Cena_za_m2' po usunięciu NA do przeprowadzenia testu normalności.\n")
    } else if (length(cena_m2_do_testu) >= 3 && length(cena_m2_do_testu) <= 5000) { 
      test_shapiro_cena <- shapiro.test(cena_m2_do_testu)
      cat("     Wynik testu Shapiro-Wilka:\n")
      print(test_shapiro_cena)
      cat("  Wniosek (Shapiro-Wilk):\n")
      if (test_shapiro_cena$p.value < alpha) {
        cat("     Odrzucamy H0: Rozkład ceny za m^2 różni się od normalnego.\n")
      } else {
        cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że rozkład ceny za m^2 różni się od normalnego.\n")
      }
    } else { # length > 5000
      cat("     Liczba obserwacji dla 'Cena_za_m2' (", length(cena_m2_do_testu), ") jest > 5000. Test Shapiro-Wilka może nie być odpowiedni.\n")
      cat("     Sugerowany test Kołmogorowa-Smirnowa (Lilliefors) lub Andersona-Darlinga.\n")
      # Przykład z Kołmogorowa-Smirnowa (Lilliefors test for normality)
      # if(require(nortest)) { # Sprawdzenie czy pakiet nortest jest dostępny
      #   lillie_test_cena <- lillie.test(cena_m2_do_testu)
      #   cat("     Wynik testu Lillieforsa (Kołmogorow-Smirnow z estymacją parametrów):\n")
      #   print(lillie_test_cena)
      #   if (lillie_test_cena$p.value < alpha) {
      #     cat("     Odrzucamy H0 (Lilliefors): Rozkład ceny za m^2 różni się od normalnego.\n")
      #   } else {
      #     cat("     Brak podstaw do odrzucenia H0 (Lilliefors): Nie ma dowodów, że rozkład ceny za m^2 różni się od normalnego.\n")
      #   }
      # } else {
      #    cat("     Pakiet 'nortest' nie jest zainstalowany. Nie można wykonać testu Lillieforsa.\n")
      # }
    }
    
    if (length(cena_m2_do_testu) > 1) { # density wymaga co najmniej 2 punktów
        cat("     Rysowanie estymatora gęstości dla 'Cena_za_m2':\n")
        plot(density(cena_m2_do_testu), main = "Estymator gęstości dla Cena_za_m2", xlab = "Cena za m2")
        x_vals <- seq(min(cena_m2_do_testu), max(cena_m2_do_testu), length = 100)
        y_norm <- dnorm(x_vals, mean = mean(cena_m2_do_testu), sd = sd(cena_m2_do_testu))
        lines(x_vals, y_norm, col = "red", lty = 2)
        legend("topright", legend = c("Estymator gęstości", "Rozkład normalny (dopasowany)"), 
               col = c("black", "red"), lty = c(1, 2))
        cat("     (Wykres gęstości został wygenerowany)\n")
    } else {
        cat("     Za mało danych w 'Cena_za_m2' do narysowania wykresu gęstości.\n")
    }
  }

  # b) Przetestować hipotezę, iż metraż mieszkań na Śródmieściu ma rozkład normalny.
  cat("\nb)\n")
  cat("  Test normalności dla 'Metraz' mieszkań na Śródmieściu:\n")
  if (!("Metraz" %in% names(dane_mieszkania)) || !("Dzielnica" %in% names(dane_mieszkania))) {
    cat("BŁĄD: Brak kolumny 'Metraz' lub 'Dzielnica' w danych 'mieszkania'. Pomijanie Zadania 5b.\n")
  } else {
    metraz_srodmiescie <- na.omit(dane_mieszkania$Metraz[dane_mieszkania$Dzielnica == "Śródmieście"])
    if (length(metraz_srodmiescie) == 0) {
        cat("     Brak danych dla dzielnicy 'Śródmieście' lub dzielnica nie istnieje pod taką nazwą.\n")
    } else if (length(metraz_srodmiescie) < 3) {
        cat("     Za mało obserwacji (<3) dla metrażu na Śródmieściu po usunięciu NA do przeprowadzenia testu normalności.\n")
    } else if (length(metraz_srodmiescie) >= 3 && length(metraz_srodmiescie) <= 5000) {
      test_shapiro_metraz_sr <- shapiro.test(metraz_srodmiescie)
      cat("     Wynik testu Shapiro-Wilka dla metrażu na Śródmieściu:\n")
      print(test_shapiro_metraz_sr)
      cat("  Wniosek (Shapiro-Wilk):\n")
      if (test_shapiro_metraz_sr$p.value < alpha) {
        cat("     Odrzucamy H0: Rozkład metrażu na Śródmieściu różni się od normalnego.\n")
      } else {
        cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że rozkład metrażu na Śródmieściu różni się od normalnego.\n")
      }
      
      if(length(metraz_srodmiescie) > 1){
          cat("     Rysowanie estymatora gęstości dla metrażu na Śródmieściu:\n")
          plot(density(metraz_srodmiescie), main = "Estymator gęstości dla Metrażu (Śródmieście)", xlab = "Metraż (Śródmieście)")
          x_vals_sr <- seq(min(metraz_srodmiescie), max(metraz_srodmiescie), length = 100)
          y_norm_sr <- dnorm(x_vals_sr, mean = mean(metraz_srodmiescie), sd = sd(metraz_srodmiescie))
          lines(x_vals_sr, y_norm_sr, col = "blue", lty = 2)
          legend("topright", legend = c("Estymator gęstości", "Rozkład normalny (dopasowany)"), 
                 col = c("black", "blue"), lty = c(1, 2))
          cat("     (Wykres gęstości został wygenerowany)\n")
      } else {
           cat("     Za mało danych dla metrażu na Śródmieściu do narysowania wykresu gęstości.\n")
      }
      
    } else { # length > 5000
      cat("     Liczba obserwacji dla metrażu na Śródmieściu (", length(metraz_srodmiescie), ") jest > 5000. Test Shapiro-Wilka może nie być odpowiedni.\n")
    }
  } 
} 

# --- Zadanie 6 ---
print_separator(6)
# a) Wygenerować próbę 1000 realizacji z rozkładu wykładniczego o wartości oczekiwanej 1.
cat("a)\n")
cat("  Generowanie próby 1000 realizacji z rozkładu Exp(lambda=1):\n")
set.seed(123) 
n_probki_exp <- 1000
lambda_exp <- 1
probka_exp <- rexp(n_probki_exp, rate = lambda_exp)
cat("     Wygenerowano", n_probki_exp, "obserwacji. Pierwsze kilka:\n")
print(head(probka_exp))
cat("     Średnia empiryczna:", mean(probka_exp), "(oczekiwana: 1)\n")
cat("     Wariancja empiryczna:", var(probka_exp), "(oczekiwana (1/lambda^2): 1)\n")

# b) Przetestować hipotezę iż próba ta pochodzi 
cat("\nb)\n")
cat("  Testowanie zgodności próby wykładniczej:\n")
cat("  i) Test zgodności z N(mu=1, sigma=1) (Test Kołmogorowa-Smirnowa):\n")
wynik_ks_exp_norm <- ks.test(probka_exp, "pnorm", mean = 1, sd = 1)
print(wynik_ks_exp_norm)
cat("  Wniosek (vs N(1,1)):\n")
if (wynik_ks_exp_norm$p.value < alpha) {
  cat("     Odrzucamy H0: Próba nie pochodzi z rozkładu N(1,1).\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że próba nie pochodzi z N(1,1).\n")
}

cat("\n  ii) Test zgodności z Exp(lambda=1) (Test Kołmogorowa-Smirnowa):\n")
wynik_ks_exp_exp <- ks.test(probka_exp, "pexp", rate = lambda_exp)
print(wynik_ks_exp_exp)
cat("  Wniosek (vs Exp(1)):\n")
if (wynik_ks_exp_exp$p.value < alpha) {
  cat("     Odrzucamy H0: Próba nie pochodzi z rozkładu Exp(1).\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że próba nie pochodzi z Exp(1).\n")
}

# c) Wygenerować próbę 1000 realizacji z rozkładu Gamma o parametrze kształtu 100 a parametrze skali 1.
cat("\nc)\n")
cat("  Generowanie próby 1000 realizacji z rozkładu Gamma(shape=100, scale=1):\n")
set.seed(456)
n_probki_gamma <- 1000
shape_gamma <- 100
scale_gamma <- 1 
probka_gamma <- rgamma(n_probki_gamma, shape = shape_gamma, scale = scale_gamma)
cat("     Wygenerowano", n_probki_gamma, "obserwacji. Pierwsze kilka:\n")
print(head(probka_gamma))
cat("     Średnia empiryczna:", mean(probka_gamma), "(oczekiwana (shape*scale):", shape_gamma * scale_gamma, ")\n")
cat("     Wariancja empiryczna:", var(probka_gamma), "(oczekiwana (shape*scale^2):", shape_gamma * scale_gamma^2, ")\n")

# d) Przetestować hipotezę iż próba ta pochodzi 
cat("\nd)\n")
cat("  Testowanie zgodności próby Gamma:\n")
cat("  i) Test zgodności z N(mu=100, sigma=10) (Test Kołmogorowa-Smirnowa):\n")
mu_norm_gamma <- 100
sigma_norm_gamma <- 10
wynik_ks_gamma_norm <- ks.test(probka_gamma, "pnorm", mean = mu_norm_gamma, sd = sigma_norm_gamma)
print(wynik_ks_gamma_norm)
cat("  Wniosek (vs N(100,10)):\n")
if (wynik_ks_gamma_norm$p.value < alpha) {
  cat("     Odrzucamy H0: Próba nie pochodzi z rozkładu N(100,10).\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że próba nie pochodzi z N(100,10).\n")
  cat("     (Co jest oczekiwane, bo dla dużego 'shape', rozkład Gamma zbliża się do normalnego - CTG)\n")
}

cat("\n  ii) Test zgodności z Gamma(shape=100, scale=1) (Test Kołmogorowa-Smirnowa):\n")
wynik_ks_gamma_gamma <- ks.test(probka_gamma, "pgamma", shape = shape_gamma, scale = scale_gamma)
print(wynik_ks_gamma_gamma)
cat("  Wniosek (vs Gamma(100,1)):\n")
if (wynik_ks_gamma_gamma$p.value < alpha) {
  cat("     Odrzucamy H0: Próba nie pochodzi z rozkładu Gamma(100,1).\n")
} else {
  cat("     Brak podstaw do odrzucenia H0: Nie ma dowodów, że próba nie pochodzi z Gamma(100,1).\n")
}

cat("\n--- Koniec skryptu Lab 11 ---\n")

