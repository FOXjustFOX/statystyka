# --- Przygotowanie danych (Mieszkania) ---
# Ten blok jest wspólny dla Zadania 1 i Zadania 2

print("--- Przygotowanie danych z pliku mieszkania.csv ---")
# Wczytanie danych
sciezka_mieszkania <- "dane/mieszkania.csv"
# Sprawdzenie, czy plik istnieje - pominięte zgodnie z życzeniem użytkownika
mieszkania_df_oryg <- read.csv2(sciezka_mieszkania, stringsAsFactors = FALSE, fileEncoding = "UTF-8") # Dodano fileEncoding

# Kopia robocza
mieszkania_df <- mieszkania_df_oryg

# Czyszczenie i konwersja kolumny Metraz
if ("Metraz" %in% names(mieszkania_df)) {
  mieszkania_df$Metraz <- gsub(",", ".", mieszkania_df$Metraz)
  mieszkania_df$Metraz <- as.numeric(mieszkania_df$Metraz)
} else {
  # Zatrzymanie lub ostrzeżenie, jeśli kluczowa kolumna nie istnieje
  warning("Brak kolumny 'Metraz' w danych. Dalsze analizy mogą być niemożliwe.")
  # Można dodać stop("Brak kolumny 'Metraz'") jeśli jest to krytyczne
}

# Czyszczenie i konwersja kolumny Cena
if ("Cena" %in% names(mieszkania_df)) {
  mieszkania_df$Cena <- gsub(" zł", "", mieszkania_df$Cena) # Usuń " zł"
  mieszkania_df$Cena <- gsub("\\s", "", mieszkania_df$Cena) # Usuń spacje (np. separatory tysięcy)
  mieszkania_df$Cena <- as.numeric(mieszkania_df$Cena)
} else {
  warning("Brak kolumny 'Cena' w danych.")
}

# Utworzenie zmiennej CenaM2 (cena za metr kwadratowy)
# Sprawdzenie, czy Cena i Metraz istnieją i są numeryczne przed dzieleniem
if (all(c("Cena", "Metraz") %in% names(mieszkania_df)) &&
    is.numeric(mieszkania_df$Cena) && is.numeric(mieszkania_df$Metraz)) {
  mieszkania_df$CenaM2 <- mieszkania_df$Cena / mieszkania_df$Metraz
} else {
  warning("Nie można utworzyć 'CenaM2' z powodu braku lub niepoprawnego formatu kolumn 'Cena' lub 'Metraz'.")
  mieszkania_df$CenaM2 <- NA # Utwórz kolumnę jako NA
}


# Czyszczenie i konwersja kolumny Pietro, utworzenie Pietro_numeric
if ("Pietro" %in% names(mieszkania_df)) {
  mieszkania_df$Pietro_numeric <- NA # Inicjalizacja

  # Mapowanie "parter" na 0
  mieszkania_df$Pietro_numeric[tolower(mieszkania_df$Pietro) == "parter"] <- 0
  # Mapowanie "suterena" na 0 (lub -1 jeśli chcesz odróżnić)
  mieszkania_df$Pietro_numeric[tolower(mieszkania_df$Pietro) == "suterena"] <- 0

  # Obsługa wartości numerycznych "3", "10", itp.
  # Sprawdza, czy cała wartość to liczba
  numeric_floor_indices <- grepl("^[0-9]+$", mieszkania_df$Pietro)
  mieszkania_df$Pietro_numeric[numeric_floor_indices] <- as.numeric(mieszkania_df$Pietro[numeric_floor_indices])

  # Obsługa "X z Y", np. "3 z 5", "10 z 11" - bierzemy X
  # Używamy wyrażenia regularnego do wyciągnięcia pierwszej liczby
  # Zakłada, że piętro to pierwsza liczba przed " z " lub "p" (piętro)
  pietro_pattern_matches <- regmatches(mieszkania_df$Pietro, regexpr("^[0-9]+", mieszkania_df$Pietro))
  # Zastosuj tylko tam, gdzie Pietro_numeric jest wciąż NA i znaleziono dopasowanie
  indices_to_update_from_pattern <- is.na(mieszkania_df$Pietro_numeric) & sapply(pietro_pattern_matches, length) > 0
  mieszkania_df$Pietro_numeric[indices_to_update_from_pattern] <- as.numeric(unlist(pietro_pattern_matches[indices_to_update_from_pattern]))

  # Uwaga: "poddasze" i inne nietypowe wartości (np. "> 10") będą NA w Pietro_numeric,
  # jeśli nie zostały jawnie obsłużone. Można dodać więcej reguł.
  # Dla "poddasze", jeśli ma być > 4, można przypisać np. 5:
  mieszkania_df$Pietro_numeric[tolower(mieszkania_df$Pietro) == "poddasze" & is.na(mieszkania_df$Pietro_numeric)] <- 5 # Przykładowa wartość
  mieszkania_df$Pietro_numeric[grepl(">\\s*10", mieszkania_df$Pietro) & is.na(mieszkania_df$Pietro_numeric)] <- 11 # Dla "> 10"

} else {
  warning("Brak kolumny 'Pietro' w danych.")
  mieszkania_df$Pietro_numeric <- NA # Utwórz kolumnę jako NA
}

# Utworzenie zmiennej Pietro_1 (0: do 4-tego włącznie, 1: powyżej 4-tego)
mieszkania_df$Pietro_1 <- NA
if (is.numeric(mieszkania_df$Pietro_numeric)) {
    valid_pietro_indices <- !is.na(mieszkania_df$Pietro_numeric)
    mieszkania_df$Pietro_1[valid_pietro_indices] <- ifelse(mieszkania_df$Pietro_numeric[valid_pietro_indices] > 4, 1, 0)
} else {
    warning("Kolumna 'Pietro_numeric' nie jest numeryczna. Nie można utworzyć 'Pietro_1'.")
}


# Przygotowanie zmiennej Dzielnica jako factor z poziomem referencyjnym "Krzyki"
if ("Dzielnica" %in% names(mieszkania_df)) {
  mieszkania_df$Dzielnica <- as.factor(mieszkania_df$Dzielnica)
  if ("Krzyki" %in% levels(mieszkania_df$Dzielnica)) {
    mieszkania_df$Dzielnica <- relevel(mieszkania_df$Dzielnica, ref = "Krzyki")
  } else {
    # Jeśli "Krzyki" nie ma, ale są inne, wybierz najczęstszy jako referencyjny
    if (length(levels(mieszkania_df$Dzielnica)) > 0) {
        freq_table <- sort(table(mieszkania_df$Dzielnica), decreasing = TRUE)
        if (length(freq_table) > 0) {
            ref_level <- names(freq_table)[1]
            mieszkania_df$Dzielnica <- relevel(mieszkania_df$Dzielnica, ref = ref_level)
            warning(paste("Poziom 'Krzyki' nie znaleziony w kolumnie Dzielnica. Ustawiono najczęstszy poziom ('", ref_level, "') jako referencyjny.", sep=""))
        } else {
             warning("Kolumna 'Dzielnica' nie ma żadnych danych po konwersji na factor.")
        }
    } else {
        warning("Poziom 'Krzyki' nie znaleziony, a kolumna 'Dzielnica' nie ma żadnych poziomów.")
    }
  }
} else {
  warning("Brak kolumny 'Dzielnica' w danych.")
  # Tworzenie pustego faktora, aby uniknąć błędów dalej, jeśli jest krytyczny
  mieszkania_df$Dzielnica <- factor(NA, levels=c("BrakDanych"))
}


# Selekcja kolumn i usunięcie wierszy z brakami danych (NA) w kluczowych zmiennych
# Kolumny potrzebne do modelu: CenaM2, Cena, Metraz, Pietro_1, Dzielnica
cols_for_model <- c("CenaM2", "Cena", "Metraz", "Pietro_1", "Dzielnica")
# Upewnij się, że wszystkie te kolumny istnieją
existing_cols_for_model <- cols_for_model[cols_for_model %in% names(mieszkania_df)]

if (length(existing_cols_for_model) < length(cols_for_model)) {
    warning("Nie wszystkie wymagane kolumny do modelu istnieją. Dalsze analizy mogą być ograniczone.")
}
# Kontynuuj z istniejącymi kolumnami
mieszkania_clean_df <- mieszkania_df[, existing_cols_for_model, drop = FALSE]
mieszkania_clean_df <- na.omit(mieszkania_clean_df)

print(paste("Liczba obserwacji po czyszczeniu i usunięciu NA:", nrow(mieszkania_clean_df)))
if(nrow(mieszkania_clean_df) < 2) { # Potrzeba co najmniej 2 obserwacji dla regresji
  stop("Po czyszczeniu danych pozostało zbyt mało kompletnych obserwacji. Sprawdź dane wejściowe i logikę czyszczenia.")
}
print("Struktura przygotowanych danych (pierwsze wiersze):")
if (nrow(mieszkania_clean_df) > 0) print(head(mieszkania_clean_df))
if ("Pietro_1" %in% names(mieszkania_clean_df)) {
    print("Podsumowanie Pietro_1:")
    print(table(mieszkania_clean_df$Pietro_1, useNA = "ifany"))
}
if ("Dzielnica" %in% names(mieszkania_clean_df) && is.factor(mieszkania_clean_df$Dzielnica)) {
    print("Poziomy Dzielnica (referencyjny powinien być pierwszy):")
    print(levels(mieszkania_clean_df$Dzielnica))
}
print("--- Koniec przygotowania danych z pliku mieszkania.csv ---")


# --- Zadanie 1 ---
print("Zadanie 1: Model dla CenaM2")

# Sprawdzenie, czy wymagane zmienne są dostępne w `mieszkania_clean_df`
required_vars_zad1 <- c("CenaM2", "Metraz", "Pietro_1", "Dzielnica")
if (!all(required_vars_zad1 %in% names(mieszkania_clean_df))) {
    print("Brak wszystkich wymaganych zmiennych dla Zadania 1 po czyszczeniu. Pomijam Zadanie 1.")
} else {
    # a) Regresja krokowa wsteczna
    # Sprawdzenie, czy Dzielnica ma co najmniej 2 poziomy w oczyszczonych danych
    formula_zad1_str <- "CenaM2 ~ Metraz + Pietro_1"
    if (is.factor(mieszkania_clean_df$Dzielnica) && nlevels(droplevels(mieszkania_clean_df$Dzielnica)) >= 2) {
        formula_zad1_str <- paste(formula_zad1_str, "+ Dzielnica")
    } else {
        print("Zmienna Dzielnica ma mniej niż 2 poziomy w oczyszczonych danych lub nie jest faktorem. Pomijam Dzielnica w modelu Zad1.")
    }
    model_pelny_zad1 <- lm(as.formula(formula_zad1_str), data = mieszkania_clean_df)

    print("a) Budowa modelu dla CenaM2 za pomocą regresji krokowej wstecznej:")
    if (length(coef(model_pelny_zad1)) > 1 && model_pelny_zad1$df.residual > 0) {
        # Upewnij się, że model ma wystarczająco stopni swobody
        # `scope` argument dla `step` może być potrzebny, jeśli model jest prosty
        lower_scope <- lm(CenaM2 ~ 1, data = mieszkania_clean_df) # Model tylko z interceptem
        model_krokowy_zad1 <- step(model_pelny_zad1, direction = "backward", trace = 0, scope = list(lower=formula(lower_scope), upper=formula(model_pelny_zad1)))
        print("Podsumowanie wybranego modelu (Zadanie 1):")
        print(summary(model_krokowy_zad1))
    } else {
        print("Nie można przeprowadzić regresji krokowej - model pełny jest zbyt prosty, brak danych lub stopni swobody.")
        model_krokowy_zad1 <- model_pelny_zad1
    }

    # b) Oszacowanie średniej ceny za m² (Y)
    print("b) Oszacowanie średniej CenaM2:")
    if (exists("model_krokowy_zad1") && inherits(model_krokowy_zad1, "lm") && model_krokowy_zad1$df.residual > 0) {

        # Przygotowanie Dzielnica dla newdata
        dzielnica_levels <- if(is.factor(mieszkania_clean_df$Dzielnica)) levels(mieszkania_clean_df$Dzielnica) else "BrakDanych"

        # i) Metraż=80m, 10-te piętro (Pietro_1=1), Fabryczna
        nowe_dane_i <- data.frame(Metraz = 80, Pietro_1 = 1)
        if ("Dzielnica" %in% names(model_krokowy_zad1$coefficients) || any(grepl("Dzielnica", names(model_krokowy_zad1$coefficients)))) {
             if ("Fabryczna" %in% dzielnica_levels) {
                nowe_dane_i$Dzielnica <- factor("Fabryczna", levels = dzielnica_levels)
             } else {
                warning("Poziom 'Fabryczna' nie istnieje w oryginalnych danych. Używam poziomu referencyjnego dla predykcji i).")
                nowe_dane_i$Dzielnica <- factor(dzielnica_levels[1], levels = dzielnica_levels)
             }
        }
        # Usunięcie kolumn z newdata, których nie ma w modelu
        model_terms_zad1 <- labels(terms(model_krokowy_zad1))
        nowe_dane_i <- nowe_dane_i[, names(nowe_dane_i) %in% c("Metraz", "Pietro_1", "Dzielnica") & names(nowe_dane_i) %in% model_terms_zad1, drop=FALSE]
        # Jeśli Dzielnica nie jest w modelu krokowym, a jest w `nowe_dane_i`, to predict może dać warning.
        # Usuwamy kolumny, które nie są w modelu:
        predictors_in_model_zad1 <- all.vars(formula(model_krokowy_zad1)[-2]) # -2 usuwa zmienną zależną
        nowe_dane_i <- nowe_dane_i[, intersect(names(nowe_dane_i), predictors_in_model_zad1), drop = FALSE]


        # Uzupełnienie brakujących kolumn predyktorów, jeśli model ich wymaga, a nie ma ich w `nowe_dane_i` (np. jeśli Dzielnica wyleciała)
        # To jest skomplikowane, predict powinien sobie poradzić jeśli `Dzielnica` jako factor jest OK

        pred_i_zad1 <- tryCatch(predict(model_krokowy_zad1, newdata = nowe_dane_i), error = function(e) {
            warning(paste("Błąd predykcji dla i):", e$message)); NA
        })
        print(paste("i) Szacowana CenaM2 (Metraz=80, Pietro_1=1, Fabryczna):", round(pred_i_zad1, 2)))

        # ii) Metraż=65m, 3-cie piętro (Pietro_1=0), Krzyki (poziom referencyjny dla Dzielnica)
        nowe_dane_ii <- data.frame(Metraz = 65, Pietro_1 = 0)
         if ("Dzielnica" %in% names(model_krokowy_zad1$coefficients) || any(grepl("Dzielnica", names(model_krokowy_zad1$coefficients)))) {
            if ("Krzyki" %in% dzielnica_levels) {
                nowe_dane_ii$Dzielnica <- factor("Krzyki", levels = dzielnica_levels)
            } else {
                 warning("Poziom 'Krzyki' nie istnieje w oryginalnych danych. Używam pierwszego poziomu dla predykcji ii).")
                 nowe_dane_ii$Dzielnica <- factor(dzielnica_levels[1], levels = dzielnica_levels)
            }
        }
        nowe_dane_ii <- nowe_dane_ii[, intersect(names(nowe_dane_ii), predictors_in_model_zad1), drop = FALSE]

        pred_ii_zad1 <- tryCatch(predict(model_krokowy_zad1, newdata = nowe_dane_ii), error = function(e) {
            warning(paste("Błąd predykcji dla ii):", e$message)); NA
        })
        print(paste("ii) Szacowana CenaM2 (Metraz=65, Pietro_1=0, Krzyki):", round(pred_ii_zad1, 2)))
    } else {
        print("Nie udało się zbudować modelu krokowego lub model jest niepoprawny, predykcje nie są możliwe.")
    }

    # c) Wyznaczenie reszt (rezyduałów)
    if (exists("model_krokowy_zad1") && inherits(model_krokowy_zad1, "lm") && model_krokowy_zad1$df.residual > 0) {
        reszty_zad1 <- residuals(model_krokowy_zad1)
        print("c) Pierwsze 10 reszt z modelu (Zadanie 1):")
        print(head(reszty_zad1, 10))
    } else {
        print("Brak modelu lub model niepoprawny, nie można obliczyć reszt.")
        reszty_zad1 <- NULL
    }

    # d) Testowanie hipotezy o normalności rozkładu reszt
    print("d) Test normalności reszt dla modelu CenaM2:")
    # H0: Reszty mają rozkład normalny.
    # Ha: Reszty nie mają rozkładu normalnego. (Poziom istotności alfa = 0.05)
    if (!is.null(reszty_zad1) && length(na.omit(reszty_zad1)) >= 3) { # shapiro.test wymaga min 3 obserwacji bez NA
        reszty_zad1_clean <- na.omit(reszty_zad1)
        if (length(reszty_zad1_clean) > 5000) {
            print("Liczba reszt > 5000, używam testu Kołmogorowa-Smirnowa (Lilliefors) dla normalności.")
            if (requireNamespace("nortest", quietly = TRUE)) {
                test_normalnosci_reszt_zad1 <- nortest::lillie.test(reszty_zad1_clean)
            } else {
                print("Pakiet 'nortest' nie jest zainstalowany. Nie można przeprowadzić testu Lillieforsa.")
                test_normalnosci_reszt_zad1 <- list(p.value = NA)
            }
        } else if (length(reszty_zad1_clean) >=3) {
             test_normalnosci_reszt_zad1 <- shapiro.test(reszty_zad1_clean)
        } else {
            print("Za mało danych w resztach dla testu Shapiro-Wilka.")
            test_normalnosci_reszt_zad1 <- list(p.value = NA)
        }

        if (!is.na(test_normalnosci_reszt_zad1$p.value)) {
            print(test_normalnosci_reszt_zad1)
            if (test_normalnosci_reszt_zad1$p.value < 0.05) {
                print("P-wartość < 0.05, więc odrzucamy H0. Reszty nie mają rozkładu normalnego.")
            } else {
                print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Reszty mogą pochodzić z rozkładu normalnego.")
            }
        } else {
             print("Nie udało się przeprowadzić testu normalności reszt.")
        }

        if (interactive() && length(reszty_zad1_clean) > 0) {
            par(mfrow=c(1,2))
            hist(reszty_zad1_clean, main="Histogram reszt (Zad 1)", breaks=30, col="lightblue", xlab="Reszty")
            qqnorm(reszty_zad1_clean, main="Wykres Q-Q reszt (Zad 1)")
            qqline(reszty_zad1_clean, col="red")
            par(mfrow=c(1,1))
        }
    } else {
        print("Nie można przetestować normalności reszt (za mało danych lub brak reszt).")
    }
} # koniec bloku if dla Zadania 1


# --- Zadanie 2 ---
print("Zadanie 2: Model dla Cena")
required_vars_zad2 <- c("Cena", "Metraz", "Pietro_1", "Dzielnica")
if (!all(required_vars_zad2 %in% names(mieszkania_clean_df))) {
    print("Brak wszystkich wymaganych zmiennych dla Zadania 2 po czyszczeniu. Pomijam Zadanie 2.")
} else {
    # a) Regresja krokowa wsteczna dla zmiennej zależnej Cena
    formula_zad2_str <- "Cena ~ Metraz + Pietro_1"
    if (is.factor(mieszkania_clean_df$Dzielnica) && nlevels(droplevels(mieszkania_clean_df$Dzielnica)) >= 2) {
        formula_zad2_str <- paste(formula_zad2_str, "+ Dzielnica")
    } else {
        print("Zmienna Dzielnica ma mniej niż 2 poziomy w oczyszczonych danych lub nie jest faktorem. Pomijam Dzielnica w modelu Zad2.")
    }
    model_pelny_zad2 <- lm(as.formula(formula_zad2_str), data = mieszkania_clean_df)

    print("a) Budowa modelu dla Cena za pomocą regresji krokowej wstecznej:")
    if (length(coef(model_pelny_zad2)) > 1 && model_pelny_zad2$df.residual > 0) {
        lower_scope_zad2 <- lm(Cena ~ 1, data = mieszkania_clean_df)
        model_krokowy_zad2 <- step(model_pelny_zad2, direction = "backward", trace = 0, scope = list(lower=formula(lower_scope_zad2), upper=formula(model_pelny_zad2)))
        print("Podsumowanie wybranego modelu (Zadanie 2):")
        print(summary(model_krokowy_zad2))
    } else {
        print("Nie można przeprowadzić regresji krokowej - model pełny jest zbyt prosty, brak danych lub stopni swobody.")
        model_krokowy_zad2 <- model_pelny_zad2
    }

    # b) Oszacowanie średniej ceny (Y)
    print("b) Oszacowanie średniej Cena:")
    if (exists("model_krokowy_zad2") && inherits(model_krokowy_zad2, "lm") && model_krokowy_zad2$df.residual > 0) {
        dzielnica_levels <- if(is.factor(mieszkania_clean_df$Dzielnica)) levels(mieszkania_clean_df$Dzielnica) else "BrakDanych"
        predictors_in_model_zad2 <- all.vars(formula(model_krokowy_zad2)[-2])


        nowe_dane_i_zad2 <- data.frame(Metraz = 80, Pietro_1 = 1)
        if ("Dzielnica" %in% predictors_in_model_zad2) {
            if ("Fabryczna" %in% dzielnica_levels) {
                nowe_dane_i_zad2$Dzielnica <- factor("Fabryczna", levels = dzielnica_levels)
            } else {
                warning("Poziom 'Fabryczna' nie istnieje. Używam poziomu referencyjnego dla predykcji i) w Zad2.")
                nowe_dane_i_zad2$Dzielnica <- factor(dzielnica_levels[1], levels = dzielnica_levels)
            }
        }
        nowe_dane_i_zad2 <- nowe_dane_i_zad2[, intersect(names(nowe_dane_i_zad2), predictors_in_model_zad2), drop=FALSE]
        pred_i_zad2 <- tryCatch(predict(model_krokowy_zad2, newdata = nowe_dane_i_zad2), error = function(e) {
            warning(paste("Błąd predykcji dla i) Zad2:", e$message)); NA
        })
        print(paste("i) Szacowana Cena (Metraz=80, Pietro_1=1, Fabryczna):", round(pred_i_zad2, 0)))

        nowe_dane_ii_zad2 <- data.frame(Metraz = 65, Pietro_1 = 0)
        if ("Dzielnica" %in% predictors_in_model_zad2) {
           if ("Krzyki" %in% dzielnica_levels) {
               nowe_dane_ii_zad2$Dzielnica <- factor("Krzyki", levels = dzielnica_levels)
           } else {
               warning("Poziom 'Krzyki' nie istnieje. Używam poziomu referencyjnego dla predykcji ii) w Zad2.")
               nowe_dane_ii_zad2$Dzielnica <- factor(dzielnica_levels[1], levels = dzielnica_levels)
           }
        }
        nowe_dane_ii_zad2 <- nowe_dane_ii_zad2[, intersect(names(nowe_dane_ii_zad2), predictors_in_model_zad2), drop=FALSE]
        pred_ii_zad2 <- tryCatch(predict(model_krokowy_zad2, newdata = nowe_dane_ii_zad2), error = function(e) {
            warning(paste("Błąd predykcji dla ii) Zad2:", e$message)); NA
        })
        print(paste("ii) Szacowana Cena (Metraz=65, Pietro_1=0, Krzyki):", round(pred_ii_zad2, 0)))
    } else {
        print("Nie udało się zbudować modelu krokowego dla Cena lub model niepoprawny, predykcje nie są możliwe.")
    }

    # c) Wyznaczenie reszt (rezyduałów)
    if (exists("model_krokowy_zad2") && inherits(model_krokowy_zad2, "lm") && model_krokowy_zad2$df.residual > 0) {
        reszty_zad2 <- residuals(model_krokowy_zad2)
        print("c) Pierwsze 10 reszt z modelu (Zadanie 2):")
        print(head(reszty_zad2, 10))
    } else {
        print("Brak modelu dla Cena lub model niepoprawny, nie można obliczyć reszt.")
        reszty_zad2 <- NULL
    }

    # d) Testowanie hipotezy o normalności rozkładu reszt
    print("d) Test normalności reszt dla modelu Cena:")
    # H0: Reszty mają rozkład normalny.
    # Ha: Reszty nie mają rozkładu normalnego. (Poziom istotności alfa = 0.05)
    if (!is.null(reszty_zad2) && length(na.omit(reszty_zad2)) >= 3) {
        reszty_zad2_clean <- na.omit(reszty_zad2)
        if (length(reszty_zad2_clean) > 5000) {
            print("Liczba reszt > 5000, używam testu Kołmogorowa-Smirnowa (Lilliefors) dla normalności.")
            if (requireNamespace("nortest", quietly = TRUE)) {
                test_normalnosci_reszt_zad2 <- nortest::lillie.test(reszty_zad2_clean)
            } else {
                print("Pakiet 'nortest' nie jest zainstalowany. Nie można przeprowadzić testu Lillieforsa.")
                test_normalnosci_reszt_zad2 <- list(p.value = NA)
            }
        } else if (length(reszty_zad2_clean) >=3) {
            test_normalnosci_reszt_zad2 <- shapiro.test(reszty_zad2_clean)
        } else {
            print("Za mało danych w resztach dla testu Shapiro-Wilka.")
            test_normalnosci_reszt_zad2 <- list(p.value = NA)
        }

        if (!is.na(test_normalnosci_reszt_zad2$p.value)) {
            print(test_normalnosci_reszt_zad2)
            if (test_normalnosci_reszt_zad2$p.value < 0.05) {
                print("P-wartość < 0.05, więc odrzucamy H0. Reszty nie mają rozkładu normalnego.")
            } else {
                print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Reszty mogą pochodzić z rozkładu normalnego.")
            }
        } else {
            print("Nie udało się przeprowadzić testu normalności reszt.")
        }

        if (interactive() && length(reszty_zad2_clean) > 0) {
            par(mfrow=c(1,2))
            hist(reszty_zad2_clean, main="Histogram reszt (Zad 2)", breaks=30, col="lightgreen", xlab="Reszty")
            qqnorm(reszty_zad2_clean, main="Wykres Q-Q reszt (Zad 2)")
            qqline(reszty_zad2_clean, col="red")
            par(mfrow=c(1,1))
        }
    } else {
        print("Nie można przetestować normalności reszt (za mało danych lub brak reszt).")
    }
} # koniec bloku if dla Zadania 2


# --- Zadanie 3 ---
print("Zadanie 3: Model dla masy bakterii")
# UWAGA: Ten kod zakłada, że plik 'dane/bakteria.csv' istnieje i ma kolumny 'Czas' i 'Masa'.
# Użytkownik musi dostarczyć ten plik i ewentualnie dostosować nazwy kolumn.

sciezka_bakterie <- "dane/bakteria.csv"
# Pomijam sprawdzanie istnienia pliku 'bakteria.csv' zgodnie z życzeniem.
# Użytkownik musi się upewnić, że plik jest dostępny.
bakteria_df <- read.csv2(sciezka_bakterie) # Zakładamy format csv2 (średnik, przecinek dziesiętny)

# Sprawdzenie i konwersja kolumn - zakładane nazwy to 'Czas' i 'Masa'
# Dostosuj nazwy kolumn, jeśli są inne w Twoim pliku!
col_czas <- "Czas" # Zmień, jeśli Twoja kolumna czasu nazywa się inaczej
col_masa <- "Masa" # Zmień, jeśli Twoja kolumna masy nazywa się inaczej

if (!(col_czas %in% names(bakteria_df)) || !(col_masa %in% names(bakteria_df))) {
    print(paste("Brak wymaganych kolumn ('", col_czas, "' lub '", col_masa, "') w pliku bakteria.csv. Pomijam Zadanie 3.", sep=""))
} else {
    # Konwersja na numeryczne, jeśli potrzeba
    if (!is.numeric(bakteria_df[[col_czas]])) {
        bakteria_df[[col_czas]] <- as.numeric(gsub(",", ".", bakteria_df[[col_czas]]))
    }
    if (!is.numeric(bakteria_df[[col_masa]])) {
        bakteria_df[[col_masa]] <- as.numeric(gsub(",", ".", bakteria_df[[col_masa]]))
    }

    bakteria_df_clean <- na.omit(bakteria_df[, c(col_czas, col_masa)])

    if(nrow(bakteria_df_clean) < 2) {
        print("Za mało danych w pliku bakteria.csv po usunięciu NA. Pomijam Zadanie 3.")
    } else {
        # a) Sporządzenie wykresu rozrzutu masy bakterii względem czasu
        print("a) Wykres rozrzutu masy bakterii względem czasu:")
        if (interactive()) {
            plot(bakteria_df_clean[[col_masa]] ~ bakteria_df_clean[[col_czas]],
                 xlab = col_czas, ylab = col_masa,
                 main = "Rozrzut masy bakterii względem czasu", pch = 19, col = "blue")
        } else {
            print("Wykres rozrzutu nie zostanie wyświetlony w trybie nieinteraktywnym.")
        }

        # b) Budowa modelu regresji liniowej
        print("b) Model regresji liniowej: Masa ~ Czas")
        # Tworzymy formułę dynamicznie
        formula_zad3 <- as.formula(paste(col_masa, "~", col_czas))
        model_bakterie <- lm(formula_zad3, data = bakteria_df_clean)
        print(summary(model_bakterie))

        # c) Obliczenie współczynnika determinacji R^2
        print("c) Współczynnik determinacji R^2:")
        r_squared_zad3 <- summary(model_bakterie)$r.squared
        print(paste("R^2 =", round(r_squared_zad3, 4)))
        adj_r_squared_zad3 <- summary(model_bakterie)$adj.r.squared
        print(paste("Skorygowany R^2 =", round(adj_r_squared_zad3, 4)))


        # d) Czy model ten jest dobrze dopasowany do danych?
        print("d) Ocena dopasowania modelu:")
        print(paste("Współczynnik R^2 wynoszący", round(r_squared_zad3, 4), "wskazuje, że model wyjaśnia",
                    round(r_squared_zad3 * 100, 2), "% wariancji w masie bakterii."))
        print("Należy również ocenić wykresy diagnostyczne reszt (np. reszty vs dopasowane, Q-Q plot reszt).")
        if (interactive()) {
            par(mfrow=c(2,2))
            plot(model_bakterie) # Standardowe wykresy diagnostyczne dla lm
            par(mfrow=c(1,1))
        }

        # e) Testowanie hipotezy, iż reszty mają rozkład normalny
        print("e) Test normalności reszt (Shapiro-Wilk) dla modelu masy bakterii:")
        reszty_zad3 <- residuals(model_bakterie)
        # H0: Reszty mają rozkład normalny.
        # Ha: Reszty nie mają rozkładu normalnego. (Poziom istotności alfa = 0.05)
        if (length(na.omit(reszty_zad3)) >= 3) {
            reszty_zad3_clean <- na.omit(reszty_zad3)
             if (length(reszty_zad3_clean) > 5000) {
                print("Liczba reszt > 5000, używam testu Kołmogorowa-Smirnowa (Lilliefors) dla normalności.")
                if (requireNamespace("nortest", quietly = TRUE)) {
                    test_normalnosci_reszt_zad3 <- nortest::lillie.test(reszty_zad3_clean)
                } else {
                    print("Pakiet 'nortest' nie jest zainstalowany. Nie można przeprowadzić testu Lillieforsa.")
                    test_normalnosci_reszt_zad3 <- list(p.value = NA)
                }
            } else if (length(reszty_zad3_clean) >=3) {
                test_normalnosci_reszt_zad3 <- shapiro.test(reszty_zad3_clean)
            } else {
                print("Za mało danych w resztach dla testu Shapiro-Wilka.")
                test_normalnosci_reszt_zad3 <- list(p.value = NA)
            }

            if (!is.na(test_normalnosci_reszt_zad3$p.value)) {
                print(test_normalnosci_reszt_zad3)
                if (test_normalnosci_reszt_zad3$p.value < 0.05) {
                    print("P-wartość < 0.05, więc odrzucamy H0. Reszty nie mają rozkładu normalnego.")
                } else {
                    print("P-wartość >= 0.05, więc brak podstaw do odrzucenia H0. Reszty mogą pochodzić z rozkładu normalnego.")
                }
            } else {
                 print("Nie udało się przeprowadzić testu normalności reszt.")
            }
        } else {
            print("Nie można przetestować normalności reszt (za mało danych lub brak reszt).")
        }

        # f) Oszacowanie średniej masy bakterii po upływie 12 godzin
        print("f) Oszacowanie średniej masy bakterii po 12 godzinach:")
        czas_predykcji <- 12
        nowe_dane_bakterie <- data.frame(Czas = czas_predykcji)
        # Zmiana nazwy kolumny w 'nowe_dane_bakterie' aby pasowała do modelu
        names(nowe_dane_bakterie) <- col_czas

        pred_masa_12h <- predict(model_bakterie, newdata = nowe_dane_bakterie)
        print(paste("Szacowana średnia masa bakterii po", czas_predykcji, "godzinach:", round(pred_masa_12h, 4)))
    }
}
print("--- Koniec Zadania 3 ---")