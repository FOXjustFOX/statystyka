# Laboratorium 10 - Testy dla Proporcji oraz Różnicy między Średnimi
# Plik waga1.csv: separator ';', płeć: 0-M, 1-K
# Oczekiwane nagłówki w CSV: plec;Wzrost;Waga_przed;Waga_po

# install.packages("boot") # Odkomentuj, jeśli pakiet 'boot' nie jest zainstalowany
library(boot)

# Funkcja pomocnicza do drukowania separatora
print_separator <- function(exercise_number = NULL) {
  if (!is.null(exercise_number)) {
    cat(paste0("\n------------------------------\nZadanie ", exercise_number, "\n------------------------------\n"))
  } else {
    cat("\n------------------------------\n")
  }
}

# --- Zadanie 1 ---
print_separator(1)
# a) Test H0: 40% populacji ma wyższe wykształcenie.
cat("a)\n")

# i) Test Z
cat("  i) Test Z (ręcznie):\n")
n_ankieta <- 1000
x_wyksztalcenie <- 385
p0_wyksztalcenie <- 0.40

p_hat_wyksztalcenie <- x_wyksztalcenie / n_ankieta
z_stat_wyksztalcenie <- (p_hat_wyksztalcenie - p0_wyksztalcenie) / sqrt(p0_wyksztalcenie * (1 - p0_wyksztalcenie) / n_ankieta)
p_value_z_wyksztalcenie <- 2 * pnorm(-abs(z_stat_wyksztalcenie))
cat("     p-value =", p_value_z_wyksztalcenie, "\n")

alpha <- 0.05
if (p_value_z_wyksztalcenie < alpha) {
  cat("     Odrzucamy H0: Proporcja różni się od 40%.\n")
} else {
  cat("     Brak podstaw do odrzucenia H0.\n")
}

# ii) Polecenie "prop.test"
cat("  ii) prop.test:\n")
wynik_prop_test_wyksztalcenie <- prop.test(x = x_wyksztalcenie, n = n_ankieta, p = p0_wyksztalcenie, correct = FALSE)
cat("     p-value =", wynik_prop_test_wyksztalcenie$p.value, "\n")

# b) Test H0: Prawdopodobieństwo ukończenia studiów nie zależy od płci.
cat("b)\n")
n_kobiety_ankieta <- 520
x_kobiety_wykszt <- 220
n_mezczyzni_ankieta <- 480
x_mezczyzni_wykszt <- 165

# i) Test Z dla dwóch proporcji
cat("  i) Test Z dla 2 proporcji (ręcznie):\n")
p_hat_kobiety_wykszt <- x_kobiety_wykszt / n_kobiety_ankieta
p_hat_mezczyzni_wykszt <- x_mezczyzni_wykszt / n_mezczyzni_ankieta
p_pooled_wykszt <- (x_kobiety_wykszt + x_mezczyzni_wykszt) / (n_kobiety_ankieta + n_mezczyzni_ankieta)
z_stat_plec_wykszt <- (p_hat_kobiety_wykszt - p_hat_mezczyzni_wykszt) /
  sqrt(p_pooled_wykszt * (1 - p_pooled_wykszt) * (1/n_kobiety_ankieta + 1/n_mezczyzni_ankieta))
p_value_z_plec_wykszt <- 2 * pnorm(-abs(z_stat_plec_wykszt))
cat("     p-value =", p_value_z_plec_wykszt, "\n")

if (p_value_z_plec_wykszt < alpha) {
  cat("     Odrzucamy H0: Prawdopodobieństwo zależy od płci.\n")
} else {
  cat("     Brak podstaw do odrzucenia H0.\n")
}

# ii) Polecenie "prop.test"
cat("  ii) prop.test dla 2 proporcji:\n")
wynik_prop_test_plec_wykszt <- prop.test(x = c(x_kobiety_wykszt, x_mezczyzni_wykszt), n = c(n_kobiety_ankieta, n_mezczyzni_ankieta), correct = FALSE)
cat("     p-value =", wynik_prop_test_plec_wykszt$p.value, "\n")

# c) Test H0: Średni wzrost nie zależy od płci (Test Z).
cat("c)\n")
cat("  i) Test Z dla różnicy średnich (ręcznie):\n")
n_k_wzrost <- 520; srednia_k_wzrost <- 166; wariancja_k_wzrost <- 100
n_m_wzrost <- 480; srednia_m_wzrost <- 174; wariancja_m_wzrost <- 121

z_stat_wzrost_plec <- (srednia_k_wzrost - srednia_m_wzrost) /
  sqrt(wariancja_k_wzrost/n_k_wzrost + wariancja_m_wzrost/n_m_wzrost)
p_value_z_wzrost_plec <- 2 * pnorm(-abs(z_stat_wzrost_plec))
cat("     p-value =", p_value_z_wzrost_plec, "\n")

if (p_value_z_wzrost_plec < alpha) {
  cat("     Odrzucamy H0: Średni wzrost zależy od płci.\n")
} else {
  cat("     Brak podstaw do odrzucenia H0.\n")
}

# Wczytanie danych z pliku waga1.csv
cat("\n--- Wczytywanie danych z pliku waga1.csv ---\n")
dane_waga <- read.csv2("waga1.csv") 
cat("Dane wczytane.\n")

# Tworzenie opisowej kolumny płci
dane_waga$plec_opis <- factor(dane_waga$plec, levels = c(0, 1), labels = c("Mężczyzna", "Kobieta"))

# --- Zadanie 2 ---
print_separator(2)
# Test H0: Proporcja kobiet wśród studentów wynosi 0,5.
n_studenci <- nrow(dane_waga)
liczba_kobiet_stud <- sum(dane_waga$plec == 1, na.rm = TRUE)
p0_kobiety_stud <- 0.5
p_hat_kobiety_stud <- liczba_kobiet_stud / n_studenci

# a) Test Z
cat("a) Test Z (ręcznie):\n")
z_stat_kobiety_stud <- (p_hat_kobiety_stud - p0_kobiety_stud) /
  sqrt(p0_kobiety_stud * (1 - p0_kobiety_stud) / n_studenci)
p_value_z_kobiety_stud <- 2 * pnorm(-abs(z_stat_kobiety_stud))
cat("   p-value =", p_value_z_kobiety_stud, "\n")
if (p_value_z_kobiety_stud < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }

# b) Polecenie "prop.test"
cat("b) prop.test:\n")
wynik_prop_test_kobiety_stud <- prop.test(x = liczba_kobiet_stud, n = n_studenci, p = p0_kobiety_stud, correct = FALSE)
cat("   p-value =", wynik_prop_test_kobiety_stud$p.value, "\n")

# --- Zadanie 3 ---
print_separator(3)
# Test H0: Średnia waga po studiach nie zależy od płci.
waga_po_kobiety <- dane_waga$Waga_po[dane_waga$plec == 1 & !is.na(dane_waga$Waga_po)]
waga_po_mezczyzni <- dane_waga$Waga_po[dane_waga$plec == 0 & !is.na(dane_waga$Waga_po)]

if(length(waga_po_kobiety) < 2 || length(waga_po_mezczyzni) < 2) {
    cat("Za mało danych (po usunięciu NA) dla jednej lub obu grup płci w kolumnie 'Waga_po' do przeprowadzenia testów w Zadaniu 3.\n")
} else {
    # a) Test Z
    cat("a) Test Z (ręcznie):\n")
    n_k_waga_po <- length(waga_po_kobiety); srednia_k_waga_po <- mean(waga_po_kobiety, na.rm = TRUE); wariancja_k_waga_po <- var(waga_po_kobiety, na.rm = TRUE)
    n_m_waga_po <- length(waga_po_mezczyzni); srednia_m_waga_po <- mean(waga_po_mezczyzni, na.rm = TRUE); wariancja_m_waga_po <- var(waga_po_mezczyzni, na.rm = TRUE)
    
    if(is.na(wariancja_k_waga_po) || is.na(wariancja_m_waga_po) || wariancja_k_waga_po == 0 || wariancja_m_waga_po == 0 || n_k_waga_po == 0 || n_m_waga_po == 0){
         cat("   Nie można obliczyć statystyki Z - wariancja jednej z grup jest NA lub zero, lub brak obserwacji w grupie.\n")
    } else {
        z_stat_waga_po_plec <- (srednia_k_waga_po - srednia_m_waga_po) /
          sqrt(wariancja_k_waga_po/n_k_waga_po + wariancja_m_waga_po/n_m_waga_po)
        p_value_z_waga_po_plec <- 2 * pnorm(-abs(z_stat_waga_po_plec))
        cat("   p-value =", p_value_z_waga_po_plec, "\n")
        if (p_value_z_waga_po_plec < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }
    }

    # b) Polecenie "t.test"
    cat("b) t.test:\n")
    wynik_ttest_waga_po_plec <- t.test(waga_po_kobiety, waga_po_mezczyzni)
    cat("   p-value =", wynik_ttest_waga_po_plec$p.value, "\n")
}

# --- Zadanie 4 ---
print_separator(4)
# Test H0: Proporcja K ważących >70kg po studiach = proporcji M ważących >70kg po studiach.
prog_wagi <- 70
liczba_k_nad_prog <- sum(dane_waga$plec == 1 & dane_waga$Waga_po > prog_wagi, na.rm = TRUE)
n_k_ogolem <- sum(dane_waga$plec == 1, na.rm = TRUE)

liczba_m_nad_prog <- sum(dane_waga$plec == 0 & dane_waga$Waga_po > prog_wagi, na.rm = TRUE)
n_m_ogolem <- sum(dane_waga$plec == 0, na.rm = TRUE)

if (n_k_ogolem == 0 || n_m_ogolem == 0) {
    cat("Brak obserwacji dla jednej z płci w Zadaniu 4.\n")
} else {
    prop_k_nad_prog <- ifelse(n_k_ogolem > 0, liczba_k_nad_prog / n_k_ogolem, NA) # Zabezpieczenie przed dzieleniem przez 0
    prop_m_nad_prog <- ifelse(n_m_ogolem > 0, liczba_m_nad_prog / n_m_ogolem, NA) # Zabezpieczenie przed dzieleniem przez 0


    # a) Test Z
    cat("a) Test Z (ręcznie):\n")
    if (!is.na(prop_k_nad_prog) && !is.na(prop_m_nad_prog)) { # Kontynuuj tylko jeśli proporcje są obliczalne
        p_pooled_nad_prog <- (liczba_k_nad_prog + liczba_m_nad_prog) / (n_k_ogolem + n_m_ogolem)
        if (!is.na(p_pooled_nad_prog) && p_pooled_nad_prog > 0 && p_pooled_nad_prog < 1 && n_k_ogolem > 0 && n_m_ogolem > 0) {
            z_stat_prop_nad_prog <- (prop_k_nad_prog - prop_m_nad_prog) /
              sqrt(p_pooled_nad_prog * (1 - p_pooled_nad_prog) * (1/n_k_ogolem + 1/n_m_ogolem))
            p_value_z_prop_nad_prog <- 2 * pnorm(-abs(z_stat_prop_nad_prog))
            cat("   p-value =", p_value_z_prop_nad_prog, "\n")
            if (p_value_z_prop_nad_prog < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }
        } else {
            cat("   Nie można przeprowadzić testu Z dla Zadania 4a - wspólna proporcja jest NA, 0 lub 1, lub brak obserwacji w jednej z grup.\n")
        }
    } else {
         cat("   Nie można obliczyć proporcji dla jednej z grup w Zadaniu 4a.\n")
    }


    # b) Polecenie "prop.test"
    cat("b) prop.test:\n")
    wynik_prop_test_nad_prog <- prop.test(x = c(liczba_k_nad_prog, liczba_m_nad_prog), n = c(n_k_ogolem, n_m_ogolem), correct = FALSE)
    cat("   p-value =", wynik_prop_test_nad_prog$p.value, "\n")

    # c) Bootstrap - przedział ufności 95% dla różnicy proporcji
    cat("c) Bootstrap - przedział ufności 95% dla różnicy proporcji (K-M):\n")
    roznica_proporcji_boot <- function(data, indices) {
      próbka <- data[indices, ]; 
      k_ogolem_boot <- sum(próbka$plec == 1, na.rm=T); 
      m_ogolem_boot <- sum(próbka$plec == 0, na.rm=T)
      
      if (k_ogolem_boot == 0 || m_ogolem_boot == 0) { return(NA) }
      
      k_nad_prog_boot <- sum(próbka$Waga_po[próbka$plec == 1 & !is.na(próbka$Waga_po)] > prog_wagi, na.rm=T)
      m_nad_prog_boot <- sum(próbka$Waga_po[próbka$plec == 0 & !is.na(próbka$Waga_po)] > prog_wagi, na.rm=T)
      
      prop_k_boot <- ifelse(k_ogolem_boot > 0, k_nad_prog_boot / k_ogolem_boot, NA)
      prop_m_boot <- ifelse(m_ogolem_boot > 0, m_nad_prog_boot / m_ogolem_boot, NA)

      if(is.na(prop_k_boot) || is.na(prop_m_boot)) return(NA)
      return(prop_k_boot - prop_m_boot)
    }
    set.seed(123)
    # Używamy oryginalnych nazw kolumn z pliku CSV w selekcji dla bootstrapu
    dane_waga_boot_4c_filtered <- dane_waga[!is.na(dane_waga$Waga_po) & !is.na(dane_waga$plec), c("Waga_po", "plec")]
    
    if(nrow(dane_waga_boot_4c_filtered[dane_waga_boot_4c_filtered$plec == 1,]) > 1 && 
       nrow(dane_waga_boot_4c_filtered[dane_waga_boot_4c_filtered$plec == 0,]) > 1){
      bootstrap_wynik_4c <- boot(data = dane_waga_boot_4c_filtered, statistic = roznica_proporcji_boot, R = 2000)
      przedzial_ufnosci_boot_4c <- boot.ci(bootstrap_wynik_4c, conf = 0.95, type = "perc")
      cat("   Wyniki bootstrapu:\n")
      print(przedzial_ufnosci_boot_4c)
    } else { cat("   Za mało danych dla bootstrapu w zad 4c po usunięciu NA lub dla jednej z płci.\n") }
}

# --- Zadanie 5 ---
print_separator(5)
# Test H0: Średnio mężczyźni są o 5cm wyżsi niż kobiety (mu_m - mu_k = 5).
wzrost_kobiety <- dane_waga$Wzrost[dane_waga$plec == 1 & !is.na(dane_waga$Wzrost)]
wzrost_mezczyzni <- dane_waga$Wzrost[dane_waga$plec == 0 & !is.na(dane_waga$Wzrost)]

if(length(wzrost_kobiety) < 2 || length(wzrost_mezczyzni) < 2){
    cat("Za mało danych (po usunięciu NA) dla 'Wzrost' w Zadaniu 5.\n")
} else {
    cat("Test t dla hipotezy, że mężczyźni są średnio o 5cm wyżsi:\n")
    wynik_ttest_wzrost_roznica <- t.test(wzrost_mezczyzni, wzrost_kobiety, mu = 5, alternative = "two.sided")
    cat("   p-value =", wynik_ttest_wzrost_roznica$p.value, "\n")
    if (wynik_ttest_wzrost_roznica$p.value < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }
}

# --- Zadanie 6 ---
print_separator(6)
# Test H0: 80% studentów przybiera na wadze w trakcie studiów.
dane_waga$przyrost_wagi <- dane_waga$Waga_po - dane_waga$Waga_przed # Używamy Waga_po i Waga_przed
liczba_studentow_przybralo <- sum(dane_waga$przyrost_wagi > 0, na.rm = TRUE)
n_wszyscy_studenci_waga <- sum(!is.na(dane_waga$przyrost_wagi)) 

p0_przyrost <- 0.80
if (n_wszyscy_studenci_waga > 0) {
  cat("Test prop.test dla hipotezy, że 80% studentów przybiera na wadze:\n")
  wynik_prop_test_przyrost <- prop.test(x = liczba_studentow_przybralo, n = n_wszyscy_studenci_waga, p = p0_przyrost, correct = FALSE)
  cat("   p-value =", wynik_prop_test_przyrost$p.value, "\n")
  if (wynik_prop_test_przyrost$p.value < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }
} else { cat("Za mało danych (brak kompletnych par Waga_przed/Waga_po po usunięciu NA) dla zad 6.\n") }


# --- Zadanie 7 ---
print_separator(7)
prog_wzrostu_7 <- 170
# a) Test H0: Proporcja studentek >170cm = proporcji studentów >170cm.
cat("a) Test prop.test dla różnicy proporcji studentek i studentów >170cm:\n")
liczba_k_wyzsze_7 <- sum(dane_waga$plec == 1 & dane_waga$Wzrost > prog_wzrostu_7, na.rm = TRUE) # Używamy Wzrost
n_k_ogolem_7 <- sum(dane_waga$plec == 1, na.rm = TRUE)
liczba_m_wyzsi_7 <- sum(dane_waga$plec == 0 & dane_waga$Wzrost > prog_wzrostu_7, na.rm = TRUE) # Używamy Wzrost
n_m_ogolem_7 <- sum(dane_waga$plec == 0, na.rm = TRUE)

if (n_k_ogolem_7 > 0 && n_m_ogolem_7 > 0) {
  wynik_prop_test_wzrost_7 <- prop.test(x = c(liczba_k_wyzsze_7, liczba_m_wyzsi_7), n = c(n_k_ogolem_7, n_m_ogolem_7), correct = FALSE)
  cat("   p-value =", wynik_prop_test_wzrost_7$p.value, "\n")
  if (wynik_prop_test_wzrost_7$p.value < alpha) { cat("   Odrzucamy H0.\n") } else { cat("   Brak podstaw do odrzucenia H0.\n") }
} else { cat("   Za mało danych dla jednej z płci w zad 7a.\n") }

# b) Bootstrap - przedział ufności 98% dla różnicy proporcji (studentki >170cm - studenci >170cm).
cat("b) Bootstrap - przedział ufności 98% dla różnicy proporcji (K-M) >170cm:\n")
roznica_proporcji_wzrost_boot <- function(data, indices) {
  próbka <- data[indices, ]; 
  k_ogolem_boot_7b <- sum(próbka$plec == 1, na.rm = TRUE); 
  m_ogolem_boot_7b <- sum(próbka$plec == 0, na.rm = TRUE)
  if (k_ogolem_boot_7b == 0 || m_ogolem_boot_7b == 0) { return(NA) }
  
  # Używamy Wzrost wewnątrz funkcji bootstrap, ponieważ dane przekazywane do 'boot' będą miały tę nazwę kolumny
  k_wyzsze_boot_7b <- sum(próbka$Wzrost[próbka$plec == 1 & !is.na(próbka$Wzrost)] > prog_wzrostu_7, na.rm = TRUE)
  m_wyzsi_boot_7b <- sum(próbka$Wzrost[próbka$plec == 0 & !is.na(próbka$Wzrost)] > prog_wzrostu_7, na.rm = TRUE)
  
  prop_k_boot_7b <- ifelse(k_ogolem_boot_7b > 0, k_wyzsze_boot_7b / k_ogolem_boot_7b, NA)
  prop_m_boot_7b <- ifelse(m_ogolem_boot_7b > 0, m_wyzsi_boot_7b / m_ogolem_boot_7b, NA)
  
  if(is.na(prop_k_boot_7b) || is.na(prop_m_boot_7b)) return(NA)
  return(prop_k_boot_7b - prop_m_boot_7b)
}
set.seed(456)
# Używamy oryginalnych nazw kolumn z pliku CSV w selekcji dla bootstrapu
dane_waga_boot_7b_filtered <- dane_waga[!is.na(dane_waga$Wzrost) & !is.na(dane_waga$plec), c("Wzrost", "plec")]
    
if(nrow(dane_waga_boot_7b_filtered[dane_waga_boot_7b_filtered$plec == 1,]) > 1 && 
   nrow(dane_waga_boot_7b_filtered[dane_waga_boot_7b_filtered$plec == 0,]) > 1){
  bootstrap_wynik_7b <- boot(data = dane_waga_boot_7b_filtered, statistic = roznica_proporcji_wzrost_boot, R = 2500)
  przedzial_ufnosci_boot_7b <- boot.ci(bootstrap_wynik_7b, conf = 0.98, type = "perc")
  cat("   Wyniki bootstrapu:\n")
  print(przedzial_ufnosci_boot_7b)
} else { cat("   Za mało danych dla bootstrapu w zad 7b po usunięciu NA lub dla jednej z płci.\n") }