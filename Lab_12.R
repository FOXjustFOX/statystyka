# Załadowanie potrzebnych bibliotek (jeśli jakieś są potrzebne, np. do czytania Excela, ale tu CSV)
# install.packages("dplyr") # Jeśli nie masz dplyr
# library(dplyr)

# --- Zadanie 1 ---
print("Zadanie 1")

# Dane wejściowe
polacy <- c(68, 80, 74, 62)
brytyjczycy <- c(85, 67, 79, 73)
chinczycy <- c(60, 66, 57)

# a) Wypełnienie tablicy ANOVA (procedura z wykładu)

# Połączone dane
wszystkie_wagi <- c(polacy, brytyjczycy, chinczycy)
narodowosci <- factor(c(rep("Polacy", length(polacy)),
                        rep("Brytyjczycy", length(brytyjczycy)),
                        rep("Chinczycy", length(chinczycy))))

N <- length(wszystkie_wagi) # Całkowita liczba obserwacji
k <- length(levels(narodowosci)) # Liczba grup

# Średnie
srednia_ogolna <- mean(wszystkie_wagi)
srednie_grupowe <- tapply(wszystkie_wagi, narodowosci, mean)

# Sumy kwadratów
# Suma kwadratów między grupami (MG)
SS_MG <- sum(sapply(levels(narodowosci), function(grupa) {
  length(wszystkie_wagi[narodowosci == grupa]) * (srednie_grupowe[grupa] - srednia_ogolna)^2
}))

# Suma kwadratów wewnątrz grup (WG)
SS_WG <- sum(sapply(levels(narodowosci), function(grupa) {
  sum((wszystkie_wagi[narodowosci == grupa] - srednie_grupowe[grupa])^2)
}))

# Całkowita suma kwadratów (C)
# Zgodnie z tożsamością ANOVA, SS_C = SS_MG + SS_WG
# Alternatywnie: SS_C <- sum((wszystkie_wagi - srednia_ogolna)^2)
# Sprawdźmy: sum((wszystkie_wagi - srednia_ogolna)^2) daje 95942/121 ~ 792.9091
# SS_MG (4300/11) + SS_WG (402) = (47300+48642)/121 = 95942/121. Zgadza się.
SS_C <- SS_MG + SS_WG

# Stopnie swobody
df_MG <- k - 1
df_WG <- N - k
df_C <- N - 1 # lub df_MG + df_WG

# Średnie kwadratów
MS_MG <- SS_MG / df_MG
MS_WG <- SS_WG / df_WG
# MS_C nie jest typowo używane do testu F, ale można obliczyć jako SS_C / df_C
MS_C <- SS_C / df_C


# Tworzenie tablicy ANOVA jako data frame
tabela_anova_manualna <- data.frame(
  "Suma kwadratów" = c(SS_MG, SS_WG, SS_C),
  "Stopnie swobody" = c(df_MG, df_WG, df_C),
  "Średnia kwadratów" = c(MS_MG, MS_WG, MS_C) # Można dać NA dla MS_C jeśli tak jest na wykładzie
)
rownames(tabela_anova_manualna) <- c("MG (Między grupami)", "WG (Wewnątrz grup)", "C (Całkowita)")

print("a) Wypełniona tablica ANOVA (manualnie):")
print(tabela_anova_manualna)


# b) Wyznaczenie realizacji statystyki testowej F
statystyka_F_manualna <- MS_MG / MS_WG
print(paste("b) Realizacja statystyki testowej F (manualnie):", round(statystyka_F_manualna, 4)))


# c) Weryfikacja hipotezy na poziomie istotności 5%
alfa <- 0.05
wartosc_krytyczna_F <- qf(1 - alfa, df_MG, df_WG)
print(paste("c) Wartość krytyczna F(", df_MG, ",", df_WG, ") dla alfa = 0.05:", round(wartosc_krytyczna_F, 4)))

if (statystyka_F_manualna > wartosc_krytyczna_F) {
  print("Statystyka F jest większa od wartości krytycznej. Odrzucamy H0.")
  print("Wnioskujemy, że waga zależy od narodowości.")
} else {
  print("Statystyka F nie jest większa od wartości krytycznej. Brak podstaw do odrzucenia H0.")
  print("Wnioskujemy, że waga nie zależy od narodowości.")
}

# Alternatywnie, używając p-wartości
p_wartosc_manualna <- pf(statystyka_F_manualna, df_MG, df_WG, lower.tail = FALSE)
print(paste("P-wartość (manualnie):", round(p_wartosc_manualna, 4)))

if (p_wartosc_manualna < alfa) {
  print("P-wartość jest mniejsza od alfa. Odrzucamy H0.")
  print("Wnioskujemy, że waga zależy od narodowości.")
} else {
  print("P-wartość nie jest mniejsza od alfa. Brak podstaw do odrzucenia H0.")
  print("Wnioskujemy, że waga nie zależy od narodowości.")
}


# d) Weryfikacja hipotezy za pomocą polecenia aov
dane_zad1 <- data.frame(waga = wszystkie_wagi, narodowosc = narodowosci)
model_aov_zad1 <- aov(waga ~ narodowosc, data = dane_zad1)
wynik_aov_zad1 <- summary(model_aov_zad1)

print("d) Weryfikacja hipotezy za pomocą polecenia aov:")
print(wynik_aov_zad1)
# Porównanie F i p-wartości z summary(model_aov_zad1) z obliczeniami manualnymi


# --- Zadanie 2 ---
print("Zadanie 2")

# Wczytanie danych. Zakładamy, że plik jest w podkatalogu 'dane' względem katalogu roboczego.
# Używamy read.csv2, ponieważ plik CSV jest oddzielony średnikami i używa przecinka jako separatora dziesiętnego.
# Jeśli plik używa kropki jako separatora dziesiętnego, użyj read.csv(..., sep = ";", dec = ".")
# Sprawdź nazwy kolumn po wczytaniu, np. przez names(mieszkania_df)
# Tutaj zakładam, że kolumny to "Metraz" i "Dzielnica"
 scieżka_pliku <- "dane/mieszkania.csv"
 mieszkania_df <- read.csv2(scieżka_pliku)

# a) Przetestowanie hipotezy, że metraż nie zależy od dzielnicy
# Upewnijmy się, że Dzielnica jest traktowana jako factor
mieszkania_df$Dzielnica <- as.factor(mieszkania_df$Dzielnica)
# Sprawdzenie, czy kolumna Metraz jest numeryczna (jeśli nie, trzeba ją skonwertować)
if(!is.numeric(mieszkania_df$Metraz)) {
  mieszkania_df$Metraz <- as.numeric(gsub(",", ".", mieszkania_df$Metraz)) # Zamiana przecinków na kropki i konwersja
}


print("a) Testowanie hipotezy: metraż vs dzielnica (ANOVA)")
# Hipotezy:
# H0: Średni metraż jest taki sam we wszystkich dzielnicach.
# H1: Średni metraż różni się w przynajmniej jednej dzielnicy.

model_aov_metraz <- aov(Metraz ~ Dzielnica, data = mieszkania_df)
wynik_aov_metraz <- summary(model_aov_metraz)
print(wynik_aov_metraz)

# Wnioskowanie na podstawie p-wartości z wyniku ANOVA
# (Zakładając standardowy poziom istotności alfa = 0.05)
p_wartosc_metraz <- wynik_aov_metraz[[1]][["Pr(>F)"]][1]
if (p_wartosc_metraz < 0.05) {
  print("Odrzucamy H0: Istnieją statystycznie istotne różnice w średnim metrażu między dzielnicami.")
} else {
  print("Brak podstaw do odrzucenia H0: Nie stwierdzono statystycznie istotnych różnic w średnim metrażu między dzielnicami.")
}


# b) Porównanie rejonów parami (test post-hoc, np. Tukey HSD)
print("b) Porównanie rejonów parami (Tukey HSD)")
test_tukey_metraz <- TukeyHSD(model_aov_metraz)
print(test_tukey_metraz)
print("Interpretacja wyników TukeyHSD:")
print("Każdy wiersz w tabeli 'diff' pokazuje różnicę średnich między dwoma dzielnicami.")
print("'lwr' i 'upr' to dolna i górna granica przedziału ufności dla tej różnicy.")
print("'p adj' to skorygowana p-wartość dla porównania. Jeśli 'p adj' < 0.05 (dla alfa=0.05), różnica jest istotna statystycznie.")


# --- Zadanie 3 ---
print("Zadanie 3")

# a) Podzielenie mieszkań na 4 kategorie liczby pokoi
# Zakładam, że kolumna z liczbą pokoi nazywa się "Pokoje"
# Kategorie: 1-pokojowe, 2-pokojowe, 3-pokojowe, 4 i więcej pokojowe

# Sprawdzenie, czy kolumna Pokoje jest numeryczna (jeśli nie, trzeba ją skonwertować)
if(!is.numeric(mieszkania_df$Pokoje)) {
 mieszkania_df$Pokoje <- as.numeric(mieszkania_df$Pokoje)
}

mieszkania_df$KategoriaPokoi <- cut(mieszkania_df$Pokoje,
                                    breaks = c(0, 1, 2, 3, Inf),
                                    labels = c("1-pokojowe", "2-pokojowe", "3-pokojowe", "4 i więcej"),
                                    right = TRUE, include.lowest = TRUE)

print("a) Dodano nową kolumnę 'KategoriaPokoi'. Oto pierwsze kilka wierszy z nową kolumną:")
print(head(mieszkania_df[, c("Pokoje", "KategoriaPokoi")]))
print("Liczebność mieszkań w poszczególnych kategoriach:")
print(table(mieszkania_df$KategoriaPokoi))

# Zadanie 3a prosi tylko o podział. Jeśli potrzebne są dalsze analizy (np. ANOVA ceny od KategoriaPokoi),
# można je dodać tutaj.