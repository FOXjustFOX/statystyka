# Zadanie 1
# a) rama danych oceny
oceny <- data.frame(
    Imie = c("Krzysztof", "Maria", "Henryk", "Anna"),
    Plec = c("m", "k", "m", "k"),
    Analiza = c(3.5, 4.5, 5.0, 4.5),
    Algebra = c(4.0, 5.0, 4.0, 3.5)
)

# b) pierwsze dwa wiersze
oceny[1:2, ]

# c) opis struktury
str(oceny)

# d) sr z analizy
sr_z_analizy <- mean(oceny$Analiza)

# e) nowa kolumna ze sr ocen
oceny$Srednia <- (oceny$Analiza + oceny$Algebra) / 2

# f) wyniki kobiet
kobiety <- oceny[oceny$Plec == "k", ]
sr_kobiety <- mean(kobiety$Srednia)

# g) stud, ktorzy maja co najmniej 4.5 z jednego z przedmiotów
dobre_oceny <- oceny[oceny$Analiza >= 4.5 | oceny$Algebra >= 4.5, ] # | to OR

# h) Liczba osób z analiza <= 4.5
con_4_5_z_analizy <- sum(oceny$Analiza >= 4.5)




# Zadanie 2
# a) wczytanie pliku waga1.csv
waga <- read.csv("dane/waga1.csv", sep = ";")

# b) pierwsze piec wierszy
waga[1:5, ]

# c) opis struktury
str(waga)

# d) sr wzrost i sr waga przed studiami
sredni_wzrost <- mean(waga$Wzrost)
srednia_waga <- mean(waga$Waga_przed)

# e) nowa kol z bmi
waga$W <- waga$Waga_przed / ((waga$Wzrost / 100)^2) # metry

# f) kobiety z nadwaga
kobiety_nadwaga <- waga[waga$plec == 1 & waga$W > 25, ]
#                       ^^^^             ^^^^
#                       1 to kobieta     25 to bmi

# g) mężczyźni
mezczyzni <- waga[waga$plec == 0, ] # , dla wszystkich kolumn

m_bmi <- waga[waga$plec == 0 & waga$W < 20, ]

# h) l osób wzrost < 175
sum_pow_175 <- sum(waga$Wzrost > 175)




# Zadanie 3
# a) dane z pliku mieszkania.csv
mieszkania <- read.csv("dane/mieszkania.csv", sep = ";")

# b) pierwsze szesc wierszy
mieszkania[1:6, ]

# c) opis struktury
str(mieszkania)

# d) sr metraz i sr cena
sredni_metr <- mean(mieszkania$Metraz)
srednia_cena <- mean(mieszkania$Cena)

# e) nowa kol ceny za m2
mieszkania$cena_m2 <- mieszkania$Cena / mieszkania$Metraz

# f) oferty na Psim Polu o cenie > 400 000PLN
psie_pole_tanio_ja_mhm <- mieszkania[mieszkania$Dzielnica == "Psie Pole" & mieszkania$Cena < 400000, ]

# g) ofertami na srodmiesciu o metrażu < 60m2
srodmiescie_duze <- mieszkania[mieszkania$Dzielnica == "Srodmiescie" & mieszkania$Metraz > 60, ]

# h) Liczba mieszkań o metrażu > 60m2 i cenie < 350 000PLN
m_60_cena_mala <- sum(mieszkania$Metraz > 60 & mieszkania$Cena < 350000)

#te mieszkania #nieprawda, zbyt piękne
wow <- mieszkania[mieszkania$Metraz > 60 & mieszkania$Cena < 350000, ]

s_p_w_2 <- mieszkania[mieszkania$Dzielnica == "Srodmiescie" & mieszkania$Pietro == 0 & mieszkania$Pokoje > 2, ]
