# Zadanie 1

a <- c(1, 4, 6, 13, -10, 8)
b <- seq(1, 101, by=2)
c <- rep(c(4, 7, 9), each=3)
d <- c("czy", "to", "jest", "wektor", NA)
e <- c("czy", "to", "jest", "wektor", "NA")
f <- rep(c(4, 7, 9), times=6)

# ii)
dlugosc_a <- length(a)
dlugosc_b <- length(b)
dlugosc_c <- length(c)
dlugosc_d <- length(d)
dlugosc_e <- length(e)
dlugosc_f <- length(f)

typ_a <- typeof(a)
typ_b <- typeof(b)
typ_c <- typeof(c)
typ_d <- typeof(d)
typ_e <- typeof(e)
typ_f <- typeof(f)

min_a <- min(a)
min_b <- min(b)
min_c <- min(c)
min_d <- min(d, na.rm=TRUE) #sortuje alf i bierze pierwszy
min_e <- min(e)
min_f <- min(f)

max_a <- max(a)
max_b <- max(b)
max_c <- max(c)
max_f <- max(f)

suma_a <- sum(a)
suma_b <- sum(b)
suma_c <- sum(c)
#suma nie może być ze stringiem d e 
suma_f <- sum(f)

# iii)
a_sorted <- sort(a)
b_sorted <- sort(b)
c_sorted <- sort(c)
d_sorted <- sort(d, na.last = TRUE)
e_sorted <- sort(e)
f_sorted <- sort(f)

# iv)
a_plus_f <- a + f
a_times_f <- a * f
a_plus_c <- a + c
a_plus_10 <- a + 10
fifteen_a <- 15 * a
element_26_b <- b[26]
elementy_f_6_10 <- f[6:10]

# v)
b_wieksze_niz_50 <- b[b > 50]
ilosc_b_wiekszych_niz_50 <- length(b_wieksze_niz_50)

# Łączenie wektorów
g <- c(a, b)

#zad 2:
# i)
A <- matrix(c(3, 1, 2, 4, 5, 3), nrow=2, byrow=TRUE)  # byrow=TRUE oznacza wypełnianie macierzy wierszami
B <- matrix(c(-1, 2, 3, -4, -5, 6), nrow=3, byrow=TRUE) # bo auto to kolumny
C <- matrix(c(7, 3, 2, 1), nrow=2, byrow=TRUE)
D <- matrix(c(1, 2, 4, 3, 5, 7, 5, 7, 11), nrow=3, byrow=TRUE)

# ii) Operacje na macierzach
# A_plus_B <- A + B
A_plus_B_T <- A + t(B)   #TO nie działa BŁĄD
A_times_B <- A %*% B    #TO nie działa BŁĄD
A_mn <- A * A
D_inv <- solve(D)
D_D_inv <- D %*% D_inv

# iii) Rozwiązywanie równań
CX=A <- solve(C) %*% A
XD=A <- A %*% solve(D)  


