#lista6

#zadanie 1

# Definicja rozkładu łącznego
# Definicja macierzy prawdopodobieństw dla rozkładu łącznego:
# Wiersze reprezentują wartości X (X=0, X=1), a kolumny wartości Y (Y=0, Y=1, Y=2)
prob_matrix1 <- matrix(
  c(1 / 8, 1 / 4, 1 / 8,
    1 / 6, 1 / 6, 1 / 6),
  nrow = 2,
  byrow = TRUE
)

rownames(prob_matrix1) <- c("X=0", "X=1")
colnames(prob_matrix1) <- c("Y=0", "Y=1", "Y=2")

#i) rozkłady brzegowe
brzegowe_X1 <- rowSums(prob_matrix1)
brzegowe_Y1 <- colSums(prob_matrix1)
cat("Zadanie1_i: ","\n")
cat("Rozkłady brzegowe:\nX:", brzegowe_X1, "\nY:", brzegowe_Y1, "\n")

#ii) współczynnik korelacji  ρ(X,Y)
x_vals1 <- c(0, 1)
y_vals1 <- c(0, 1, 2)
e_x1 <- sum(x_vals1 * brzegowe_X1)
e_y1 <- sum(y_vals1 * brzegowe_Y1)
e_xy1 <- sum(outer(x_vals1, y_vals1) * prob_matrix1)
cov_xy1 <- e_xy1 - e_x1 * e_y1
var_x1 <- sum((x_vals1 - e_x1)^2 * brzegowe_X1)
var_y1 <- sum((y_vals1 - e_y1)^2 * brzegowe_Y1)
corr1 <- cov_xy1 / (sqrt(var_x1) * sqrt(var_y1))
cat("Zadanie1_ii:")
cat("Współczynnik korelacji:", corr1, "\n")


#iii) Rozkłady warunkowe Y|X      P(Y=y∣X=x), x∈[0,1]
warunkowe_Y1 <- list(
  "X=0" = prob_matrix1[1, ] / brzegowe_X1[1],
  "X=1" = prob_matrix1[2, ] / brzegowe_X1[2] )
cat("Zadanie1_iii:", "\n")
cat("Rozkłady warunkowe Y|X:\n")
print(warunkowe_Y1)


#iv) Niezależność
niezalezne1 <- all(prob_matrix1 == outer(brzegowe_X1, brzegowe_Y1))
cat("Zadanie1_iv:", "\n")
cat("Zmienne są niezależne?:", niezalezne1, "\n")


#zadanie 2
#a) i
set.seed(123)
n1 <- 1000

# Metoda skumulowana
wszystkie_pary1 <- expand.grid(X = x_vals1, Y = y_vals1)
dane_skum1 <- wszystkie_pary1[
  sample(1:6, n1, replace = TRUE, prob = as.vector(t(prob_matrix1))), ]

# Estymacja korelacji
cat("Zadanie2_ai:","\n")
cat("\nMetoda skumulowana - współczynniki korelacji:\n")
cat("Pearson:", cor(dane_skum1$X, dane_skum1$Y), "\n")
cat("Spearman:", cor(dane_skum1$X, dane_skum1$Y, method = "spearman"), "\n")
cat("Kendall:", cor(dane_skum1$X, dane_skum1$Y, method = "kendall"), "\n")

# Metoda warunkowa
marginal_x1 <- rowSums(prob_matrix1) # Rozkład brzegowy X
conditional_y1 <- prop.table(prob_matrix1, margin = 1) # Rozkłady warunkowe Y|X

dane_war1 <- data.frame(X = sample(x_vals1, n1, prob = marginal_x1, replace = TRUE))
dane_war1$Y <- sapply(dane_war1$X, function(x) {
  sample(y_vals1, 1, prob = conditional_y1[which(x_vals1 == x), ]) })



#v Tablica rozdzielcza
cat("\nTabela rozdzielcza (metoda warunkowa):\n")
grades_x <- c(2, 3, 3.5, 4, 4.5, 5)
grades_y <- c(2, 3, 3.5, 4, 4.5, 5)

# --- Zadanie 2b ---
n2b <- 1000

# Rozkład brzegowy X
marginal_x2b <- rowSums(prob_matrix1)

# Rozkłady warunkowe Y|X
conditional_y2b <- prop.table(prob_matrix1, margin = 1)

# Losowanie X z rozkładu brzegowego
X2b <- sample(x_vals1, n2b, replace = TRUE, prob = marginal_x2b)

# Losowanie Y z rozkładu warunkowego Y|X
Y2b <- sapply(X2b, function(x) {
  idx <- which(x_vals1 == x)
  sample(y_vals1, 1, prob = conditional_y2b[idx, ])
})

# Zapisz wyniki do ramki danych
dane2b <- data.frame(X = X2b, Y = Y2b)

# Estymacja współczynników korelacji
cat("\nZadanie 2b: Współczynniki korelacji:\n")
cat("Pearson:", cor(dane2b$X, dane2b$Y), "\n")
cat("Spearman:", cor(dane2b$X, dane2b$Y, method = "spearman"), "\n")
cat("Kendall:", cor(dane2b$X, dane2b$Y, method = "kendall"), "\n")

# Tablica rozdzielcza (relatywne częstości)
cat("\nZadanie 2b: Tablica rozdzielcza (relatywne częstości):\n")
print(round(prop.table(table(dane2b)), 3))

#zadanie 3

# Definicja rozkładu łącznego
oceny_X <- c(2, 3, 3.5, 4, 4.5, 5)
oceny_Y <- c(2, 3, 3.5, 4, 4.5, 5)
prob_matrix3 <- matrix(c(
  0.05, 0.03, 0.02, 0.00, 0.00, 0.00,
  0.05, 0.07, 0.05, 0.03, 0.00, 0.00,
  0.03, 0.05, 0.06, 0.04, 0.02, 0.00,
  0.01, 0.04, 0.06, 0.06, 0.02, 0.01,
  0.00, 0.02, 0.05, 0.08, 0.04, 0.01,
  0.00, 0.01, 0.01, 0.02, 0.03, 0.03),
nrow = 6, byrow = TRUE)


#i) rozkłady brzegowe
brzegowe_X3 <- rowSums(prob_matrix3)
brzegowe_Y3 <- colSums(prob_matrix3)
cat("\nZad 3 \nRozkłady brzegowe:\nX:", brzegowe_X3, "\nY:", brzegowe_Y3, "\n")


#ii) współczynnik korelacji
e_x3 <- sum(oceny_X * brzegowe_X3)
e_x32 <- sum(oceny_X^2 * brzegowe_X3)

e_y3 <- sum(oceny_Y * brzegowe_Y3)
e_y32 <- sum(oceny_Y^2 * brzegowe_Y3)

e_xy3 <- sum(outer(oceny_X, oceny_Y) * prob_matrix3)

cov_xy3 <- e_xy3 - e_x3 * e_y3

# var_x3 <- sum((oceny_X - e_x3)^2 * brzegowe_X3)

var_x32 <- (e_x32 - (e_x3)^2)
var_y32 <- (e_y32 - (e_y3)^2)

corr3 <- cov_xy3 / (sqrt(var_x32) * sqrt(var_y32))

cat("Współczynnik korelacji:", corr3, "\n")


#iii) Rozkłady warunkowe
warunkowe_Y3 <- lapply(1:6, function(i) prob_matrix3[i, ] / brzegowe_X3[i])
names(warunkowe_Y3) <- paste0("X=", oceny_X)
cat("Rozkłady warunkowe Y|X:\n")
print(warunkowe_Y3)

# iv)
n3 <- 1000
set.seed(123)
indices_X <- sample(1:6, n3, prob = brzegowe_X3, replace = TRUE)
dane_war3 <- data.frame(X = oceny_X[indices_X])
dane_war3$Y <- sapply(indices_X, function(idx) {
  sample(oceny_Y, 1, prob = warunkowe_Y3[[idx]]) })

#v) Tablica rozdzielcza
cat("Tabela rozdzielcza:\n")
print(round(prop.table(table(dane_war3)), 3))

# Histogramy
par(mfrow=c(2,3))
for(i in 1:6) {
  dane_x_i <- dane_war3$Y[dane_war3$X == oceny_X[i]]
  if(length(dane_x_i) > 0) {
    hist(dane_x_i, 
         main=paste("Y dla X=", oceny_X[i], sep=""),
         breaks=6,
         col="#ff0000")
  } else {
    plot.new(); text(0.5, 0.5, "Brak danych")
  }
}
par(mfrow=c(1,1))

#vi) Estymacja korelacji
cat("Współczynniki korelacji:\n")
cat("Pearson:", cor(dane_war3$X, dane_war3$Y), "\n")
cat("Spearman:", cor(dane_war3$X, dane_war3$Y, method = "spearman"), "\n")