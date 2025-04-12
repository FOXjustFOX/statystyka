# Zadanie 1
cat("\n===== Zadanie 1 =====\n")

# Definicja rozkładu łącznego
prob_matrix1 <- matrix(c(1 / 8, 1 / 4, 1 / 8, 1 / 6, 1 / 6, 1 / 6), nrow = 2, byrow = TRUE)
rownames(prob_matrix1) <- c("X=0", "X=1")
colnames(prob_matrix1) <- c("Y=0", "Y=1", "Y=2")

# i) Rozkłady brzegowe
marginal_x1 <- rowSums(prob_matrix1)
marginal_y1 <- colSums(prob_matrix1)
cat("\ni) Rozkłady brzegowe:\nX:", brzegowe_X1, "\nY:", brzegowe_Y1, "\n")

# ii) Współczynnik korelacji
x_vals1 <- c(0, 1)
y_vals1 <- c(0, 1, 2)
e_x1 <- sum(x_vals1 * brzegowe_X1)
e_y1 <- sum(y_vals1 * brzegowe_Y1)
e_xy1 <- sum(outer(x_vals1, y_vals1) * prob_matrix1)
cov_xy1 <- e_xy1 - e_x1 * e_y1
var_x1 <- sum((x_vals1 - e_x1)^2 * brzegowe_X1)
var_y1 <- sum((y_vals1 - e_y1)^2 * brzegowe_Y1)
corr1 <- cov_xy1 / (sqrt(var_x1) * sqrt(var_y1))
cat("\nii) Współczynnik korelacji:", corr1, "\n")

# iii) Rozkłady warunkowe Y|X
conditional_y1 <- list(
  "X=0" = prob_matrix1[1, ] / brzegowe_X1[1],
  "X=1" = prob_matrix1[2, ] / brzegowe_X1[2]
)
cat("\niii) Rozkłady warunkowe Y|X:\n")
print(warunkowe_Y1)

# iv) Niezależność
niezalezne1 <- all(prob_matrix1 == outer(brzegowe_X1, brzegowe_Y1))
cat("\niv) Zmienne są niezależne?:", niezalezne1, "\n")

# Generowanie danych (część 2)
set.seed(123)
n1 <- 1000

# Metoda skumulowana
wszystkie_pary1 <- expand.grid(X = x_vals1, Y = y_vals1)
dane_skum1 <- wszystkie_pary1[
  sample(1:6, n1, replace = TRUE, prob = as.vector(t(prob_matrix1))), 
]

# Estymacja korelacji
cat("\nMetoda skumulowana - współczynniki korelacji:\n")
cat("Pearson:", cor(dane_skum1$X, dane_skum1$Y), "\n")
cat("Spearman:", cor(dane_skum1$X, dane_skum1$Y, method = "spearman"), "\n")
cat("Kendall:", cor(dane_skum1$X, dane_skum1$Y, method = "kendall"), "\n")
dane_war1 <- data.frame(
  X = sample(x_vals1, n1, prob = marginal_x1, replace = TRUE),
  Y = sapply(
    sample(x_vals1, n1, prob = marginal_x1, replace = TRUE),
    function(x) sample(y_vals1, 1, prob = ifelse(x == 0, conditional_y1[["X=0"]], conditional_y1[["X=1"]]))
  )
)
# Metoda warunkowa
dane_war1 <- data.frame(X = sample(x_vals1, n1, prob = brzegowe_X1, replace = TRUE))
dane_war1$Y <- sapply(dane_war1$X, function(x) {
  sample(y_vals1, 1, prob = ifelse(x == 0, warunkowe_Y1[[1]], warunkowe_Y1[[2]]))
})

# Tablica rozdzielcza
cat("\nTabela rozdzielcza (metoda warunkowa):\n")
grades_x <- c(2, 3, 3.5, 4, 4.5, 5)
grades_y <- c(2, 3, 3.5, 4, 4.5, 5)
# Zadanie 3
cat("\n\n===== Zadanie 3 =====")

# Definicja rozkładu łącznego
oceny_X <- c(2, 3, 3.5, 4, 4.5, 5)
oceny_Y <- c(2, 3, 3.5, 4, 4.5, 5)
prob_matrix3 <- matrix(c(
  0.05, 0.03, 0.02, 0, 0, 0,
  0.05, 0.07, 0.05, 0.03, 0, 0,
  0.03, 0.05, 0.06, 0.04, 0.02, 0,
  0, 0.01, 0.01, 0.02, 0.03, 0.03
), nrow = 6, byrow = TRUE)

marginal_x3 <- rowSums(prob_matrix3)
marginal_y3 <- colSums(prob_matrix3)
# i) Rozkłady brzegowe
brzegowe_X3 <- rowSums(prob_matrix3)
brzegowe_Y3 <- colSums(prob_matrix3)
cat("\n\ni) Rozkłady brzegowe:\nX:", brzegowe_X3, "\nY:", brzegowe_Y3, "\n")

# ii) Współczynnik korelacji
e_x3 <- sum(oceny_X * brzegowe_X3)
e_y3 <- sum(oceny_Y * brzegowe_Y3)
e_xy3 <- sum(outer(oceny_X, oceny_Y) * prob_matrix3)
cov_xy3 <- e_xy3 - e_x3 * e_y3
conditional_y3 <- lapply(1:6, function(i) prob_matrix3[i, ] / marginal_x3[i])
names(conditional_y3) <- paste0("X=", grades_x)
corr3 <- cov_xy3 / (sqrt(var_x3) * sqrt(var_y3))
cat("\nii) Współczynnik korelacji:", corr3, "\n")

# iii) Rozkłady warunkowe
warunkowe_Y3 <- lapply(1:6, function(i) prob_matrix3[i, ] / brzegowe_X3[i])
names(warunkowe_Y3) <- paste0("X=", oceny_X)
cat("\niii) Rozkłady warunkowe Y|X:\n")
print(warunkowe_Y3)
indices_x <- sample(1:6, n3, prob = marginal_x3, replace = TRUE)
# Generowanie danych (część 2)
n3 <- 1000

# Metoda warunkowa - POPRAWIONE: generowanie indeksów zamiast wartości X
set.seed(123)
indices_X <- sample(1:6, n3, prob = brzegowe_X3, replace = TRUE)
dane_war3 <- data.frame(X = oceny_X[indices_X])
dane_war3$Y <- sapply(indices_X, function(idx) {
  sample(oceny_Y, 1, prob = warunkowe_Y3[[idx]])
})

# v) Tablica rozdzielcza
cat("\nv) Tabela rozdzielcza:\n")
print(round(prop.table(table(dane_war3)), 3))

# vi) Estymacja korelacji
cat("\nvi) Współczynniki korelacji:\n")
cat("Pearson:", cor(dane_war3$X, dane_war3$Y), "\n")
cat("Spearman:", cor(dane_war3$X, dane_war3$Y, method = "spearman"), "\n")