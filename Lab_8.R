#lista8 na 6 maja

#(płeć: 0-mężczyzna, 1-kobieta)

#zadanie1
#a)
n <- 100 #wielkosc proby
successes <- 30
p_hat <- successes / n #proporcja w pprobie
#wzór wada (chyba sie tak nazywa)
#CI = p^ -+ z( alfa/2 ) *  √ ( p^ (1-p^ ) / n )

#i) 95%
z_95 <- qnorm(0.975)
ci_a_i <- p_hat + c(-1, 1) * z_95 * sqrt( p_hat * (1-p_hat) /n )
cat("Zadanie1_ai:","\n")
cat(ci_a_i, "\n\n")

#ii) 99%
z_99 <- qnorm(0.995)
ci_a_ii <- p_hat + c(-1, 1) * z_99 * sqrt( p_hat*( 1-p_hat) /n )
cat("Zadanie1_aii:","\n")
cat(ci_a_ii, "\n\n")

#b)
mean_iq <- 109
sd_iq <- 15 #sqrt(225)
#CI = x_ -+ z( alfa/2 ) *  sd / √ n )

#i) 95%
ci_b_i <- mean_iq + c(-1, 1) * z_95 * sd_iq / sqrt(n)
cat("Zadanie1_bi:","\n")
cat(ci_b_i, "\n\n")

#ii) 99%
ci_b_ii <- mean_iq + c(-1, 1) * z_99 * sd_iq / sqrt(n)
cat("Zadanie1_bii:","\n")
cat(ci_b_ii, "\n\n")

#c)
t_95 <- qt(0.975, df = n-1)
t_99 <- qt(0.995, df = n-1)
#CI = x_ -+ t( alfa/2,df ) *  sd / √ n )

#i) 95%
ci_c_i <- mean_iq + c(-1, 1) * t_95 * sd_iq / sqrt(n)
cat("Zadanie1_ci:","\n")
cat(ci_c_i, "\n\n")

#ii) 99%
ci_c_ii <- mean_iq + c(-1, 1) * t_99 * sd_iq / sqrt(n)
cat("Zadanie1_cii:","\n")
cat(ci_c_ii, "\n\n")

#zadanie2 90%
#to samo

waga <- read.csv2("waga1.csv", sep=";")
wzrost_all <- waga$Wzrost
n_all <- length(wzrost_all)
mean_all <- mean(wzrost_all)
sd_all <- sd(wzrost_all)

#a) wartość krytyczną dla rozkładu normalnego
z_90 <- qnorm(0.95)
ci_2a <- mean_all + c(-1,1) * z_90 * sd_all / sqrt(n_all)
cat("Zadanie2_a:","\n")
cat(ci_2a, "\n\n")

#b) wartość krytyczną dla rozkładu Studenta.
t_90 <- qt(0.95, df = n_all-1)
ci_2b <- mean_all + c(-1,1) * t_90 * sd_all / sqrt(n_all)
cat("Zadanie2_b:","\n")
cat(ci_2b, "\n\n")

#c) metodę „bootstrap”
library(boot)
stat_fun <- function(data, indices) mean(data[indices])
set.seed(123)
boot_results <- boot(wzrost_all, stat_fun, R=1000)
ci_2c <- boot.ci(boot_results, type="perc", conf=0.90)$percent[4:5]
cat("Zadanie2_c:","\n")
cat(ci_2c, "\n\n")

#zadanie3 przedział ufności dla średniego wzrostu STUDENTEK na poziomie ufności 98%
studentki <- subset(waga, plec == 1)   #1 = kobiety
wzrost_studentki <- studentki$Wzrost
n_studentki <- length(wzrost_studentki)
mean_studentki <- mean(wzrost_studentki)
sd_studentki <- sd(wzrost_studentki)

#a) wartość krytyczną dla rozkładu normalnego
z_98 <- qnorm(0.99)
ci_3a <- mean_studentki + c(-1,1) * z_98 * sd_studentki / sqrt(n_studentki)
cat("Zadanie3_a:","\n")
cat(ci_3a, "\n\n")

#b) wartość krytyczną dla rozkładu Studenta
t_98 <- qt(0.99, df = n_studentki-1)
ci_3b <- mean_studentki + c(-1,1) * t_98 * sd_studentki / sqrt(n_studentki)
cat("Zadanie3_b:","\n")
cat(ci_3b, "\n\n")

#c) metodę „bootstrap”
boot_results_studentki <- boot(wzrost_studentki, stat_fun, R=1000)
ci_3c <- boot.ci(boot_results_studentki, type="perc", conf=0.98)$percent[4:5]
cat("Zadanie3_c:","\n")
cat(ci_3c, "\n\n")

#zadanie4 ufnośc 94% ,(obie płcie), X>168cm
proporcja_4 <- sum(wzrost_all > 168) / n_all

#a) wartość krytyczną dla rozkładu normalnego
z_94 <- qnorm(0.97)
ci_4a <- proporcja_4 + c(-1,1) * z_94 * sqrt(proporcja_4*(1-proporcja_4)/n_all)
cat("Zadanie4_a:","\n")
cat(ci_4a, "\n\n")

#b)  metodę „bootstrap”
stat_prop <- function(data, indices) mean(data[indices] > 168)
boot_prop4 <- boot(wzrost_all, stat_prop, R=1000)
ci_4b <- boot.ci(boot_prop4, type="perc", conf=0.94)$percent[4:5]
cat("Zadanie4_b:","\n")
cat(ci_4b, "\n\n")

#zadanie5  96%, STUDENTKI> 168cm
proporcja_5 <- sum(wzrost_studentki > 168) / n_studentki

#a) wartość krytyczną dla rozkładu normalnego
z_96 <- qnorm(0.98)
ci_5a <- proporcja_5 + c(-1,1) * z_96 * sqrt(proporcja_5*(1-proporcja_5)/n_studentki)
cat("Zadanie5_a:","\n")
cat(ci_5a, "\n\n")

#b) metodę „bootstrap”
boot_prop5 <- boot(wzrost_studentki, stat_prop, R=1000)
ci_5b <- boot.ci(boot_prop5, type="perc", conf=0.96)$percent[4:5]
cat("Zadanie5_b:","\n")
cat(ci_5b, "\n\n")