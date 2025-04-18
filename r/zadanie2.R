# ----------------------------------------------------------
# WIZUALIZACJA ROZKŁADU PODATKÓW (Zadanie 2 - Analiza mikrodanych)

# 1. Utwórz dane_2.1 tylko z poprawnymi wartościami d_uop
dane_2.1 <- dane[
  !is.na(dane$d_uop) & dane$d_uop > 0 &
    dane$d_dg == 0 &
    dane$d_fin == 0, 
]

# 1) Obliczenie klina podatkowego w dane_2.1
dane_2.1$klin_podatkowy <- dane_2.1$podatek_uop / dane_2.1$d_uop

# 2) Usunięcie wierszy z NA lub dochodem ≤ 0
dane_2.1 <- dane_2.1[!is.na(dane_2.1$d_uop) & dane_2.1$d_uop > 0, ]

# 3) Posortowanie po rosnącym dochodzie
dane_2.1_sorted <- dane_2.1[order(dane_2.1$d_uop), ]



# Zakresy osi z buforem 20%
x_min <- min(dane_2.1_sorted$d_uop)
x_max <- max(dane_2.1_sorted$d_uop)
x_range <- x_max - x_min
xlim <- c(x_min - 0.1 * x_range, x_max + 0.1 * x_range)

y_min <- min(dane_2.1_sorted$klin_podatkowy * 100)
y_max <- max(dane_2.1_sorted$klin_podatkowy * 100)
y_range <- y_max - y_min
ylim <- c(y_min - 0.1 * y_range, y_max + 0.1 * y_range)



# 4) Wykres linii klina podatkowego
# Rysowanie wykresu
plot(dane_2.1_sorted$d_uop,
     dane_2.1_sorted$klin_podatkowy * 100,  # procenty
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Dochód brutto (PLN)",
     ylab = "Klin podatkowy (% dochodu)",
     main = "(2.1) Klin podatkowy dla pracy najemnej (teoria)")

grid()

#legend("topleft", legend = "Teoretyczny klin podatkowy", col = "blue", lwd = 2)
#-----------------------------------------------------------------------------------------------
# 2.2) Empiryczna przeciętna stopa opodatkowania wg grup decylowych dochodu

# Przygotowanie danych: decyle na podstawie pozytywnych dochodów z UoP
deciles <- quantile(dane_2.1$d_uop, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Przypisanie decyla (1-10)
dane_2.1$decyl <- cut(dane_2.1$d_uop,
                      breaks = deciles,
                      include.lowest = TRUE,
                      labels = FALSE)

# Obliczenie empirycznej średniej stopy opodatkowania (w %)
emp_rate <- tapply(dane_2.1$podatek_uop / dane_2.1$d_uop,
                   dane_2.1$decyl,
                   mean, na.rm = TRUE) * 100

# Rysowanie wykresu słupkowego empirycznej stopy opodatkowania
decyls <- 1:10
barplot(emp_rate,
        names.arg = decyls,
        xlab = "Grupa decylowa dochodu",
        ylab = "Empiryczna średnia stopa opodatkowania (%)",
        main = "Empiryczna średnia stopa opodatkowania wg decyli dochodu",
        ylim = c(0, max(emp_rate) * 1.1))

grid()

# ----------------------------------------------------------
# 2.4. Teoretyczny wykres klina podatkowego dla działalności gospodarczej
#    (Zadanie 2.4 – optymalna forma opodatkowania)

# Przygotowanie danych: tylko dodatnie d_dg i brak innych dochodów
dane_2.4 <- dane[
  !is.na(dane$d_dg) & dane$d_dg > 0 &
    dane$d_uop == 0 &
    dane$d_fin == 0, 
]
#######




# <-- Obliczenie optymalnego podatku dla każdego podatnika -->
# Wyliczenie podatku wg skali podatkowej (15% od nadwyżki do 100k, 40% powyżej)
suma_skala <- dane_2.4$d_dg
podstawa <- pmax(suma_skala - 25000, 0, na.rm = TRUE)
tax_skala <- pmin(podstawa, 100000, na.rm = TRUE) * 0.15 +
  pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.40
# Wyliczenie podatku wg liniowej stawki 20%
tax_liniowa <- pmax(dane_2.4$d_dg, 0, na.rm = TRUE) * 0.20
# Wybór mniejszej wartości jako optymalnej

dane_2.4$podatek_dg_opt <- pmin(tax_skala, tax_liniowa)








#######
# 1) Obliczenie klina podatkowego w dane_2.4
dane_2.4$klin_podatkowy <- dane_2.4$podatek_dg_opt / dane_2.4$d_dg

# 2) Usunięcie wierszy z NA lub dochodem ≤ 0
dane_2.4 <- dane_2.4[!is.na(dane_2.4$d_dg) & dane_2.4$d_dg > 0, ]

# 3) Posortowanie po rosnącym dochodzie
dane_2.4_sorted <- dane_2.4[order(dane_2.4$d_dg), ]

# 4) Wykres linii klina podatkowego
# Rysowanie wykresu
plot(dane_2.4_sorted$d_dg,
     dane_2.4_sorted$klin_podatkowy * 100,  # procenty
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Dochód brutto (PLN)",
     ylab = "Klin podatkowy (% dochodu)",
     main = "(2.4) Klin podatkowy dla działalności gospodarczej")

grid()
# ----------------------------------------------------------
# 2.5) Empiryczna przeciętna stopa opodatkowania wg grup decylowych dochodu

# Przygotowanie danych: decyle na podstawie pozytywnych dochodów z DG
deciles <- quantile(dane_2.4$d_dg, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Przypisanie decyla (1-10)
dane_2.4$decyl <- cut(dane_2.4$d_dg,
                      breaks = deciles,
                      include.lowest = TRUE,
                      labels = FALSE)

# Obliczenie empirycznej średniej stopy opodatkowania (w %)
emp_rate <- tapply(dane_2.4$podatek_dg / dane_2.4$d_dg,
                   dane_2.4$decyl,
                   mean, na.rm = TRUE) * 100

# Rysowanie wykresu słupkowego empirycznej stopy opodatkowania
decyls <- 1:10
barplot(emp_rate,
        names.arg = decyls,
        xlab = "Grupa decylowa dochodu",
        ylab = "Empiryczna średnia stopa opodatkowania (%)",
        main = "Empiryczna średnia stopa opodatkowania wg decyli dochodu",
        ylim = c(0, max(emp_rate) * 1.1))

grid()