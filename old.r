
setwd("E:/Projekty/analiza-makro-konkurs")

# 1. Wczytanie danych 
dane <- read.csv2("E:/Projekty/analiza-makro-konkurs/data/data.csv", 
                  stringsAsFactors = FALSE)
# 2. Zamiana NA na 0 w kolumnach liczbowych
dane$d_uop[is.na(dane$d_uop)] <- 0
dane$d_dg[is.na(dane$d_dg)] <- 0
dane$d_fin[is.na(dane$d_fin)] <- 0
#-----------------------------------------------------------------------------------------------
library(dplyr)
# 3. Dodajemy kolumnę suma_skala
dane <- dane %>%
  mutate(
    suma_skala = if_else(forma_opodatkowania == "skala", rowSums(across(c(d_uop, d_dg))),0)
  )


# 4. dane <- dane %>%
dane <- dane %>%
  mutate(
      # Podatek UoP (tylko jeśli NIE jest "skala")
      podatek_uop = case_when(
        forma_opodatkowania != "skala" ~ {
          podstawa = pmax(d_uop - 25000, 0, na.rm = TRUE)
          pmin(podstawa, 100000, na.rm = TRUE) * 0.15 + 
            pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.40
        },
        TRUE ~ 0 # Dla "skala" podatek UoP = 0
      ),
      
      # Podatek DG
      podatek_dg = case_when(
        # Przypadek 1: Skala podatkowa (UoP + DG łącznie)
        forma_opodatkowania == "skala" ~ {
          podstawa = pmax(suma_skala - 25000, 0, na.rm = TRUE)
          pmin(podstawa, 100000, na.rm = TRUE) * 0.15 + 
            pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.40
        },
        
        # Przypadek 2: Liniówka (tylko DG)
        forma_opodatkowania == "liniowka" ~ pmax(d_dg, 0, na.rm = TRUE) * 0.20,
        
        # (domyślnie 0)
        TRUE ~ 0
      ),
      
      # Podatek FIN 
      podatek_fin = pmax(d_fin, 0, na.rm = TRUE) * 0.25
    )

# 5. Dodanie nowych kolumn
# Suma wiersza
dane$suma_wiersza <- rowSums(dane[, c("podatek_uop", "podatek_dg", "podatek_fin")], na.rm = TRUE)

# Suma kumulacyjna
dane$suma_kumulacyjna <- cumsum(ifelse(is.na(dane$suma_wiersza), 0, dane$suma_wiersza))

# Obliczenie sumy dla każdej kategorii podatku
suma_uop <- sum(dane$podatek_uop, na.rm = TRUE)
suma_dg <- sum(dane$podatek_dg, na.rm = TRUE)
suma_fin <- sum(dane$podatek_fin, na.rm = TRUE)

# Suma wszystkich kategorii
suma_wszystkich <- suma_uop + suma_dg + suma_fin

# Suma kumulacyjna z ostatniego wiersza
suma_kumulacyjna_ostatni <- tail(dane$suma_kumulacyjna, 1)

# Sprawdzenie czy suma kategorii jest równa sumie kumulacyjnej
# z tolerancją (np. 1e-4)
czy_rowne <- abs(suma_wszystkich - suma_kumulacyjna_ostatni) < 1e-4

# 5. Wyświetlenie podsumowania
cat("Suma wpływów do budżetu Fiskalii z tytułu wszystkich trzech istniejących podatków dochodowych wynosi:", 
    format(suma_kumulacyjna_ostatni, big.mark = " ", decimal.mark = ","), "zł\n")

# 6. Zapis do pliku (opcjonalnie)
write.csv2(dane, "data/r.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------------------
# ----------------------------------------------------------

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
###################

library(dplyr)
library(ggplot2)
dane_3 <- dane

# Najpierw A
dane_3 <- dane %>%
  mutate(
    podatek_uop_A = case_when(
      forma_opodatkowania != "skala" ~ {
        podstawa <- pmax(d_uop - 25000, 0, na.rm = TRUE)
        wynik   <- pmin(podstawa, 100000, na.rm = TRUE) * 0.10 +
          pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.45
        wynik
      },
      TRUE ~ 0
    ),
    podatek_dg_A = case_when(
      forma_opodatkowania == "skala" ~ {
        podstawa <- pmax(suma_skala - 25000, 0, na.rm = TRUE)
        wynik   <- pmin(podstawa, 100000, na.rm = TRUE) * 0.10 +
          pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.45
        wynik
      },
      forma_opodatkowania == "liniowka" ~ pmax(d_dg, 0, na.rm = TRUE) * 0.20,
      TRUE ~ 0
    )
  ) %>%
  # zachowujemy dane_3 z kolumnami A i dopisujemy B
  mutate(
    podatek_uop_B = case_when(
      forma_opodatkowania != "skala" ~ {
        podstawa <- pmax(d_uop - 25000, 0, na.rm = TRUE)
        podatek  <- pmin(podstawa, 100000, na.rm = TRUE) * 0.15 +
          pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.45
        pmax(podatek - 2500, 0)
      },
      TRUE ~ 0
    ),
    podatek_dg_B = case_when(
      forma_opodatkowania == "skala" ~ {
        podstawa <- pmax(suma_skala - 25000, 0, na.rm = TRUE)
        podatek  <- pmin(podstawa, 100000, na.rm = TRUE) * 0.15 +
          pmax(podstawa - 100000, 0, na.rm = TRUE) * 0.45
        pmax(podatek - 2500, 0)
      },
      forma_opodatkowania == "liniowka" ~ pmax(d_dg, 0, na.rm = TRUE) * 0.20,
      TRUE ~ 0
    )
  ) %>%
  # sumy wierszy
  mutate(
    suma_wiersza_A = rowSums(across(c(podatek_uop_A, podatek_dg_A, podatek_fin)), na.rm = TRUE),
    suma_wiersza_B = rowSums(across(c(podatek_uop_B, podatek_dg_B, podatek_fin)), na.rm = TRUE)
  )



suma_suma_wiersza   <- sum(dane_3$suma_wiersza, na.rm = TRUE)
suma_suma_wiersza_A <- sum(dane_3$suma_wiersza_A, na.rm = TRUE)
suma_suma_wiersza_B <- sum(dane_3$suma_wiersza_B, na.rm = TRUE)

# Przygotowanie danych do wyświetlenia
wyniki <- data.frame(
  Scenariusz = c("Obecny system", "Scenariusz A", "Scenariusz B"),
  Suma_podatków = c(suma_suma_wiersza, suma_suma_wiersza_A, suma_suma_wiersza_B),
  Różnica_względem_obecnego = c(0, 
                                suma_suma_wiersza_A - suma_suma_wiersza,
                                suma_suma_wiersza_B - suma_suma_wiersza)
)

# Formatowanie liczby z separatorem tysięcy
formatuj_kwoty <- function(x) {
  format(round(x), big.mark = " ", decimal.mark = ",", scientific = FALSE)
}

# Wyświetlenie wyników
cat("\nPORÓWNANIE WPŁYWÓW PODATKOWYCH:\n")
print(wyniki %>% mutate(
  Suma_podatków = formatuj_kwoty(Suma_podatków),
  Różnica_względem_obecnego = formatuj_kwoty(Różnica_względem_obecnego)
), row.names = FALSE)

# Dodatkowe statystyki
cat("\nWNIOSKI:\n")
cat("1. Scenariusz A zmienia wpływy o", 
    round((suma_suma_wiersza_A/suma_suma_wiersza - 1)*100, 1), "%\n")
cat("2. Scenariusz B zmienia wpływy o", 
    round((suma_suma_wiersza_B/suma_suma_wiersza - 1)*100, 1), "%\n")

#-----------------------------------------------------------------------------------------------
# 3.2) Proszę przedstawić na wykresie, o ile zmienią się łączne obciążenia podatkowe podatników na skali podatkowej w podziale na grupy decylowe po wprowadzeniu scenariusza A

# Wyodrębnienie osób rozliczanych skalą:
dane_3.2 <- dane_3[
  (!is.na(dane_3$d_uop) & dane_3$d_uop > 0) |
    (dane_3$forma_opodatkowania == "skala" & dane_3$d_dg > 0)
  , ]


# Obliczenie łącznych podatków przed i po scenariuszu A
dane_3.2$podatek_total_przed <- with(dane_3.2, podatek_uop + podatek_dg)
dane_3.2$podatek_total_po <- with(dane_3.2, podatek_uop_A + podatek_dg_A)
dane_3.2$roznica <- dane_3.2$podatek_total_po - dane_3.2$podatek_total_przed

# Obliczenie sumy dochodów (brutto) w celu stworzenia decyli:
dane_3.2$doch_total <- with(dane_3.2, d_uop + d_dg)
dane_3.2 <- dane_3.2[dane_3.2$doch_total > 0, ]

# Obliczenie kwantyli dla całkowitego dochodu
decyle <- quantile(dane_3.2$doch_total, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Przydzielenie grup decylowych
dane_3.2$decyl <- cut(dane_3.2$doch_total,
                      breaks = decyle,
                      include.lowest = TRUE,
                      labels = FALSE)


# Średnia zmiana podatku w każdej grupie decylowej:
#zmiana_srednia <- tapply(dane_3.2$roznica, dane_3.2$decyl, mean, na.rm = TRUE)

# przygotowanie danych:
zmiana_srednia <- dane_3.2 %>%
  group_by(decyl) %>%
  summarise(srednia_roznica = mean(roznica, na.rm = TRUE))

# rysowanie:
ggplot(zmiana_srednia, aes(x = factor(decyl), y = srednia_roznica)) +
  geom_col() +
  labs(
    x = "Grupa decylowa dochodu",
    y = "Średnia zmiana podatku (PLN)",
    title = "Zmiana obciążeń podatkowych po scenariuszu A"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------------------------
# 3.3) Proszę przedstawić na wykresie, o ile zmienią się łączne obciążenia podatkowe podatników na skali podatkowej w podziale na grupy decylowe po wprowadzeniu scenariusza B

# Wyodrębnienie osób rozliczanych skalą:
dane_3.3 <- dane_3[
  (!is.na(dane_3$d_uop) & dane_3$d_uop > 0) |
    (dane_3$forma_opodatkowania == "skala" & dane_3$d_dg > 0)
  , ]


# Obliczenie łącznych podatków przed i po scenariuszu A
dane_3.3$podatek_total_przed <- with(dane_3.3, podatek_uop + podatek_dg)
dane_3.3$podatek_total_po <- with(dane_3.3, podatek_uop_B + podatek_dg_B)
dane_3.3$roznica <- dane_3.3$podatek_total_po - dane_3.3$podatek_total_przed

# Obliczenie sumy dochodów (brutto) w celu stworzenia decyli:
dane_3.3$doch_total <- with(dane_3.3, d_uop + d_dg)
dane_3.3 <- dane_3.3[dane_3.3$doch_total > 0, ]

# Obliczenie kwantyli dla całkowitego dochodu
decyle <- quantile(dane_3.3$doch_total, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Przydzielenie grup decylowych
dane_3.3$decyl <- cut(dane_3.3$doch_total,
                      breaks = decyle,
                      include.lowest = TRUE,
                      labels = FALSE)


# Średnia zmiana podatku w każdej grupie decylowej:
#zmiana_srednia <- tapply(dane_3.3$roznica, dane_3.3$decyl, mean, na.rm = TRUE)

# przygotowanie danych:
zmiana_srednia <- dane_3.3 %>%
  group_by(decyl) %>%
  summarise(srednia_roznica = mean(roznica, na.rm = TRUE))

# rysowanie:
ggplot(zmiana_srednia, aes(x = factor(decyl), y = srednia_roznica)) +
  geom_col() +
  labs(
    x = "Grupa decylowa dochodu",
    y = "Średnia zmiana podatku (PLN)",
    title = "Zmiana obciążeń podatkowych po scenariuszu A"
  ) +
  theme_minimal()
