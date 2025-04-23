
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
