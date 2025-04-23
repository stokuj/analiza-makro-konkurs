
setwd("E:/Projekty/analiza-makro-konkurs")

library(dplyr)
library(ggplot2)
# 1. Wczytanie danych 
dane <- read.csv2("E:/Projekty/analiza-makro-konkurs/data/data.csv", 
                  stringsAsFactors = FALSE)
# 2. Zamiana NA na 0 w kolumnach liczbowych
dane$d_uop[is.na(dane$d_uop)] <- 0
dane$d_dg[is.na(dane$d_dg)] <- 0
dane$d_fin[is.na(dane$d_fin)] <- 0
#-----------------------------------------------------------------------------------------------

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
#-----------------------------------------------------------------------------------------------
# ----------------------------------------------------------

