library(dplyr)
library(ggplot2)

# Wczytanie i przygotowanie danych
dane <- read.csv2("E:/Projekty/analiza-makro-konkurs/data/data.csv") %>%
  mutate(across(c(d_uop, d_dg, d_fin), ~ ifelse(is.na(.), 0, .)))

# Funkcje do obliczania podatków z zabezpieczeniem przed ujemnymi wartościami
calculate_tax <- function(income, tax_type) {
  case_when(
    tax_type == "uop" ~ {
      base <- pmax(income - 25000, 0)
      tax <- pmin(base, 100000) * 0.15 + pmax(base - 100000, 0) * 0.40
      pmax(tax, 0)  # Zabezpieczenie przed ujemnymi wartościami
    },
    tax_type == "dg_skala" ~ {
      base <- pmax(income - 25000, 0)
      tax <- pmin(base, 100000) * 0.15 + pmax(base - 100000, 0) * 0.40
      pmax(tax, 0)
    },
    tax_type == "dg_liniowa" ~ pmax(income * 0.20, 0),
    tax_type == "fin" ~ pmax(income * 0.25, 0),
    TRUE ~ 0
  )
}

# Obliczenie podatków
dane <- dane %>%
  mutate(
    suma_skala = ifelse(forma_opodatkowania == "skala", d_uop + d_dg, 0),
    
    p_uop = ifelse(forma_opodatkowania != "skala", 
                   calculate_tax(d_uop, "uop"), 0),
    
    p_dg = case_when(
      forma_opodatkowania == "skala" ~ calculate_tax(suma_skala, "dg_skala"),
      forma_opodatkowania == "liniowka" ~ calculate_tax(d_dg, "dg_liniowa"),
      TRUE ~ 0
    ),
    
    p_fin = calculate_tax(d_fin, "fin"),
    
    p_sum = p_uop + p_dg + p_fin
  )

# Podsumowanie z formatowaniem
total_tax <- sum(dane$p_sum, na.rm = TRUE)
cat("Suma wpływów do budżetu Fiskalii:", 
    format(round(total_tax), big.mark = " ", decimal.mark = ","), 
    "zł\n")

##########################################################################################################################

# Funkcja pomocnicza do tworzenia wykresów
plot_tax_wedge <- function(data, income_col, tax_col, title) {
  data %>%
    filter({{income_col}} > 0) %>%
    mutate(klin_podatkowy = {{tax_col}} / {{income_col}} * 100) %>%
    arrange({{income_col}}) %>%
    ggplot(aes({{income_col}}, klin_podatkowy)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(x = "Dochód brutto (PLN)", 
         y = "Klin podatkowy (% dochodu)",
         title = title) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

# Funkcja do wykresów decylowych
plot_decile_tax <- function(data, income_col, tax_col, title) {
  data %>%
    filter({{income_col}} > 0) %>%
    mutate(decyl = ntile({{income_col}}, 10),
           stopa_podatkowa = {{tax_col}} / {{income_col}} * 100) %>%
    group_by(decyl) %>%
    summarise(srednia_stopa = mean(stopa_podatkowa, na.rm = TRUE)) %>%
    ggplot(aes(factor(decyl), srednia_stopa)) +
    geom_col(fill = "steelblue") +
    labs(x = "Grupa decylowa dochodu", 
         y = "Średnia stopa opodatkowania (%)",
         title = title) +
    theme_minimal()
}

# 2.1 & # 2.2 
dane_uop <- dane %>% 
  filter(d_uop > 0, d_dg == 0, d_fin == 0, !is.na(d_uop))

# 2.1 
plot_tax_wedge(dane_uop, d_uop, p_uop, 
               "(2.1) Klin podatkowy dla pracy najemnej")
# 2.2 
plot_decile_tax(dane_uop, d_uop, p_uop,
                "Empiryczna stopa opodatkowania wg decyli (UoP)")
# 2.4 & # 2.5 
dane_dg <- dane %>% 
  filter(d_dg > 0, d_uop == 0, d_fin == 0, !is.na(d_dg)) %>%
  mutate(
    tax_skala = pmax(d_dg - 25000, 0) %>% 
      {pmin(., 100000) * 0.15 + pmax(. - 100000, 0) * 0.40},
    tax_liniowa = d_dg * 0.20,
    podatek_dg_opt = pmin(tax_skala, tax_liniowa)
  )
# 2.4 
plot_tax_wedge(dane_dg, d_dg, podatek_dg_opt, 
               "(2.4) Klin podatkowy dla działalności gospodarczej")
# 2.5 
plot_decile_tax(dane_dg, d_dg, p_dg,
                "Empiryczna stopa opodatkowania wg decyli (DG)")
##########################################################################################################################
# Funkcje do obliczania podatków z zabezpieczeniem przed ujemnymi wartościami
calculate_tax_A <- function(income) {
  base <- pmax(income - 25000, 0)
  tax <- pmin(base, 100000) * 0.10 + pmax(base - 100000, 0) * 0.45
  pmax(tax, 0)  # Gwarancja nieujemności
}

calculate_tax_B <- function(income) {
  base <- pmax(income - 25000, 0)
  tax <- pmin(base, 100000) * 0.15 + pmax(base - 100000, 0) * 0.45 - 2500
  pmax(tax, 0)  # Gwarancja nieujemności
}

# Obliczenia podatków dla scenariuszy A i B z zabezpieczeniami
dane_3 <- dane %>%
  mutate(
    # Scenariusz A
    p_uop_A = ifelse(forma_opodatkowania != "skala", 
                     pmax(calculate_tax_A(d_uop), 0), 0),  # Podwójne zabezpieczenie
    p_dg_A = case_when(
      forma_opodatkowania == "skala" ~ pmax(calculate_tax_A(d_uop + d_dg), 0),
      forma_opodatkowania == "liniowka" ~ pmax(d_dg * 0.20, 0),  # Zabezpieczenie
      TRUE ~ 0
    ),
    
    # Scenariusz B
    p_uop_B = ifelse(forma_opodatkowania != "skala", 
                     pmax(calculate_tax_B(d_uop), 0), 0),  # Podwójne zabezpieczenie
    p_dg_B = case_when(
      forma_opodatkowania == "skala" ~ pmax(calculate_tax_B(d_uop + d_dg), 0),
      forma_opodatkowania == "liniowka" ~ pmax(d_dg * 0.20, 0),  # Zabezpieczenie
      TRUE ~ 0
    ),
    
    # Sumy podatków z dodatkowym zabezpieczeniem
    p_sum_A = pmax(p_uop_A + p_dg_A + p_fin, 0),
    p_sum_B = pmax(p_uop_B + p_dg_B + p_fin, 0)
  )

# Reszta kodu pozostaje bez zmian...
summarize_results <- function() {
  sums <- dane_3 %>% 
    summarise(
      current = sum(p_sum, na.rm = TRUE),
      scenario_A = sum(p_sum_A, na.rm = TRUE),
      scenario_B = sum(p_sum_B, na.rm = TRUE)
    )
  
  data.frame(
    Scenariusz = c("Obecny system", "Scenariusz A", "Scenariusz B"),
    Suma_podatków = format(c(sums$current, sums$scenario_A, sums$scenario_B), 
                           big.mark = " ", nsmall = 0),
    Różnica_abs = format(c(0, sums$scenario_A - sums$current, sums$scenario_B - sums$current), 
                         big.mark = " ", nsmall = 0),
    Zmiana_proc = paste0(round(c(0, 
                                 (sums$scenario_A - sums$current)/sums$current * 100,
                                 (sums$scenario_B - sums$current)/sums$current * 100), 1), "%")
  ) %>% 
    rename(`Różnica (PLN)` = Różnica_abs,
           `Zmiana (%)` = Zmiana_proc)
}

# Wykres zmian podatków wg decyli
plot_tax_changes <- function(data, scenario) {
  tax_col <- ifelse(scenario == "A", "p_uop_A + p_dg_A", "p_uop_B + p_dg_B")
  
  data %>%
    filter((d_uop > 0 | (forma_opodatkowania == "skala" & d_dg > 0))) %>%
    mutate(
      doch_total = d_uop + d_dg,
      podatek_total_przed = pmax(p_uop + p_dg, 0),  # Zabezpieczenie
      podatek_total_po = pmax(!!rlang::parse_expr(tax_col), 0),  # Zabezpieczenie
      roznica = podatek_total_po - podatek_total_przed,
      decyl = ntile(doch_total, 10)
    ) %>%
    group_by(decyl) %>%
    summarise(srednia_roznica = mean(roznica, na.rm = TRUE)) %>%  # Zabezpieczenie NA
    ggplot(aes(factor(decyl), srednia_roznica)) +
    geom_col(fill = "steelblue") +
    labs(
      x = "Grupa decylowa dochodu",
      y = "Średnia zmiana podatku (PLN)",
      title = paste("Zmiana obciążeń podatkowych po scenariuszu", scenario)
    ) +
    theme_minimal()
}

# Wyniki
print(summarize_results())

# Wykresy
plot_tax_changes(dane_3, "A")
plot_tax_changes(dane_3, "B")

