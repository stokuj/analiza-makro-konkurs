import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Zadanie 1
# Ile wynosi suma wpływów do budżetu Fiskalii z tytułu wszystkich trzech istniejących podatków dochodowych?
# Wczytanie danych z pliku CSV'

df = pd.read_csv('data/data.csv', sep=';', decimal=',')

# ----------------------------------------------------------------------------------
# Funkcje do obliczania podatków
# ----------------------------------------------------------------------------------

def oblicz_podatek_uop(dochody_uop: pd.Series) -> pd.Series:
    """Oblicza podatek od pracy najemnej (zawsze skala podatkowa)."""
    podatek = []
    for dochod in dochody_uop:
        # Zabezpieczenie dla ujemnych wartości dochodu
        dochod = max(0, dochod)
        if dochod <= 25_000:
            podatek.append(0)
        elif dochod <= 125_000:  # 25k wolne + 100k progu
            podatek.append(0.15 * (dochod - 25_000))
        else:  # 25k wolne + 100k progu + 40% powyżej 125k
            podatek.append(0.15 * 100_000 + 0.4 * (dochod - 125_000))
    return pd.Series(podatek, index=dochody_uop.index)


def oblicz_podatek_dg(row: pd.Series) -> float:
    """Oblicza podatek od działalności gospodarczej (skala lub liniowy)."""
    dochod = max(0, row['d_dg'])  # Ujemne dochody = 0
    
    if row['forma_opodatkowania'] == 'skala':
        # Suma z dochodem z pracy jeśli oba na skali
        suma_dochodow = max(0, row['d_uop']) + dochod
        if suma_dochodow <= 25_000:
            return 0
        elif suma_dochodow <= 125_000:
            return 0.15 * (suma_dochodow - 25_000)
        else:
            return 0.15 * 100_000 + 0.4 * (suma_dochodow - 125_000)
    elif row['forma_opodatkowania'] == 'liniowka':
        return 0.2 * dochod
    else:
        return 0  # Dla "nie dotyczy"

def oblicz_podatek_fin(dochody_fin: pd.Series) -> pd.Series:
    """Oblicza podatek od dochodów kapitałowych (zawsze 25%)."""
    return 0.25 * dochody_fin.clip(lower=0)  # Ujemne = 0

# ----------------------------------------------------------------------------------
# Obliczenia główne
# ----------------------------------------------------------------------------------

# Oblicz podatki dla każdego typu dochodu
df['podatek_uop'] = oblicz_podatek_uop(df['d_uop'])
df['podatek_dg'] = df.apply(oblicz_podatek_dg, axis=1)
df['podatek_fin'] = oblicz_podatek_fin(df['d_fin'])

# Suma podatków dla każdego podatnika
df['calkowity_podatek'] = df['podatek_uop'] + df['podatek_dg'] + df['podatek_fin']

# Suma wpływów do budżetu
suma_wplywow = df['calkowity_podatek'].sum()

# ----------------------------------------------------------------------------------
# Wynik
# ----------------------------------------------------------------------------------

print(f"Suma wpływów do budżetu Fiskalii: {suma_wplywow:.2f} PLN\n")

# Podgląd danych z podatkami (opcjonalnie)
#print(df[['id', 'd_uop', 'd_dg', 'd_fin', 'calkowity_podatek']].head(10))