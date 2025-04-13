import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Wczytanie danych z pliku CSV
df = pd.read_csv('data/data.csv', sep=';', decimal=',')

# ----------------------------------------------------------------------------------
# Funkcje do obliczania podatków
# ----------------------------------------------------------------------------------

def oblicz_podatek_uop(dochody_uop: pd.Series) -> pd.Series:
    """Oblicza podatek od pracy najemnej (zawsze skala podatkowa)."""
    podatek = []
    for dochod in dochody_uop:
        if dochod <= 25_000:
            podatek.append(0)
        elif dochod <= 125_000:  # 25k wolne + 100k progu
            podatek.append(0.15 * (dochod - 25_000))
        else:
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

# ----------------------------------------------------------------------------------
# Część 1: Analiza dla pracy najemnej (tylko d_uop > 0, pozostałe dochody = 0)
# ----------------------------------------------------------------------------------

print("\n" + "="*50)
print("(2.1) Teoretyczny wykres klina podatkowego dla pracy najemnej")
print("="*50)

# Filtruj podatników tylko z dochodem z pracy najemnej
df_praca = df[(df['d_uop'] > 0) & (df['d_dg'] == 0) & (df['d_fin'] == 0)].copy()

# Oblicz podatek dla pracy najemnej (skala podatkowa)
df_praca['podatek_uop'] = np.where(
    df_praca['d_uop'] <= 25_000,
    0,
    np.where(
        df_praca['d_uop'] <= 125_000,
        0.15 * (df_praca['d_uop'] - 25_000),
        0.15 * 100_000 + 0.4 * (df_praca['d_uop'] - 125_000)
    )
)

# 1. Teoretyczny wykres klina podatkowego
def klin_podatkowy(dochod):
    if dochod <= 25_000:
        return 0
    elif dochod <= 125_000:
        return (0.15 * (dochod - 25_000)) / dochod
    else:
        return (0.15 * 100_000 + 0.4 * (dochod - 125_000)) / dochod

dochod_teoretyczny = np.linspace(1, 500_000, 1000)
klin_teoretyczny = [klin_podatkowy(d) for d in dochod_teoretyczny]

plt.figure(figsize=(10, 6))
plt.plot(dochod_teoretyczny, klin_teoretyczny, label='Teoretyczny klin podatkowy', color='blue')
plt.xlabel('Dochód brutto (PLN)')
plt.ylabel('Klin podatkowy (% dochodu)')
plt.title('(2.1) Klin podatkowy dla pracy najemnej (teoria)')
plt.grid(True)
plt.legend()
plt.show()

# ----------------------------------------------------------------------------------
print("\n" + "="*50)
print("(2.2) Empiryczna stopa opodatkowania pracy najemnej w decylach")
print("="*50)

# 2. Empiryczna stopa opodatkowania w decylach
df_praca['decyl'] = pd.qcut(df_praca['d_uop'], q=10, labels=False, duplicates='drop')
stopa_decyl = df_praca.groupby('decyl').apply(
    lambda x: x['podatek_uop'].sum() / x['d_uop'].sum()
).reset_index(name='stopa')

plt.figure(figsize=(10, 6))
sns.barplot(x='decyl', y='stopa', data=stopa_decyl, palette='viridis')
plt.xlabel('Decyl dochodu')
plt.ylabel('Średnia stopa opodatkowania')
plt.title('(2.2) Empiryczna stopa opodatkowania pracy najemnej w decylach')
plt.xticks(ticks=range(10), labels=[f'D{i+1}' for i in range(10)])
plt.show()

# ----------------------------------------------------------------------------------
print("\n" + "="*50)
print("(2.3) Ocena progresywności opodatkowania pracy")
print("="*50)

# 3. Ocena progresywności
print("Czy system jest progresywny?")
print("Odpowiedź: Tak, średnia stopa rośnie z decylem:", stopa_decyl['stopa'].is_monotonic_increasing)

# ----------------------------------------------------------------------------------
# Część 2: Analiza dla działalności gospodarczej (tylko d_dg > 0, pozostałe dochody = 0)
# ----------------------------------------------------------------------------------
print("\n" + "="*50)
print("(2.4) Teoretyczny wykres klina podatkowego dla działalności gospodarczej")
print("="*50)

df_dg = df[(df['d_dg'] > 0) & (df['d_uop'] == 0) & (df['d_fin'] == 0)].copy()

# Oblicz podatek dla DG (optymalny wybór: skala vs liniowy)
def podatek_dg(row):
    dochod = row['d_dg']
    if row['forma_opodatkowania'] == 'skala':
        if dochod <= 25_000:
            return 0
        elif dochod <= 125_000:
            return 0.15 * (dochod - 25_000)
        else:
            return 0.15 * 100_000 + 0.4 * (dochod - 125_000)
    elif row['forma_opodatkowania'] == 'liniowka':
        return 0.2 * dochod
    else:
        return 0

df_dg['podatek_dg'] = df_dg.apply(podatek_dg, axis=1)

# 4. Teoretyczny klin dla DG (optymalny wybór)
def klin_dg(dochod):
    # Porównaj podatek na skali vs liniowy
    podatek_skala = 0 if dochod <= 25_000 else (
        0.15 * (dochod - 25_000) if dochod <= 125_000 else 
        0.15 * 100_000 + 0.4 * (dochod - 125_000)
    )
    podatek_liniowy = 0.2 * dochod
    return min(podatek_skala, podatek_liniowy) / dochod

klin_dg_teoretyczny = [klin_dg(d) for d in dochod_teoretyczny]

plt.figure(figsize=(10, 6))
plt.plot(dochod_teoretyczny, klin_dg_teoretyczny, label='Klin DG (optymalny wybór)', color='red')
plt.xlabel('Dochód brutto (PLN)')
plt.ylabel('Klin podatkowy (% dochodu)')
plt.title('(2.4) Klin podatkowy dla działalności gospodarczej (teoria)')
plt.grid(True)
plt.legend()
plt.show()

# ----------------------------------------------------------------------------------
print("\n" + "="*50)
print("(2.5) Empiryczna stopa opodatkowania DG w decylach")
print("="*50)

# 5. Empiryczna stopa w decylach
df_dg['decyl'] = pd.qcut(df_dg['d_dg'], q=10, labels=False, duplicates='drop')
stopa_dg_decyl = df_dg.groupby('decyl').apply(
    lambda x: x['podatek_dg'].sum() / x['d_dg'].sum()
).reset_index(name='stopa')

plt.figure(figsize=(10, 6))
sns.barplot(x='decyl', y='stopa', data=stopa_dg_decyl, palette='magma')
plt.xlabel('Decyl dochodu')
plt.ylabel('Średnia stopa opodatkowania')
plt.title('(2.5) Empiryczna stopa opodatkowania DG w decylach')
plt.xticks(ticks=range(10), labels=[f'D{i+1}' for i in range(10)])
plt.show()

# ----------------------------------------------------------------------------------
print("\n" + "="*50)
print("(2.6) Ocena progresywności opodatkowania działalności gospodarczej")
print("="*50)

# 6. Ocena progresywności DG
print("Czy opodatkowanie DG jest progresywne?")
print("Odpowiedź: Wynik mieszany (progresja do ~175 tys. PLN, potem regresja):", 
      not stopa_dg_decyl['stopa'].is_monotonic_increasing)