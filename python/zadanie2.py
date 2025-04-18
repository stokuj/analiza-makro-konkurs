import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Wczytanie danych z pliku CSV
df = pd.read_csv(r'E:\Projekty\analiza-makro-konkurs\data\data.csv', sep=';', decimal=',')


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