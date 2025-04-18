#porównanie.py
import pandas as pd
import numpy as np

# Wczytanie danych
df1 = pd.read_csv('data/r.csv', sep=';', decimal=',')
df2 = pd.read_csv('data/python.csv', sep=';', decimal=',')

# Kolumny do porównania
kolumny = ['podatek_uop', 'podatek_dg', 'podatek_fin','suma_kumulacyjna']

# Zamień NaN na 0 w podatkach
for kol in kolumny:
    df1[kol] = df1[kol].fillna(0)
    df2[kol] = df2[kol].fillna(0)

# Oblicz różnice i znajdź różniące się wiersze
roznice = pd.DataFrame()
for kol in kolumny:
    roznice[kol] = np.isclose(df1[kol], df2[kol], atol=0.0001)  # tolerancja 1 grosz

# Wiersze z różnicami
maska_roznic = ~roznice.all(axis=1)
df_roznice = pd.concat([
    df1.loc[maska_roznic, kolumny].add_suffix('_df1'),
    df2.loc[maska_roznic, kolumny].add_suffix('_df2')
], axis=1)


# Sprawdź i wypisz różnice
if df_roznice.empty:
    print("Brak różnic w podatkach (z tolerancją 0.0001). 🎉")
else:
    print("Wiersze z różnicami w podatkach:\n")
    print(df_roznice)

# Zapisz różnice do pliku
df_roznice.to_csv('roznice_podatki.csv', sep=';', decimal=',', index=False)