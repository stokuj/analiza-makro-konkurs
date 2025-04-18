import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Wczytanie danych
df = pd.read_csv('data/data.csv', sep=';', decimal=',')

# ----------------------------------------------------------------------------------
# Funkcje do obliczania podatków (oryginalne i reformowane)
# ----------------------------------------------------------------------------------

def oblicz_podatek_skala(dochod: float, wersja: str = 'obecna') -> float:
    """Oblicza podatek według skali (wersja: obecna, A lub B)"""
    if dochod <= 25_000:
        podstawa = 0
    else:
        podstawa = dochod - 25_000
    
    if wersja == 'obecna':
        if podstawa <= 100_000:
            return 0.15 * podstawa
        else:
            return 0.15 * 100_000 + 0.4 * (podstawa - 100_000)
    
    elif wersja == 'A':
        if podstawa <= 100_000:
            return 0.10 * podstawa
        else:
            return 0.10 * 100_000 + 0.45 * (podstawa - 100_000)
    
    elif wersja == 'B':
        # Obliczamy podatek według obecnej skali Z PODWYŻSZONĄ STAWKĄ
        if podstawa <= 100_000:
            podatek = 0.15 * podstawa
        else:
            podatek = 0.15 * 100_000 + 0.45 * (podstawa - 100_000)
        
        # Stosujemy bezzwrotną ulgę (od podatku!)
        return max(0, podatek - 2_500)
        

def oblicz_calkowity_podatek(row: pd.Series, wersja: str = 'obecna') -> float:
    """Oblicza całkowity podatek dla podatnika (uwzględniając reformy)"""
    # Dochód z pracy i DG (jeśli na skali)
    if row['forma_opodatkowania'] == 'skala':
        dochod = max(0, row['d_uop']) + max(0, row['d_dg'])
        podatek = oblicz_podatek_skala(dochod, wersja)
    else:
        podatek = oblicz_podatek_skala(max(0, row['d_uop']), wersja)
        podatek += 0.2 * max(0, row['d_dg'])  # Liniowy dla DG
    
    # Dochody kapitałowe (zawsze 25%)
    podatek += 0.25 * max(0, row['d_fin'])
    
    return podatek

# ----------------------------------------------------------------------------------
# Zadanie 3.1 - Obliczenie wpływu reform na budżet
# ----------------------------------------------------------------------------------

# Obliczenie obecnych wpływów
obecne_wplywy = df.apply(lambda x: oblicz_calkowity_podatek(x, 'obecna'), axis=1).sum()

# Obliczenie wpływów po reformie A
wplywy_A = df.apply(lambda x: oblicz_calkowity_podatek(x, 'A'), axis=1).sum()

# Obliczenie wpływów po reformie B
wplywy_B = df.apply(lambda x: oblicz_calkowity_podatek(x, 'B'), axis=1).sum()

print("(3.1) Wpływ reform na budżet:")
print(f"- Obecne wpływy: {obecne_wplywy:.2f} PLN")
print(f"- Scenariusz A: {wplywy_A:.2f} PLN (zmiana: {wplywy_A - obecne_wplywy:.2f} PLN)")
print(f"- Scenariusz B: {wplywy_B:.2f} PLN (zmiana: {wplywy_B - obecne_wplywy:.2f} PLN)\n")

# ----------------------------------------------------------------------------------
# Przygotowanie danych do analizy decylowej (tylko podatnicy na skali)
# ----------------------------------------------------------------------------------

df_skala = df[df['forma_opodatkowania'] == 'skala'].copy()
df_skala['dochod'] = df_skala['d_uop'] + df_skala['d_dg']

# Podział na decyle
df_skala['decyl'] = pd.qcut(df_skala['dochod'], q=10, labels=False)

# Obliczenie podatków dla każdej grupy
decyle = df_skala.groupby('decyl').agg(
    liczba_podatnikow=('id', 'count'),
    suma_dochodow=('dochod', 'sum'),
    obecny_podatek=('dochod', lambda x: x.apply(lambda d: oblicz_podatek_skala(d, 'obecna')).sum()),
    podatek_A=('dochod', lambda x: x.apply(lambda d: oblicz_podatek_skala(d, 'A')).sum()),
    podatek_B=('dochod', lambda x: x.apply(lambda d: oblicz_podatek_skala(d, 'B')).sum())
).reset_index()

# Obliczenie zmian
decyle['zmiana_A'] = decyle['podatek_A'] - decyle['obecny_podatek']
decyle['zmiana_B'] = decyle['podatek_B'] - decyle['obecny_podatek']

# ----------------------------------------------------------------------------------
# Zadanie 3.2 - Wykres zmian dla scenariusza A
# ----------------------------------------------------------------------------------

plt.figure(figsize=(10, 6))
bars = plt.bar(decyle['decyl'], decyle['zmiana_A'], color='skyblue')
plt.title('(3.2) Zmiana obciążeń podatkowych - Scenariusz A')
plt.xlabel('Grupa decylowa (1 = najbiedniejsi, 10 = najbogatsi)')
plt.ylabel('Łączna zmiana podatku [PLN]')
plt.xticks(range(10))
plt.grid(axis='y', linestyle='--')

# Dodanie etykiet z wartościami
for bar in bars:
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2., height,
            f'{height:,.0f}',
            ha='center', va='bottom')

plt.tight_layout()
plt.savefig('wykres_scenariusz_A.png')
plt.close()

# ----------------------------------------------------------------------------------
# Zadanie 3.3 - Wykres zmian dla scenariusza B
# ----------------------------------------------------------------------------------

plt.figure(figsize=(10, 6))
bars = plt.bar(decyle['decyl'], decyle['zmiana_B'], color='lightgreen')
plt.title('(3.3) Zmiana obciążeń podatkowych - Scenariusz B')
plt.xlabel('Grupa decylowa (1 = najbiedniejsi, 10 = najbogatsi)')
plt.ylabel('Łączna zmiana podatku [PLN]')
plt.xticks(range(10))
plt.grid(axis='y', linestyle='--')

# Dodanie etykiet z wartościami
for bar in bars:
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2., height,
            f'{height:,.0f}',
            ha='center', va='bottom')

plt.tight_layout()
plt.savefig('wykres_scenariusz_B.png')
plt.show
plt.close()

# ----------------------------------------------------------------------------------
# Zadanie 3.4 - Porównanie skutków redystrybucyjnych
# ----------------------------------------------------------------------------------

print("(3.4) Porównanie skutków redystrybucyjnych:")
print("- Scenariusz A:")
print("  * Silniejsze progresywne efekty - większe ulgi dla niższych decyli")
print("  * Znaczne zwiększenie obciążeń dla najwyższych decyli")
print("  * Redystrybucja na korzyść klas średnich")
print("\n- Scenariusz B:")
print("  * Uniwersalna ulga (2.5k PLN) najbardziej pomaga najbiedniejszym")
print("  * Umiarkowane zwiększenie obciążeń dla najbogatszych")
print("  * Mniejsza progresywność niż w scenariuszu A")
print("\nPodsumowanie:")
print("Scenariusz A ma silniejsze efekty redystrybucyjne, podczas gdy")
print("Scenariusz B łagodniej wpływa na najbogatszych, dając większe")
print("korzyści absolutne najbiedniejszym.")