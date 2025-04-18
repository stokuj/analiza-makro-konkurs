import pandas as pd
from decimal import Decimal, getcontext, ROUND_HALF_UP
import re

# Ustawienia precyzji
getcontext().prec = 20
getcontext().rounding = ROUND_HALF_UP

# ----------------------------------------------------------------------------------
# Poprawiona funkcja konwersji
# ----------------------------------------------------------------------------------
def safe_decimal(value) -> Decimal:
    """Konwertuje dowolną wartość na Decimal z obsługą błędów."""
    try:
        cleaned = re.sub(r'[^\d.,-]', '', str(value))
        if not cleaned:
            return Decimal('0')
        
        # Obsługa różnych formatów liczbowych
        if ',' in cleaned and '.' in cleaned:
            parts = re.split('[,.]', cleaned)
            integer_part = ''.join(parts[:-1]).replace('.', '').replace(',', '')
            decimal_part = parts[-1]
        elif ',' in cleaned:
            parts = cleaned.split(',')
            integer_part = parts[0].replace('.', '')
            decimal_part = parts[1] if len(parts) > 1 else '0'
        else:
            parts = cleaned.split('.')
            integer_part = parts[0].replace(',', '')
            decimal_part = parts[1] if len(parts) > 1 else '0'
        
        # Budowanie liczby
        number_str = f"{integer_part}.{decimal_part}".strip('.')
        return Decimal(number_str)
    except:
        return Decimal('0')

# ----------------------------------------------------------------------------------
# Funkcje podatkowe z zaokrągleniem
# ----------------------------------------------------------------------------------
def oblicz_podatek_uop(dochod) -> Decimal:
    dochod_dec = safe_decimal(dochod)
    podstawa = max(dochod_dec - Decimal('25000'), Decimal('0'))
    if podstawa <= Decimal('100000'):
        return (podstawa * Decimal('0.15')).quantize(Decimal('0.000'))
    return (Decimal('15000') + (podstawa - Decimal('100000')) * Decimal('0.4')).quantize(Decimal('0.000'))

def oblicz_podatek_dg(row) -> Decimal:
    d_uop = safe_decimal(row['d_uop'])
    d_dg = safe_decimal(row['d_dg'])
    forma = str(row['forma_opodatkowania']).strip().lower()
    
    if forma == 'skala':
        suma = d_uop + d_dg
        podstawa = max(suma - Decimal('25000'), Decimal('0'))
        if podstawa <= Decimal('100000'):
            return (podstawa * Decimal('0.15')).quantize(Decimal('0.000'))
        return (Decimal('15000') + (podstawa - Decimal('100000')) * Decimal('0.4')).quantize(Decimal('0.000'))
    elif forma == 'liniowka':
        return (max(d_dg, Decimal('0')) * Decimal('0.20')).quantize(Decimal('0.000'))
    return Decimal('0.000')

def oblicz_podatek_fin(dochod) -> Decimal:
    return (max(safe_decimal(dochod), Decimal('0')) * Decimal('0.25')).quantize(Decimal('0.000'))

# ----------------------------------------------------------------------------------
# Główne przetwarzanie
# ----------------------------------------------------------------------------------

# Wczytaj dane jako tekst
df = pd.read_csv('data/data.csv', sep=';', dtype=str)

# Oblicz podatki z precyzją
df['podatek_uop'] = df['d_uop'].apply(oblicz_podatek_uop)
df['podatek_dg'] = df.apply(oblicz_podatek_dg, axis=1)
df['podatek_fin'] = df['d_fin'].apply(oblicz_podatek_fin)

# Suma wiersza z zaokrągleniem
df['suma_wiersza'] = df.apply(
    lambda row: row['podatek_uop'] + row['podatek_dg'] + row['podatek_fin'], 
    axis=1
).apply(lambda x: x.quantize(Decimal('0.000')))

# Suma kumulacyjna z precyzją
df['suma_kumulacyjna'] = df['suma_wiersza'].cumsum().apply(
    lambda x: x.quantize(Decimal('0.000')) if isinstance(x, Decimal) else Decimal('0.000')
)

# Konwersja do float do zapisu
df = df.applymap(lambda x: float(x) if isinstance(x, Decimal) else x)

# Zapisz wyniki
df.to_csv('data/python.csv', sep=';', decimal=',', index=False, float_format='%.4f')

print("Przykładowe dane:")
print(df.head().to_string(float_format=lambda x: "{:.4f}".format(x)))

# Podsumowanie
suma_uop = round(df['podatek_uop'].sum(), 4)
suma_dg = round(df['podatek_dg'].sum(), 4)
suma_fin = round(df['podatek_fin'].sum(), 4)

print("\nPodsumowanie z dokładnością do 4 miejsc dziesiętnych:")
print(f"{'Podatek UoP:':<25} {suma_uop:>10.4f} PLN")
print(f"{'Podatek DG:':<25} {suma_dg:>10.4f} PLN")
print(f"{'Podatek FIN:':<25} {suma_fin:>10.4f} PLN")
print(f"{'ŁĄCZNIE:':<25} {suma_uop + suma_dg + suma_fin:>10.4f} PLN")

