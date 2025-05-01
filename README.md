# 💰 Analiza rozkładu obciążeń podatkowych – projekt konkursowy

## 📌 Opis projektu

Projekt przygotowany na konkurs analityczny. Tematem analizy była dystrybucja obciążeń podatkowych w fikcyjnym państwie **Fiskalia**, w oparciu o dane mikropodatkowe. Celem było zbadanie, jak różne źródła dochodów i formy opodatkowania wpływają na system redystrybucji.

## 📊 Zakres analizy

- Obliczenie całkowitych wpływów z trzech podatków:
  - progresywny PIT,
  - liniowy PIT,
  - podatek od zysków kapitałowych.

- Analiza redystrybucji:
  - empiryczne i teoretyczne wykresy klina podatkowego,
  - średnia efektywna stawka opodatkowania według decyli dochodowych,
  - ocena progresji opodatkowania.

- Dwa scenariusze reform:
  - zmiana stawek,
  - ulga + zmiana progu,
  - wpływ reform na wpływy budżetowe i grupy społeczne.

- Komentarz teoretyczny dotyczący wpływu progresji na rynek pracy.

## 🛠️ Technologie

- **Język**: R
- **Biblioteki**: `dplyr`, `ggplot2`, `readr`, `scales`, `knitr`, `rmarkdown`
- **Format raportu**: RMarkdown → PDF

## 📂 Struktura

```
📁 data/         # Dane wejściowe
📁 plots/        # Wygenerowane wykresy
📄 analiza_makro.Rmd    # Główny raport (RMarkdown)
📄 wyniki.pdf           # Gotowy raport w PDF
📄 functions.R          # Pomocnicze funkcje
```

## ▶️ Uruchamianie

W RStudio lub z konsoli R:
```r
rmarkdown::render("analiza_makro.Rmd")
```

## 📄 Licencja

Projekt dostępny na licencji MIT.

---

> 📎 Projekt edukacyjny – dane i scenariusze mają charakter fikcyjny.
