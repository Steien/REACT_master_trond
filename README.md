# REACT_MASTER — Reproduserbar analysepipeline

**Masteroppgave — IDR4001**
Universitetet i Innlandet — Fakultet for helse- og sosialvitenskap

**Tittel:**
Effekt av 12 ukers digital hjemmebasert styrketrening på muskelmasse og fettmasse hos ferdigbehandlede kreftpasienter sammenlignet med stedlig styrketrening i gruppe.

**Forfatter:** Trond Steien
**År:** 2026

---

## Studiedesign

Randomisert kontrollert studie (RCT) med to parallelle grupper:

| Gruppe | Beskrivelse |
|---|---|
| `digital` | Hjemmebasert styrketrening via forhåndsinnspilte treningsvideoer |
| `stedlig` | Veiledet gruppebasert styrketrening med oppmøte |

- **Intervensjonslengde:** 12 uker (2 økter per uke, 24 planlagte økter)
- **Populasjon:** Ferdigbehandlede kreftpasienter
- **Utfallsmål:** Mager masse (LBM, gram) og total fettprosent (%) målt med DXA
- **Statistisk metode:** TOST-ekvivalenstest via ANCOVA

---

## Mappestruktur

```
REACT_MASTER/
  src/
    01_import.R          # Import og rensing av rådata → 3 .rds-filer
    02_deskriptiv.R      # Deskriptiv statistikk + Tabell 4 og Tabell 5
    03_flytskjema.R      # CONSORT flytskjema (Figur 1)
    04_analyse.R         # TOST-ekvivalenstest + frafallsanalyse
    05_figurer.R         # Boksplot og ekvivalensplot (Figur 2–5)
  data/
    raw/
      DXA_data_20242025.xlsx                         # DXA-målinger 2024 og 2025
      REACT_data_til_studenter.xlsx                  # Bakgrunn og antropometri
      Alderskategorier til studenter 16.03.26.xlsx   # Korrigerte WHO-alderskategorier
    processed/                                       # Genereres av 01_import.R
      dxa_clean.rds
      bakgrunn_clean.rds
      antropometri_clean.rds
  output/
    tables/
      tabell4_baseline.docx     # Baseline-karakteristikker
      tabell5_etterlevelse.docx # Etterlevelse
    figures/
      flytskjema.png            # CONSORT flytskjema
      ekvivalens_lbm.png        # Figur 2
      boksplot_lbm.png          # Figur 3
      ekvivalens_fat.png        # Figur 4
      boksplot_fat.png          # Figur 5
  renv.lock                     # Pakkelåsfil for reproduserbart miljø
```

---

## Reproduksjon av analysen

### 1. Krav

- R versjon 4.4.2 (2024-10-31)
- RStudio versjon 2026.01.0+392 "Apple Blossom"
- Pakker håndteres av `renv` — se `renv.lock` for eksakte versjoner

### 2. Gjenopprett pakkemiljø

Åpne prosjektet i RStudio og kjør:

```r
renv::restore()
```

### 3. Kjør skriptene i rekkefølge

```r
source("src/01_import.R")      # Les inn og rens rådata
source("src/02_deskriptiv.R")  # Lag tabeller
source("src/03_flytskjema.R")  # Lag flytskjema
source("src/04_analyse.R")     # Kjør statistiske analyser
source("src/05_figurer.R")     # Lag figurer
```

Alle output-filer lagres automatisk i `output/`.

---

## Datakvalitet og manuelle korreksjoner

Følgende korreksjoner er bekreftet av veileder og dokumentert i skriptkode:

| FP | Korreksjon | Kilde |
|---|---|---|
| FP 24 | Diagnose endret fra Prostatakreft → Brystkreft | Veileder |
| FP 45 | DXA-verdier byttet med FP 46 (fysiologisk verifisert) | Veileder + rådata |
| FP 46 | Har ingen egne DXA-rader; ekskluderes | Verifisert mot rådata |
| FP 63 | Dager siden behandling satt til NA (~45 900 dager, umulig verdi) | Verifisert mot rådata |
| FP 77 | Dager siden behandling satt til NA; behandlet som dropout | Verifisert mot rådata |
| FP 80 | Registrert som dropout i bakgrunnsdata, men har begge DXA-scanner → fullført | Verifisert mot rådata |

---

## Nøkkeltall

| | |
|---|---|
| Randomiserte RCT-deltakere | 70 (35 digital, 35 stedlig) |
| Frafall | 17 (13 digital, 4 stedlig) |
| Fullførte intervensjon | 53 |
| Analyseutvalg (komplett DXA) | 50 (21 digital, 29 stedlig) |

---

## Ekvivalensgrenser

Basert på Brown et al. (2019) — 50 % av nedre konfidensintervallgrense:

| Utfallsmål | Margin |
|---|---|
| Mager masse (LBM) | ±380 g |
| Total fettprosent | ±0.35 prosentpoeng |

---

## Programvare og pakker

| Programvare/pakke | Versjon | Referanse |
|---|---|---|
| R | 4.4.2 | R Core Team (2024) |
| RStudio | 2026.01.0+392 | Posit team (2026) |
| marginaleffects | 0.32.0 | Arel-Bundock et al. (2024) |
| dplyr | 1.1.4 | Wickham et al. (2023) |
| tidyr | 1.3.2 | Wickham et al. (2025) |
| readxl | 1.4.5 | Wickham & Bryan (2025) |
| ggplot2 | 4.0.1 | Wickham (2016) |
| flextable | 0.9.11 | Gohel & Skintzos (2026) |
| officer | 0.7.3 | Gohel et al. (2026) |
| consort | 1.2.2 | Dayim (2024) |

Eksakte pakkeversjonene er låst i `renv.lock`.

---

## AI-assistanse

Kodeskriving ble støttet av Claude Sonnet 4.6 (Anthropic, 2026).
Samtalelogg er tilgjengelig fra forfatteren på forespørsel.
