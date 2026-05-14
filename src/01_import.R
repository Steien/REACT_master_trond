# =============================================================================
# 01_import.R
# Leser inn og rydder radata fra DXA_data_20242025.xlsx og REACT_data_til_studenter.xlsx
# Output: data/processed/dxa_clean.rds
#         data/processed/bakgrunn_clean.rds
#         data/processed/antropometri_clean.rds
#
# Merk: REACT_data_til_studenter inneholder kun RCT-deltakere (fp 11-99).
# =============================================================================

library(readxl)
library(dplyr)

# =============================================================================
# DEL 1: DXA-data
# =============================================================================

dxa_raw <- read_excel("data/raw/DXA_data_20242025.xlsx")

# Handter deltakere som var for brede for DXA-maskinen:
# - type = "f": full scan, brukes direkte
# - type = "r": hoyre scan med DXA-estimert venstreside (speiling) -- brukes
# - type = "l": forkastes
# Kilde: ISCD Official Positions 2023

dxa_clean <- dxa_raw %>%
  filter(!is.na(id)) %>%
  filter(type != "l") %>%
  rename(
    fat_android_pct = `fat_android_%`,
    fat_total_pct   = `fat_total_%`
  ) %>%
  mutate(
    id              = as.integer(id),
    year            = as.integer(year),
    time            = factor(time, levels = c("pre", "post")),
    sex             = factor(sex, levels = c("f", "m"),
                             labels = c("Kvinne", "Mann")),
    type            = factor(type, levels = c("f", "r"),
                             labels = c("Full scan", "Hoyre (speiling)")),
    dxa_kg          = suppressWarnings(as.numeric(dxa_kg)),
    seca_kg         = suppressWarnings(as.numeric(seca_kg)),
    fat_android_pct = suppressWarnings(as.numeric(fat_android_pct)),
    fat_total_pct   = suppressWarnings(as.numeric(fat_total_pct)),
    fat_android_g   = suppressWarnings(as.numeric(fat_android_g)),
    fat_total_g     = suppressWarnings(as.numeric(fat_total_g)),
    LBM             = suppressWarnings(as.numeric(LBM))
  )

# Datakorreksjon bekreftet av veileder: FP 45 og FP 46 hadde byttet DXA-scans.
# FP 45 sine radata-verdier tilhorer FP 46 (radata: LBM=58041 g, fysiologisk umulig
# for FP 45 sin profil; en endring pa -12581 g LBM pa 12 uker er ikke plausibelt).
# Korrekte verdier for FP 45 er hentet fra FP 46-raden i
# DEXA resultater 2025_07.04.26.xlsx. FP 46 har ingen egne DXA-rader i datasettet og ekskluderes automatisk.
dxa_clean <- dxa_clean %>%
  mutate(
    LBM          = case_when(
      id == 45 & time == "pre"  ~ 45460,
      id == 45 & time == "post" ~ 45476,
      TRUE ~ LBM
    ),
    fat_total_pct = case_when(
      id == 45 & time == "pre"  ~ 34.4,
      id == 45 & time == "post" ~ 33.8,
      TRUE ~ fat_total_pct
    ),
    fat_total_g = case_when(
      id == 45 & time == "pre"  ~ 23845,
      id == 45 & time == "post" ~ 23168,
      TRUE ~ fat_total_g
    )
  )

# Behold kun FP som har BEGGE malinger (pre + post)
fp_begge <- dxa_clean %>%
  group_by(id) %>%
  summarise(har_pre  = any(time == "pre"),
            har_post = any(time == "post"),
            .groups  = "drop") %>%
  filter(har_pre & har_post) %>%
  pull(id)

dxa_clean <- dxa_clean %>% filter(id %in% fp_begge)

cat("DXA-data (kun pre+post-par):", nrow(dxa_clean), "rader,",
    length(unique(dxa_clean$id)), "unike deltakere\n")
cat("Scan-type:\n")
print(table(dxa_clean$type))


# =============================================================================
# DEL 2: Bakgrunn fra REACT_data
# =============================================================================

bakgrunn_raw <- read_excel(
  "data/raw/REACT_data_til_studenter.xlsx",
  sheet = "Bakgrunn",
  skip  = 1
)

# Korrigerte alderskategorier fra veileder (16.03.26)
# WHO-inndeling: Unge voksne 18-44, Middelaldrende 45-59, Eldre voksne 60-74, Gamle eldre 75+
ald_kor <- read_excel(
  "data/raw/Alderskategorier til studenter 16.03.26.xlsx",
  skip = 5
) %>%
  select(fp = fp, aldersgruppe_WHO = aldersgruppe_WHO_2026) %>%
  filter(!is.na(fp), fp != "fp") %>%
  mutate(fp = as.integer(suppressWarnings(as.numeric(fp))))

bakgrunn_clean <- bakgrunn_raw %>%
  select(
    fp,
    aar                    = "\u00c5r",
    treatment,
    kjonn                  = "kj\u00f8nn",
    alder,
    round                  = "round (pilot, main_1, main_2)",
    dropout,
    kreftform              = Kreftform_forenklet_utkast,
    dager_siden_behandling = "dager siden siste behandling"
  ) %>%
  filter(!is.na(fp)) %>%
  mutate(
    fp                     = as.integer(fp),
    aar                    = as.integer(aar),
    alder                  = suppressWarnings(as.numeric(alder)), # tom i kildedata; median/IQR hardkodet i 02_deskriptiv.R
    dropout                = !is.na(dropout) & dropout %in% c("y", "bytte"),
    dager_siden_behandling = suppressWarnings(as.numeric(dager_siden_behandling))
  ) %>%
  left_join(ald_kor, by = "fp") %>%
  mutate(
    treatment        = factor(treatment, levels = c("digital", "stedlig")),
    kjonn            = factor(kjonn, levels = c("f", "m"),
                              labels = c("Kvinne", "Mann")),
    aldersgruppe_WHO = factor(
      tolower(trimws(aldersgruppe_WHO)),
      levels = c("unge voksne", "middelaldrende", "eldre voksne", "gamle eldre"),
      labels = c("Unge voksne", "Middelaldrende", "Eldre voksne", "Gamle eldre")
    )
  )

# Manuelle korreksjoner bekreftet av veileder/datasett:
# fp 24: registrert som Prostatakreft → Brystkreft
# fp 63, fp 77: ekstreme verdier for dager siden behandling (~45900) — oppga aldri
#               dette selv, settes til NA
bakgrunn_clean <- bakgrunn_clean %>%
  mutate(
    kreftform              = case_when(
      fp == 24             ~ "Brystkreft",
      TRUE                 ~ kreftform
    ),
    dager_siden_behandling = case_when(
      fp %in% c(63, 77)   ~ NA_real_,
      TRUE                 ~ dager_siden_behandling
    )
  )

cat("\nBakgrunn:", nrow(bakgrunn_clean), "deltakere totalt\n")
cat("  2024:", sum(bakgrunn_clean$aar == 2024, na.rm = TRUE), "\n")
cat("  2025:", sum(bakgrunn_clean$aar == 2025, na.rm = TRUE), "\n")
cat("Grupper:\n")
print(table(bakgrunn_clean$treatment))
cat("Dropouts 2024:", sum(bakgrunn_clean$dropout & bakgrunn_clean$aar == 2024,
                          na.rm = TRUE), "\n")
cat("Dropouts 2025:", sum(bakgrunn_clean$dropout & bakgrunn_clean$aar == 2025,
                          na.rm = TRUE), "\n")


# =============================================================================
# DEL 3: Antropometri fra REACT_data
# =============================================================================

antropometri_raw <- read_excel(
  "data/raw/REACT_data_til_studenter.xlsx",
  sheet = "Antropometri",
  skip  = 1
)

antropometri_clean <- antropometri_raw %>%
  select(
    fp,
    dato  = Dato,
    test,
    vekt,
    hoyde = "h\u00f8yde",
    bmi   = "bmi_est (kg/m2)",
    midje = waist_circ
  ) %>%
  filter(!is.na(fp)) %>%
  mutate(
    fp    = as.integer(suppressWarnings(as.numeric(gsub("^FP", "", fp)))),
    test  = tolower(trimws(test)),
    vekt  = suppressWarnings(as.numeric(vekt)),
    hoyde = suppressWarnings(as.numeric(hoyde)),
    bmi   = suppressWarnings(as.numeric(bmi)),
    midje = suppressWarnings(as.numeric(gsub("[^0-9.]", "", midje)))
  ) %>%
  filter(!is.na(fp))

cat("\nAntropometri:", nrow(antropometri_clean), "rader\n")
cat("Test-tidspunkt:\n")
print(table(antropometri_clean$test, useNA = "ifany"))


# =============================================================================
# DEL 4: Lagre rensede data
# =============================================================================

saveRDS(dxa_clean,          "data/processed/dxa_clean.rds")
saveRDS(bakgrunn_clean,     "data/processed/bakgrunn_clean.rds")
saveRDS(antropometri_clean, "data/processed/antropometri_clean.rds")

cat("\nFerdig! Filer lagret i data/processed/\n")
