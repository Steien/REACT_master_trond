# =============================================================================
# flytskjema_tall.R
# Henter ut tall til flytskjema
# Kjør 01_import.R først hvis processed-filene ikke er oppdatert
# =============================================================================

library(dplyr)
library(readxl)

# Les inn rensede data
bakgrunn     <- readRDS("data/processed/bakgrunn_clean.rds")
dxa_raw_full <- read_excel("data/raw/DXA_data_20242025.xlsx") %>%
  filter(!is.na(id)) %>%
  mutate(
    id   = as.integer(id),
    year = as.integer(year),
    time = tolower(trimws(time)),
    type = tolower(trimws(type))
  ) %>%
  filter(type != "l")  # kast l-scans (samme logikk som 01_import.R)

cat("=== TALL TIL FLYTSKJEMA ===\n\n")

# --- Totalt inkludert i bakgrunn (RCT) ---
cat("Totalt i REACT_data (RCT):", nrow(bakgrunn), "\n")
cat("  2024:", sum(bakgrunn$aar == 2024, na.rm = TRUE), "\n")
cat("  2025:", sum(bakgrunn$aar == 2025, na.rm = TRUE), "\n")
cat("  Registrerte dropouts:", sum(bakgrunn$dropout, na.rm = TRUE), "\n\n")

# --- Hvem har pre-scan i DXA ---
fp_med_pre  <- dxa_raw_full %>% filter(time == "pre")  %>% pull(id) %>% unique()
fp_med_post <- dxa_raw_full %>% filter(time == "post") %>% pull(id) %>% unique()

cat("FP med pre-scan i DXA:", length(fp_med_pre), "\n")
cat("FP med post-scan i DXA:", length(fp_med_post), "\n\n")

# --- FP i bakgrunn som MANGLER pre-scan ---
fp_alle <- bakgrunn$fp
fp_mangler_pre <- setdiff(fp_alle, fp_med_pre)
cat("FP i bakgrunn som mangler pre-scan:", length(fp_mangler_pre), "\n")
if (length(fp_mangler_pre) > 0) {
  info <- bakgrunn %>%
    filter(fp %in% fp_mangler_pre) %>%
    select(fp, aar, treatment, dropout)
  print(info)
}

cat("\n")

# --- FP med pre-scan som MANGLER post-scan og IKKE er registrert som dropout ---
fp_har_pre_mangler_post <- setdiff(fp_med_pre, fp_med_post)
ikke_dropout_mangler_post <- bakgrunn %>%
  filter(fp %in% fp_har_pre_mangler_post, dropout == FALSE)

cat("FP med pre-scan men mangler post-scan:", length(fp_har_pre_mangler_post), "\n")
cat("  Herav registrert som dropout:", length(fp_har_pre_mangler_post) - nrow(ikke_dropout_mangler_post), "\n")
cat("  Herav IKKE registrert som dropout (mangler post uforklart):", nrow(ikke_dropout_mangler_post), "\n")
if (nrow(ikke_dropout_mangler_post) > 0) {
  print(ikke_dropout_mangler_post %>% select(fp, aar, treatment, dropout))
}

cat("\n")

# --- FP med begge (pre + post) = de som er med i analysen ---
fp_begge <- intersect(fp_med_pre, fp_med_post)
cat("FP med både pre- og post-scan (inkludert i analyse):", length(fp_begge), "\n")
cat("  2024:", sum(bakgrunn$fp[bakgrunn$aar == 2024] %in% fp_begge), "\n")
cat("  2025:", sum(bakgrunn$fp[bakgrunn$aar == 2025] %in% fp_begge), "\n")

cat("\n=== DETALJERT OVERSIKT: HVEM MANGLER HVA ===\n\n")

alle_fp_info <- bakgrunn %>%
  mutate(
    har_pre  = fp %in% fp_med_pre,
    har_post = fp %in% fp_med_post
  ) %>%
  select(fp, aar, treatment, dropout, har_pre, har_post)

cat("--- Mangler pre-scan ---\n")
print(alle_fp_info %>% filter(!har_pre) %>% arrange(aar, fp))

cat("\n--- Har pre men mangler post-scan ---\n")
print(alle_fp_info %>% filter(har_pre, !har_post) %>% arrange(aar, fp))

cat("\n--- Alle FP og DXA-status (full oversikt) ---\n")
print(alle_fp_info %>% arrange(aar, fp), n = 100)
