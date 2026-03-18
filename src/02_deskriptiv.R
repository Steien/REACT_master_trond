# =============================================================================
# 02_deskriptiv.R
# Deskriptiv statistikk og Tabell 1 — baseline-karakteristikker per gruppe
# Output: output/tables/tabell1_baseline.csv
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# Les inn data
# =============================================================================

bakgrunn     <- readRDS("data/processed/bakgrunn_clean.rds")
antropometri <- readRDS("data/processed/antropometri_clean.rds")
dxa          <- readRDS("data/processed/dxa_clean.rds")

# Kun 2024-deltakere med begge DXA-målinger (pre+post)
# 2025-data er ikke ferdig innsamlet ennå
fp_analyse <- unique(dxa$id)

bakgrunn_analyse <- bakgrunn %>% filter(fp %in% fp_analyse, år == 2024)
antro_pre        <- antropometri %>%
  filter(fp %in% fp_analyse, test == "pre")
dxa_pre          <- dxa %>% filter(id %in% fp_analyse, time == "pre")

cat("Deltakere i analysen:", length(fp_analyse), "\n")
cat("Gruppe digital:", sum(bakgrunn_analyse$treatment == "digital"), "\n")
cat("Gruppe stedlig:", sum(bakgrunn_analyse$treatment == "stedlig"), "\n\n")


# =============================================================================
# Hjelpefunksjoner
# =============================================================================

# Oppsummerer kontinuerlige variabler: Gjennomsnitt (SD)
mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

# Oppsummerer kategoriske variabler: n (%)
n_pct <- function(x, total) {
  n <- sum(!is.na(x) & x)
  sprintf("%d (%.0f%%)", n, 100 * n / total)
}


# =============================================================================
# Bygg Tabell 1
# =============================================================================

grupper <- c("digital", "stedlig")

lag_rad <- function(variabelnavn, digital_verdi, stedlig_verdi, total_verdi) {
  data.frame(
    Variabel = variabelnavn,
    Digital  = digital_verdi,
    Stedlig  = stedlig_verdi,
    Totalt   = total_verdi,
    stringsAsFactors = FALSE
  )
}

tabell <- list()

# --- Antall ---
n_dig <- sum(bakgrunn_analyse$treatment == "digital")
n_ste <- sum(bakgrunn_analyse$treatment == "stedlig")
n_tot <- nrow(bakgrunn_analyse)
tabell[["n"]] <- lag_rad("n",
  as.character(n_dig), as.character(n_ste), as.character(n_tot))

# --- Aldersgrupper (WHO) ---
# NB: eksakt alder er ikke tilgjengelig i datasettet, kun aldersgruppe
for (grp in levels(bakgrunn_analyse$aldersgruppe_WHO)) {
  d <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
    mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  s <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
    mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  a <- bakgrunn_analyse %>% mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  tabell[[paste0("ald_", grp)]] <- lag_rad(
    paste0("  ", grp), n_pct(d, n_dig), n_pct(s, n_ste), n_pct(a, n_tot))
}

# --- Kjønn ---
kjonn_dig_k <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)
kjonn_ste_k <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)
kjonn_all_k <- bakgrunn_analyse %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)

tabell[["kvinner"]] <- lag_rad("Kvinner, n (%)",
  n_pct(kjonn_dig_k, n_dig), n_pct(kjonn_ste_k, n_ste),
  n_pct(kjonn_all_k, n_tot))

# --- Kreftform ---
kreftformer <- sort(unique(na.omit(bakgrunn_analyse$kreftform)))
for (kf in kreftformer) {
  d <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
    mutate(x = kreftform == kf) %>% pull(x)
  s <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
    mutate(x = kreftform == kf) %>% pull(x)
  a <- bakgrunn_analyse %>% mutate(x = kreftform == kf) %>% pull(x)
  tabell[[paste0("kf_", kf)]] <- lag_rad(
    paste0("  ", kf), n_pct(d, n_dig), n_pct(s, n_ste), n_pct(a, n_tot))
}

# --- Dager siden siste behandling ---
dag_dig <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
  pull(dager_siden_behandling)
dag_ste <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
  pull(dager_siden_behandling)
tabell[["dager"]] <- lag_rad("Dager siden siste behandling, gj.snitt (SD)",
  mean_sd(dag_dig), mean_sd(dag_ste),
  mean_sd(bakgrunn_analyse$dager_siden_behandling))

# --- Antropometri (pre) ---
antro_d <- antro_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp") %>%
  filter(treatment == "digital")
antro_s <- antro_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp") %>%
  filter(treatment == "stedlig")

tabell[["vekt"]] <- lag_rad("Kroppsvekt, kg, gj.snitt (SD)",
  mean_sd(antro_d$vekt), mean_sd(antro_s$vekt), mean_sd(antro_pre$vekt))

tabell[["hoyde"]] <- lag_rad("H\u00f8yde, cm, gj.snitt (SD)",
  mean_sd(antro_d$høyde), mean_sd(antro_s$høyde),
  mean_sd(antro_pre$høyde))

tabell[["bmi"]] <- lag_rad("BMI, kg/m², gj.snitt (SD)",
  mean_sd(antro_d$bmi), mean_sd(antro_s$bmi), mean_sd(antro_pre$bmi))

tabell[["midje"]] <- lag_rad("Midjeomkrets, cm, gj.snitt (SD)",
  mean_sd(antro_d$midje), mean_sd(antro_s$midje), mean_sd(antro_pre$midje))

# --- DXA baseline ---
dxa_d <- dxa_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(treatment == "digital")
dxa_s <- dxa_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(treatment == "stedlig")

tabell[["lbm"]] <- lag_rad("Mager masse (LBM), g, gj.snitt (SD)",
  mean_sd(dxa_d$LBM), mean_sd(dxa_s$LBM), mean_sd(dxa_pre$LBM))

tabell[["fett_g"]] <- lag_rad("Total fettmasse, g, gj.snitt (SD)",
  mean_sd(dxa_d$fat_total_g), mean_sd(dxa_s$fat_total_g),
  mean_sd(dxa_pre$fat_total_g))

tabell[["fett_pct"]] <- lag_rad("Total fettprosent, %, gj.snitt (SD)",
  mean_sd(dxa_d$fat_total_pct), mean_sd(dxa_s$fat_total_pct),
  mean_sd(dxa_pre$fat_total_pct))


# =============================================================================
# Skriv ut og lagre
# =============================================================================

library(flextable)
library(officer)

tabell1 <- bind_rows(tabell)

# Lagre rådata som CSV
write.csv(tabell1, "output/tables/tabell1_baseline.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# --- Pen tabell med flextable ---
ft <- flextable(tabell1) %>%

  # Kolonneoverskrifter
  set_header_labels(
    Variabel = "",
    Digital  = paste0("Digital hjemmetrening\n(n=", n_dig, ")"),
    Stedlig  = paste0("Veiledet stedlig\n(n=", n_ste, ")"),
    Totalt   = paste0("Totalt\n(n=", n_tot, ")")
  ) %>%

  # Tittel over tabellen
  add_header_lines("Tabell 1. Baseline-karakteristikker fordelt på gruppe") %>%

  # Grupperingslinje over de tre tallkolonnene
  add_header_row(
    values = c("", "Gruppe", ""),
    colwidths = c(1, 2, 1)
  ) %>%

  # Fet skrift på overskrifter
  bold(part = "header") %>%

  # Rykk inn gruppert-rader (de som starter med mellomrom)
  italic(i = ~ grepl("^  ", Variabel), j = "Variabel") %>%

  # Legg inn horisontale skillelinjer mellom seksjoner
  hline(i = c(4, 6, 15, 16, 21),
        border = fp_border(color = "grey70", width = 0.5)) %>%

  # Kolonnebredder
  width(j = "Variabel", width = 3.5) %>%
  width(j = c("Digital", "Stedlig", "Totalt"), width = 1.8) %>%

  # Midtstill tallkolonner
  align(j = c("Digital", "Stedlig", "Totalt"), align = "center", part = "all") %>%

  # Skriftstørrelse og font
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%

  # Zebra-striper for lesbarhet
  bg(i = seq(2, nrow(tabell1), by = 2), bg = "#f5f5f5") %>%

  set_table_properties(layout = "autofit")

# Vis i RStudio Viewer
print(ft)

# Eksporter til Word
doc <- read_docx() %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft)

print(doc, target = "output/tables/tabell1_baseline.docx")
cat("Lagret: output/tables/tabell1_baseline.docx\n")
