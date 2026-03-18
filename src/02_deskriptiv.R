# =============================================================================
# 02_deskriptiv.R
# Deskriptiv statistikk og Tabell 1 — baseline-karakteristikker per gruppe
# Output: output/tables/tabell1_baseline.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)

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

# --- Kjønn (plassert rett under n) ---
kjonn_dig_k <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)
kjonn_ste_k <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)
kjonn_all_k <- bakgrunn_analyse %>%
  mutate(x = kjønn == "Kvinne") %>% pull(x)

tabell[["kvinner"]] <- lag_rad("Kvinner, n (%)",
  n_pct(kjonn_dig_k, n_dig), n_pct(kjonn_ste_k, n_ste),
  n_pct(kjonn_all_k, n_tot))

# --- Aldersgrupper (WHO) ---
# NB: eksakt alder er ikke tilgjengelig i datasettet, kun aldersgruppe
tabell[["ald_header"]] <- lag_rad("Aldersgruppe (WHO), n (%)", "", "", "")
ald_labels <- c(
  "Unge voksne"   = "  Unge voksne (18\u201344 \u00e5r)",
  "Middelaldrende" = "  Middelaldrende (45\u201359 \u00e5r)",
  "Eldre voksne"  = "  Eldre voksne (60\u201374 \u00e5r)",
  "Gamle eldre"   = "  Gamle eldre (75+ \u00e5r)"
)

for (grp in levels(bakgrunn_analyse$aldersgruppe_WHO)) {
  d <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
    mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  s <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
    mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  a <- bakgrunn_analyse %>% mutate(x = aldersgruppe_WHO == grp) %>% pull(x)
  tabell[[paste0("ald_", grp)]] <- lag_rad(
    ald_labels[grp], n_pct(d, n_dig), n_pct(s, n_ste), n_pct(a, n_tot))
}

# --- Kreftform ---
tabell[["kf_header"]] <- lag_rad("Kreftform, n (%)", "", "", "")

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

# --- Behandlingstype (ikke gjensidig utelukkende) ---
behandling_map <- c(
  "Kreftbehandling.1" = "Cellegift",
  "Kreftbehandling.2" = "Str\u00e5ling",
  "Kreftbehandling.3" = "Immunterapi",
  "Kreftbehandling.4" = "Kirurgi",
  "Kreftbehandling.5" = "Stamcellebehandling",
  "Kreftbehandling.6" = "Legemidler",
  "Kreftbehandling.7" = "Annet"
)

# Les behandlingskolonner fra rådata og koble til analysegruppen
bakgrunn_raw_beh <- read_excel(
  "data/raw/REACT_data_til_studenter.xlsx",
  sheet = "Bakgrunn", skip = 1
) %>%
  mutate(fp = as.integer(fp)) %>%
  filter(fp %in% fp_analyse, !is.na(fp)) %>%
  select(fp, all_of(names(behandling_map)))

beh_data <- bakgrunn_analyse %>%
  select(fp, treatment) %>%
  left_join(bakgrunn_raw_beh, by = "fp")

tabell[["beh_header"]] <- lag_rad("Behandlingstype, n (%)a", "", "", "")

for (kol in names(behandling_map)) {
  navn <- behandling_map[kol]
  d <- beh_data %>% filter(treatment == "digital") %>%
    mutate(x = .data[[kol]] == 1) %>% pull(x)
  s <- beh_data %>% filter(treatment == "stedlig") %>%
    mutate(x = .data[[kol]] == 1) %>% pull(x)
  a <- beh_data %>% mutate(x = .data[[kol]] == 1) %>% pull(x)
  tabell[[paste0("beh_", kol)]] <- lag_rad(
    paste0("  ", navn), n_pct(d, n_dig), n_pct(s, n_ste), n_pct(a, n_tot))
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
# Radrekkefølge etter bind_rows:
# 1:  n
# 2:  Kvinner
# 3:  Aldersgruppe (WHO), n (%) — bold
# 4:    Unge voksne (18–44 år)
# 5:    Middelaldrende (45–59 år)
# 6:    Eldre voksne (60–74 år)
# 7:    Gamle eldre (75+ år)
# 8:  Kreftform, n (%) — bold
# 9:    Blodkreft ... 17: Prostata
# 18: Behandlingstype, n (%)a — bold
# 19:   Cellegift ... 25: Annet
# 26: Dager siden siste behandling
# 27: Kroppsvekt
# 28: Høyde
# 29: BMI
# 30: Midjeomkrets
# 31: Mager masse (LBM)
# 32: Total fettmasse
# 33: Total fettprosent

thin  <- fp_border(color = "grey60", width = 0.5)
thick <- fp_border(color = "black",  width = 1.0)

# Finn radnummer for seksjonslabeler dynamisk
bold_rader <- which(tabell1$Variabel %in% c(
  "Aldersgruppe (WHO), n (%)",
  "Kreftform, n (%)",
  "Behandlingstype, n (%)a"
))

# Finn radnummer for seksjonsgrenser (linjer går ETTER disse radene)
linje_etter <- c(
  which(tabell1$Variabel == "Kvinner, n (%)"),            # etter kvinner
  which(tabell1$Variabel == "  Gamle eldre (75\u00e5r)"), # etter aldersgrupper
  which(grepl("Prostata", tabell1$Variabel)),             # etter kreftform
  which(grepl("Annet", tabell1$Variabel)),                # etter behandlingstype
  which(grepl("Midjeomkrets", tabell1$Variabel))          # etter antropometri
)

ft <- flextable(tabell1) %>%

  set_header_labels(
    Variabel = "",
    Digital  = paste0("Digital hjemmetrening\n(n=", n_dig, ")"),
    Stedlig  = paste0("Veiledet stedlig\n(n=", n_ste, ")"),
    Totalt   = paste0("Totalt\n(n=", n_tot, ")")
  ) %>%

  add_header_lines("Tabell 1. Baseline-karakteristikker fordelt p\u00e5 gruppe") %>%

  hline_top(border = thick, part = "header") %>%
  hline_bottom(border = thin,  part = "header") %>%
  hline_bottom(border = thick, part = "body") %>%
  hline(i = linje_etter, border = thin, part = "body") %>%

  bold(i = bold_rader, part = "body") %>%
  bold(part = "header") %>%

  italic(i = ~ grepl("^  ", Variabel), j = "Variabel") %>%

  # Kolonnebredder tilpasset én side (A4 med smale marger)
  width(j = "Variabel", width = 3.2) %>%
  width(j = c("Digital", "Stedlig", "Totalt"), width = 1.6) %>%

  align(j = c("Digital", "Stedlig", "Totalt"), align = "center", part = "all") %>%
  align(j = "Variabel", align = "left", part = "all") %>%

  # Reduser cellehøyde for å spare plass
  padding(padding.top = 1, padding.bottom = 1, part = "body") %>%
  padding(padding.top = 3, padding.bottom = 3, part = "header") %>%

  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bg(bg = "white", part = "all") %>%

  set_table_properties(layout = "fixed") %>%

  add_footer_lines(paste0(
    "a Kategoriene er ikke gjensidig utelukkende; en deltaker kan ha mottatt ",
    "flere behandlingstyper. ",
    "SD = standardavvik; LBM = lean body mass (mager kroppsmasse). ",
    "For n = 6 deltakere der kroppen oversteg m\u00e5leomr\u00e5det til DXA-maskinen, ",
    "ble offset-scanning benyttet med programvareestimert venstreside, ",
    "i tr\u00e5d med International Society for Clinical Densitometry (ISCD, 2023)."
  )) %>%
  italic(part = "footer") %>%
  fontsize(size = 8, part = "footer") %>%
  font(fontname = "Times New Roman", part = "footer")

# Vis i RStudio Viewer
print(ft)

# Eksporter til Word med smale marger for å få tabellen på én side
seksjon <- prop_section(
  page_size    = page_size(width = 21 / 2.54, height = 29.7 / 2.54),
  page_margins = page_mar(top = 1.5 / 2.54, bottom = 1.5 / 2.54,
                          left = 2 / 2.54,   right = 2 / 2.54)
)

doc <- read_docx(
  system.file("template/template.docx", package = "officer")
) %>%
  body_set_default_section(seksjon) %>%
  body_add_flextable(ft)

print(doc, target = "output/tables/tabell1_baseline.docx")
cat("Lagret: output/tables/tabell1_baseline.docx\n")
