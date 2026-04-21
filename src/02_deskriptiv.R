# =============================================================================
# 02_deskriptiv.R
# Deskriptiv statistikk og Tabell 1 -- baseline-karakteristikker per gruppe
# Output: output/tables/tabell4_baseline.csv
#         output/tables/tabell4_baseline.docx
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(flextable)
library(officer)

# =============================================================================
# Les inn data
# =============================================================================

bakgrunn     <- readRDS("data/processed/bakgrunn_clean.rds")
antropometri <- readRDS("data/processed/antropometri_clean.rds")
dxa          <- readRDS("data/processed/dxa_clean.rds")

fp_analyse       <- unique(dxa$id)
bakgrunn_analyse <- bakgrunn %>% filter(fp %in% fp_analyse)
antro_pre        <- antropometri %>% filter(fp %in% fp_analyse, test == "pre")
dxa_pre          <- dxa %>% filter(id %in% fp_analyse, time == "pre")

cat("Deltakere i analysen:", length(fp_analyse), "\n")
cat("Gruppe digital:", sum(bakgrunn_analyse$treatment == "digital"), "\n")
cat("Gruppe stedlig:", sum(bakgrunn_analyse$treatment == "stedlig"), "\n\n")


# =============================================================================
# Hjelpefunksjoner
# =============================================================================

mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

# For gram-verdier: ingen desimaler (DXA-presisjon tilsier ikke mer)
mean_sd_g <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf("%.0f (%.0f)", mean(x), sd(x))
}

n_pct <- function(x, total) {
  n <- sum(!is.na(x) & x)
  sprintf("%d (%.0f%%)", n, 100 * n / total)
}

lag_rad <- function(variabelnavn, digital_verdi, stedlig_verdi, total_verdi, p_verdi = "") {
  data.frame(
    Variabel = variabelnavn,
    Digital  = digital_verdi,
    Stedlig  = stedlig_verdi,
    Totalt   = total_verdi,
    p        = p_verdi,
    stringsAsFactors = FALSE
  )
}

# Formater p-verdi: < 0.001 eller 2 desimaler
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

# Wilcoxon rank-sum for kontinuerlig variabel (to grupper)
p_wilcox <- function(x, gruppe) {
  x_d <- x[gruppe == "digital"]
  x_s <- x[gruppe == "stedlig"]
  x_d <- x_d[!is.na(x_d)]
  x_s <- x_s[!is.na(x_s)]
  if (length(x_d) < 2 || length(x_s) < 2) return(NA)
  fmt_p(wilcox.test(x_d, x_s, exact = FALSE)$p.value)
}

# Fisher's exact test for kategorisk variabel (ja/nei per gruppe)
p_fisher_bin <- function(x_bool, gruppe) {
  tbl <- table(gruppe, x_bool)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return("")
  fmt_p(fisher.test(tbl)$p.value)
}

# Fisher's exact test for aldersgruppe eller kreftform (flerverdi)
p_fisher_cat <- function(variabel, gruppe) {
  tbl <- table(gruppe, variabel)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return("")
  tryCatch(fmt_p(fisher.test(tbl, simulate.p.value = TRUE, B = 10000)$p.value),
           error = function(e) "")
}


# =============================================================================
# Bygg Tabell 1
# Radrekkefolge:
#  1: n
#  2: Kvinner, n (%)
#  3: Aldersgruppe (WHO), n (%)         [bold, tom]
#  4-7:   Unge voksne ... Gamle eldre
#  8: Kreftform, n (%)                  [bold, tom]
#  9-17:  Blodkreft ... Prostata
# 18: Behandlingstype, n (%)1           [bold, tom]
# 19-25:  Cellegift ... Annet
# 26: Dager siden siste behandling
# 27: Antropometri (pre), gj.snitt (SD) [bold, tom]
# 28-31:  Kroppsvekt ... Midjeomkrets
# 32: DXA-malinger (pre), gj.snitt (SD) [bold, tom]
# 33-35:  LBM ... Fettprosent
# =============================================================================

tabell <- list()

n_dig <- sum(bakgrunn_analyse$treatment == "digital")
n_ste <- sum(bakgrunn_analyse$treatment == "stedlig")
n_tot <- nrow(bakgrunn_analyse)

# --- Antall ---
tabell[["n"]] <- lag_rad("n",
  as.character(n_dig), as.character(n_ste), as.character(n_tot))

# --- Kjonn ---
kjonn_dig_k <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
  mutate(x = kjonn == "Kvinne") %>% pull(x)
kjonn_ste_k <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
  mutate(x = kjonn == "Kvinne") %>% pull(x)
kjonn_all_k <- bakgrunn_analyse %>%
  mutate(x = kjonn == "Kvinne") %>% pull(x)
tabell[["kvinner"]] <- lag_rad("Kvinner, n (%)",
  n_pct(kjonn_dig_k, n_dig), n_pct(kjonn_ste_k, n_ste),
  n_pct(kjonn_all_k, n_tot),
  p_fisher_bin(kjonn_all_k, bakgrunn_analyse$treatment))

# --- Aldersgrupper (WHO) ---
tabell[["ald_header"]] <- lag_rad("Aldersgruppe (WHO), n (%)", "", "", "",
  p_fisher_cat(bakgrunn_analyse$aldersgruppe_WHO, bakgrunn_analyse$treatment))
ald_labels <- c(
  "Unge voksne"    = "  Unge voksne (18\u201344 \u00e5r)",
  "Middelaldrende" = "  Middelaldrende (45\u201359 \u00e5r)",
  "Eldre voksne"   = "  Eldre voksne (60\u201374 \u00e5r)",
  "Gamle eldre"    = "  Gamle eldre (75+ \u00e5r)"
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
tabell[["kf_header"]] <- lag_rad("Kreftform, n (%)", "", "", "",
  p_fisher_cat(bakgrunn_analyse$kreftform, bakgrunn_analyse$treatment))
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
# \u00b9 = superscript 1
tabell[["beh_header"]] <- lag_rad(
  "Behandlingstype, n (%)\u00b9", "", "", "")

behandling_map <- c(
  "Kreftbehandling.1" = "Cellegift",
  "Kreftbehandling.2" = "Str\u00e5ling",
  "Kreftbehandling.3" = "Immunterapi",
  "Kreftbehandling.4" = "Kirurgi",
  "Kreftbehandling.5" = "Stamcellebehandling",
  "Kreftbehandling.6" = "Legemidler",
  "Kreftbehandling.7" = "Annet"
)
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

# --- Dager siden siste behandling (plassert etter behandlingstype) ---
dag_dig <- bakgrunn_analyse %>% filter(treatment == "digital") %>%
  pull(dager_siden_behandling)
dag_ste <- bakgrunn_analyse %>% filter(treatment == "stedlig") %>%
  pull(dager_siden_behandling)
tabell[["dager"]] <- lag_rad(
  "Dager siden siste behandling, gj.snitt (SD)",
  mean_sd(dag_dig), mean_sd(dag_ste),
  mean_sd(bakgrunn_analyse$dager_siden_behandling),
  p_wilcox(bakgrunn_analyse$dager_siden_behandling, bakgrunn_analyse$treatment))

# --- Antropometri (pre) ---
antro_d <- antro_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp") %>%
  filter(treatment == "digital")
antro_s <- antro_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp") %>%
  filter(treatment == "stedlig")

tabell[["antro_header"]] <- lag_rad(
  "Antropometri (pre), gj.snitt (SD)", "", "", "")
antro_grp <- antro_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp")

tabell[["vekt"]]  <- lag_rad("  Kroppsvekt, kg",
  mean_sd(antro_d$vekt),  mean_sd(antro_s$vekt),  mean_sd(antro_pre$vekt),
  p_wilcox(antro_grp$vekt,  antro_grp$treatment))
tabell[["hoyde"]] <- lag_rad("  H\u00f8yde, cm",
  mean_sd(antro_d$hoyde), mean_sd(antro_s$hoyde), mean_sd(antro_pre$hoyde),
  p_wilcox(antro_grp$hoyde, antro_grp$treatment))
tabell[["bmi"]]   <- lag_rad("  BMI, kg/m\u00b2",
  mean_sd(antro_d$bmi),   mean_sd(antro_s$bmi),   mean_sd(antro_pre$bmi),
  p_wilcox(antro_grp$bmi,   antro_grp$treatment))
tabell[["midje"]] <- lag_rad("  Midjeomkrets, cm",
  mean_sd(antro_d$midje), mean_sd(antro_s$midje), mean_sd(antro_pre$midje),
  p_wilcox(antro_grp$midje, antro_grp$treatment))

# --- DXA-malinger (pre) ---
dxa_d <- dxa_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(treatment == "digital")
dxa_s <- dxa_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(treatment == "stedlig")

tabell[["dxa_header"]] <- lag_rad(
  "DXA-m\u00e5linger (pre), gj.snitt (SD)", "", "", "")
dxa_grp <- dxa_pre %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = c("id" = "fp"))

tabell[["lbm"]]      <- lag_rad("  Mager masse (LBM), g",
  mean_sd_g(dxa_d$LBM),          mean_sd_g(dxa_s$LBM),          mean_sd_g(dxa_pre$LBM),
  p_wilcox(dxa_grp$LBM,          dxa_grp$treatment))
tabell[["fett_g"]]   <- lag_rad("  Total fettmasse, g",
  mean_sd_g(dxa_d$fat_total_g),   mean_sd_g(dxa_s$fat_total_g),  mean_sd_g(dxa_pre$fat_total_g),
  p_wilcox(dxa_grp$fat_total_g,   dxa_grp$treatment))
tabell[["fett_pct"]] <- lag_rad("  Total fettprosent, %",
  mean_sd(dxa_d$fat_total_pct), mean_sd(dxa_s$fat_total_pct), mean_sd(dxa_pre$fat_total_pct),
  p_wilcox(dxa_grp$fat_total_pct, dxa_grp$treatment))


# =============================================================================
# Bygg flextable og eksporter
# =============================================================================

tabell1 <- bind_rows(tabell)

write.csv(tabell1, "output/tables/tabell4_baseline.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

thin  <- fp_border(color = "grey60", width = 0.5)
thick <- fp_border(color = "black",  width = 1.0)

# Finn radnummer dynamisk
bold_rader <- which(tabell1$Variabel %in% c(
  "Aldersgruppe (WHO), n (%)",
  "Kreftform, n (%)",
  "Behandlingstype, n (%)\u00b9",
  "Antropometri (pre), gj.snitt (SD)",
  "DXA-m\u00e5linger (pre), gj.snitt (SD)"
))

linje_etter <- c(
  which(tabell1$Variabel == "Kvinner, n (%)"),
  which(tabell1$Variabel == paste0("  Gamle eldre (75+ \u00e5r)")),
  which(grepl("Prostata", tabell1$Variabel)),
  which(tabell1$Variabel == "Dager siden siste behandling, gj.snitt (SD)"),
  which(grepl("Midjeomkrets", tabell1$Variabel))
)

note_text <- paste0(
  "\u00b9 Kategoriene er ikke gjensidig utelukkende; en deltaker kan ha mottatt ",
  "flere behandlingstyper. ",
  "SD = standardavvik; LBM = lean body mass (mager kroppsmasse). ",
  "For n = 14 deltakere der kroppen oversteg m\u00e5leomr\u00e5det til DXA-maskinen, ",
  "ble offset-scanning benyttet med programvareestimert venstreside, i tr\u00e5d med ",
  "International Society for Clinical Densitometry (ISCD, 2023). ",
  "P-verdier for kategoriske variabler er beregnet med Fisher\u2019s eksakte test; ",
  "for kontinuerlige variabler er Wilcoxon rank-sum test benyttet."
)

ft <- flextable(tabell1) %>%
  set_header_labels(
    Variabel = "",
    Digital  = paste0("Digital\n(n=", n_dig, ")"),
    Stedlig  = paste0("Stedlig\n(n=", n_ste, ")"),
    Totalt   = paste0("Totalt\n(n=", n_tot, ")"),
    p        = "p-verdi"
  ) %>%
  border_remove() %>%
  hline_top(border = thick, part = "header") %>%
  hline_bottom(border = thin,  part = "header") %>%
  hline_bottom(border = thick, part = "body") %>%
  hline(i = linje_etter, border = thin, part = "body") %>%
  bold(i = bold_rader, part = "body") %>%
  bold(part = "header") %>%
  italic(i = ~ grepl("^  ", Variabel), j = "Variabel") %>%
  width(j = "Variabel", width = 2.9) %>%
  width(j = c("Digital", "Stedlig", "Totalt"), width = 0.9) %>%
  width(j = "p", width = 0.7) %>%
  align(j = c("Digital", "Stedlig", "Totalt", "p"), align = "center", part = "all") %>%
  align(j = "Variabel", align = "left", part = "all") %>%
  padding(padding.top = 2, padding.bottom = 2, part = "body") %>%
  padding(i = nrow(tabell1), padding.bottom = 10, part = "body") %>%
  padding(padding.top = 4, padding.bottom = 4, part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bg(bg = "white", part = "all") %>%
  set_table_properties(layout = "fixed")

print(ft)

title_style <- fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman")
note_style  <- fp_text(font.size = 8,  italic = TRUE, font.family = "Times New Roman")

doc <- read_docx() %>%
  body_add_fpar(fpar(ftext(
    "Tabell 4. Baseline-karakteristikker fordelt p\u00e5 gruppe",
    title_style
  ))) %>%
  body_add_flextable(ft) %>%
  body_add_fpar(fpar(ftext(note_text, note_style)))

print(doc, target = "output/tables/tabell4_baseline.docx")
cat("Lagret: output/tables/tabell4_baseline.docx\n")


# =============================================================================
# Tabell 2: Etterlevelse
# =============================================================================

opm_raw <- read_excel(
  "data/raw/REACT_data_til_studenter.xlsx",
  sheet = "Oppm\u00f8te", skip = 1
)

opm <- opm_raw %>%
  mutate(fp = as.integer(suppressWarnings(as.numeric(gsub("^FP", "", fp))))) %>%
  filter(!is.na(fp), fp %in% fp_analyse) %>%
  select(fp,
         gruppe  = `group    week`,
         n_yes   = `...28`,
         n_no    = `...29`,
         pct_str = `...30`) %>%
  mutate(
    n_yes = as.integer(suppressWarnings(as.numeric(n_yes))),
    pct   = suppressWarnings(as.numeric(pct_str))
  ) %>%
  left_join(bakgrunn_analyse %>% select(fp, treatment), by = "fp") %>%
  # Behold kun raden som matcher behandlingsgruppe (FP35 har to rader)
  filter(
    (gruppe == "rand-digital" & treatment == "digital") |
    (gruppe %in% c("rand-onsite", "rand-stedlig") & treatment == "stedlig") |
    is.na(gruppe)
  ) %>%
  distinct(fp, .keep_all = TRUE)

# FP 45 og FP 46 hadde byttet DXA-scans: gi FP 45 oppmote-verdien fra FP 46
fp46_pct <- as.numeric(opm_raw %>%
  mutate(fp_raw = as.integer(suppressWarnings(as.numeric(gsub("^FP", "", fp))))) %>%
  filter(fp_raw == 46) %>%
  pull(`...30`) %>% .[1])

opm <- opm %>%
  mutate(pct = case_when(
    fp == 45 ~ fp46_pct,   # bruk FP 46 sitt oppmote
    fp == 80 ~ NA_real_,   # ekskluder FP 80 (0 registrerte okter)
    TRUE     ~ pct
  ))

cat("\nEtterlevelse n:", nrow(opm), " NA pct:", sum(is.na(opm$pct)), "\n")

mean_sd_pct <- function(x) {
  x <- x[!is.na(x)]
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}
med_iqr_pct <- function(x) {
  x <- x[!is.na(x)]
  sprintf("%.1f\n(%.1f\u2013%.1f)",
          median(x), quantile(x, 0.25), quantile(x, 0.75))
}

opm_d <- opm %>% filter(treatment == "digital") %>% pull(pct)
opm_s <- opm %>% filter(treatment == "stedlig")  %>% pull(pct)
opm_a <- opm$pct

n_d_opm <- sum(!is.na(opm_d))
n_s_opm <- sum(!is.na(opm_s))
n_a_opm <- sum(!is.na(opm_a))

p_etterlevelse <- p_wilcox(opm_a, opm$treatment)

etterlevelse_tabell <- bind_rows(
  lag_rad(
    paste0("Gj.snitt (SD), %"),
    mean_sd_pct(opm_d), mean_sd_pct(opm_s), mean_sd_pct(opm_a),
    p_etterlevelse
  ),
  lag_rad(
    "Median (25.\u201375. persentil), %",
    med_iqr_pct(opm_d), med_iqr_pct(opm_s), med_iqr_pct(opm_a),
    ""
  )
)

ft2 <- flextable(etterlevelse_tabell) %>%
  set_header_labels(
    Variabel = "",
    Digital  = paste0("Digital\n(n=", n_d_opm, ")"),
    Stedlig  = paste0("Stedlig\n(n=", n_s_opm, ")"),
    Totalt   = paste0("Totalt\n(n=", n_a_opm, ")"),
    p        = "p-verdi"
  ) %>%
  border_remove() %>%
  hline_top(border = thick, part = "header") %>%
  hline_bottom(border = thin,  part = "header") %>%
  hline_bottom(border = thick, part = "body") %>%
  bold(part = "header") %>%
  width(j = "Variabel", width = 2.9) %>%
  width(j = c("Digital", "Stedlig", "Totalt"), width = 0.9) %>%
  width(j = "p", width = 0.7) %>%
  align(j = c("Digital", "Stedlig", "Totalt", "p"), align = "center", part = "all") %>%
  align(j = "Variabel", align = "left", part = "all") %>%
  padding(padding.top = 3, padding.bottom = 3, part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bg(bg = "white", part = "all") %>%
  set_table_properties(layout = "fixed")

note2_text <- paste0(
  "Etterlevelse er beregnet som andel gjennomf\u00f8rte \u00f8kter av totalt 24 planlagte \u00f8kter ",
  "(2 \u00f8kter per uke \u00d7 12 uker). ",
  "P-verdi er beregnet med Wilcoxon rank-sum test. ",
  "2 deltakere er ekskludert fra etterlevelsesanalysen grunnet manglende eller ugyldig oppm\u00f8teregistrering (n = 48)."
)

doc2 <- read_docx() %>%
  body_add_fpar(fpar(ftext(
    "Tabell 5. Etterlevelse",
    title_style
  ))) %>%
  body_add_flextable(ft2) %>%
  body_add_fpar(fpar(ftext(note2_text, note_style)))

print(doc2, target = "output/tables/tabell5_etterlevelse.docx")
cat("Lagret: output/tables/tabell5_etterlevelse.docx\n")
