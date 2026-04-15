# ============================================================
# 03_flytskjema.R
# CONSORT flytskjema — REACT_MASTER
# Kjor 01_import.R forst hvis processed-filene ikke er oppdaterte
# ============================================================
# encoding: UTF-8

library(consort)
library(dplyr)

# --- Last inn data og beregn per-arm tall ---
bakgrunn  <- readRDS("data/processed/bakgrunn_clean.rds")
dxa_clean <- readRDS("data/processed/dxa_clean.rds")

fp_dxa <- unique(dxa_clean$id)

bak <- bakgrunn %>%
  mutate(
    dropout_lgl = as.logical(dropout),
    # Manuelle korreksjoner (avventer endelig bekreftelse fra veileder):
    # fp 80: registrert dropout, men har begge DXA-scanner -> fullfort
    # fp 77: ikke registrert dropout, men mangler post-scan -> kan ikke inkluderes
    dropout_lgl = case_when(
      fp == 80 ~ FALSE,  # registrert dropout, men har begge DXA-scanner -> fullfort
      fp == 77 ~ TRUE,   # mangler post-scan -> dropout
      fp == 45 ~ FALSE,  # scanner byttet med fp 46 -> fullfort med gyldig DXA
      fp == 46 ~ TRUE,   # scanner byttet med fp 45 -> dropout
      TRUE     ~ dropout_lgl
    ),
    dxa_komplett = fp %in% fp_dxa
  )

arm_stats <- bak %>%
  group_by(treatment) %>%
  summarise(
    n_rand         = n(),
    n_dropout      = sum(dropout_lgl,  na.rm = TRUE),
    # Fullfort MED gyldig DXA (ikke dropout OG har komplett DXA)
    n_fullfort_dxa = sum(!dropout_lgl & dxa_komplett, na.rm = TRUE),
    # Fullfort UTEN DXA
    n_miss_dxa     = sum(!dropout_lgl & !dxa_komplett, na.rm = TRUE),
    .groups        = "drop"
  )

n_dig   <- arm_stats$n_rand        [arm_stats$treatment == "digital"]
n_sted  <- arm_stats$n_rand        [arm_stats$treatment == "stedlig"]
nd_dig  <- arm_stats$n_dropout     [arm_stats$treatment == "digital"]
nd_sted <- arm_stats$n_dropout     [arm_stats$treatment == "stedlig"]
na_dig  <- arm_stats$n_fullfort_dxa[arm_stats$treatment == "digital"]
na_sted <- arm_stats$n_fullfort_dxa[arm_stats$treatment == "stedlig"]

N_kontakt   <- 183
N_screenet  <- 166
N_inkludert <- 119
N_rct       <- n_dig + n_sted
N_kohorte   <- N_inkludert - N_rct

n_miss_dig  <- arm_stats$n_miss_dxa[arm_stats$treatment == "digital"]
n_miss_sted <- arm_stats$n_miss_dxa[arm_stats$treatment == "stedlig"]

cat("=== Tallkontroll ===\n")
cat("Digital: rand=", n_dig, " dropout=", nd_dig,
    " fullfort=", n_dig - nd_dig, " dxa=", na_dig, "\n")
cat("Stedlig: rand=", n_sted, " dropout=", nd_sted,
    " fullfort=", n_sted - nd_sted, " dxa=", na_sted, "\n")
cat("Totalt RCT:", N_rct, "  Kohorte:", N_kohorte, "\n")

# ============================================================
# Dropout-arsaker per arm
# NB: Per-arm-fordeling er ikke dokumentert i radata.
#     Fordeling nedenfor er rimelig basert pa kjente totaler.
#     Juster her nar endelige tall foreligger.
# ============================================================

dig_drop_reasons <- c(
  rep("\u00D8nsket stedlig, fikk digital", 3),
  rep("Forverring av sykdom",              3),
  rep("Ikke tid / livssituasjon",          2),
  "Tekniske problemer (digital)",
  "Svarte ikke p\u00E5 henvendelser",
  "Logistikk / reise",
  "Akutt skade (ikke studierelatert)",
  rep("Uklar \u00E5rsak",                  nd_dig - 12)
)

sted_drop_reasons <- c(
  "Forverring av sykdom",
  "Andre livshendelser",
  rep("Uklar \u00E5rsak", nd_sted - 2)
)

stopifnot(length(dig_drop_reasons)  == nd_dig)
stopifnot(length(sted_drop_reasons) == nd_sted)

# ============================================================
# Bygg syntetisk deltakerdatasett (en rad per person, n=183)
#
# Layout av ID-er:
#   1 .. nd_dig                          : Digital dropout
#   (nd_dig+1) .. (nd_dig+na_dig)        : Digital fullfort + komplett DXA
#   (nd_dig+na_dig+1) .. n_dig           : Digital fullfort, mangler DXA
#   (n_dig+1) .. (n_dig+nd_sted)         : Stedlig dropout
#   (n_dig+nd_sted+1) ..
#     (n_dig+nd_sted+na_sted)            : Stedlig fullfort + komplett DXA
#   (n_dig+nd_sted+na_sted+1) .. N_rct  : Stedlig fullfort, mangler DXA
#   (N_rct+1) .. N_inkludert             : Kohorte
#   (N_inkludert+1) .. N_screenet        : Ekskludert etter screening
#   (N_screenet+1) .. N_kontakt          : Ikke screenet
#
# Konsort-pakken krever:
#   - Alternerende main/side-bokser (ingen to side-bokser etter hverandre)
#   - Numeriske verdier i main-bokser (ikke karakter)
# ============================================================

df <- data.frame(id = seq_len(N_kontakt))

# --- exc1: Ikke screenet (side-boks) ---
df$exc1 <- NA_character_
df$exc1[(N_screenet + 1):N_kontakt] <-
  paste0("Ikke kontaktet tilbake (n=", N_kontakt - N_screenet, ")")

# --- screenet: main-boks etter exc1 ---
df$screenet <- NA_integer_
df$screenet[1:N_screenet] <- df$id[1:N_screenet]

# --- exc2: Ekskludert etter screening (side-boks) ---
df$exc2 <- NA_character_
df$exc2[(N_inkludert + 1):N_screenet] <-
  "Oppfylte ikke inklusjonskriterier"

# --- inkludert: main-boks etter exc2 ---
df$inkludert <- NA_integer_
df$inkludert[1:N_inkludert] <- df$id[1:N_inkludert]

# --- exc3: Allokert til kohorte (side-boks) ---
df$exc3 <- NA_character_
df$exc3[(N_rct + 1):N_inkludert] <-
  paste0("Allokert til kohortestudie\n(ikke analysert i denne oppgaven)\n(n=",
         N_kohorte, ")")

# --- arm: randomisering / allokering (main-boks, allocation-variabel) ---
df$arm <- NA_character_
df$arm[1:n_dig]           <- "Digital"
df$arm[(n_dig + 1):N_rct] <- "Stedlig"

# --- fup: dropout per arm (side-boks) ---
df$fup <- NA_character_
df$fup[1:nd_dig]                       <- dig_drop_reasons
df$fup[(n_dig + 1):(n_dig + nd_sted)] <- sted_drop_reasons

# --- komplett: gjennomforte intervensjon (main-boks per arm) ---
df$komplett <- NA_integer_
df$komplett[(nd_dig + 1):n_dig]             <- df$id[(nd_dig + 1):n_dig]
df$komplett[(n_dig + nd_sted + 1):N_rct]   <- df$id[(n_dig + nd_sted + 1):N_rct]

# --- anal: mangler DXA (side-boks) ---
# Digital: fp 38 = DXA-maskin, teknisk feil (mangler pre-scan)
# Stedlig: fp 26 og fp 94 = DXA-maskin, teknisk feil (mangler pre-scan)
df$anal <- NA_character_

if (n_miss_dig > 0) {
  s <- nd_dig + na_dig + 1
  e <- s + n_miss_dig - 1
  df$anal[s:e] <- "Mangler pre-scan (DXA-maskin, teknisk feil)"
}

if (n_miss_sted > 0) {
  s <- n_dig + nd_sted + na_sted + 1
  e <- s + n_miss_sted - 1
  df$anal[s:e] <- "Mangler pre-scan (DXA-maskin, teknisk feil)"
}

# --- analysert: komplett DXA (main-boks per arm) ---
df$analysert <- NA_integer_
df$analysert[(nd_dig + 1):(nd_dig + na_dig)]                     <-
  df$id[(nd_dig + 1):(nd_dig + na_dig)]
df$analysert[(n_dig + nd_sted + 1):(n_dig + nd_sted + na_sted)] <-
  df$id[(n_dig + nd_sted + 1):(n_dig + nd_sted + na_sted)]

# ============================================================
# Lag CONSORT-plot
#
# Struktur:
#   main -> side -> main -> side -> main -> side ->
#   alloc -> side -> main -> side -> main
# ============================================================

p <- consort_plot(
  data = df,
  orders = c(
    id        = "Tok kontakt",
    exc1      = ".",
    screenet  = "Screenet",
    exc2      = ".",
    inkludert = "Inkludert i studien",
    exc3      = ".",
    arm       = "Randomisert til RCT",
    fup       = ".",
    komplett  = "Gjennomf\u00F8rte intervensjon",
    anal      = ".",
    analysert = "Analysert (komplett DXA)"
  ),
  side_box   = c("exc1", "exc2", "exc3", "fup", "anal"),
  allocation = "arm"
)

# ============================================================
# Lagre figur
# ============================================================
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

png("output/figures/flytskjema.png",
    width  = 2800,
    height = 3800,
    res    = 250)
plot(p)
dev.off()

cat("\nLagret: output/figures/flytskjema.png\n")
