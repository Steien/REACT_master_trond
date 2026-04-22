# =============================================================================
# 04_analyse.R
# TOST-ekvivalenstest for primært og sekundært utfallsmål
# Primært:   Mager masse (LBM, gram)
# Sekundært: Total fettprosent (%)
#
# Metode: ANCOVA med post som utfall, pre som kovariat, gruppe som prediktor.
# TOST via hypotheses() i marginaleffects (Arel-Bundock et al., 2024).
# Ekvivalensgrenser fra Brown et al. (2019): ±380 g LBM, ±0.35 % fettprosent.
# 90 % KI benyttes (tilsvarer alpha = 0.05 for TOST).
# =============================================================================

library(dplyr)
library(marginaleffects)

# --- Last inn data ---
dxa      <- readRDS("data/processed/dxa_clean.rds")
bakgrunn <- readRDS("data/processed/bakgrunn_clean.rds")

# --- Lag bredt datasett (en rad per deltaker) ---
dxa_wide <- dxa %>%
  select(id, time, LBM, fat_total_pct) %>%
  tidyr::pivot_wider(
    names_from  = time,
    values_from = c(LBM, fat_total_pct)
  ) %>%
  left_join(bakgrunn %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(!is.na(treatment))

cat("Analyseutvalg:", nrow(dxa_wide), "deltakere\n")
cat("Digital:", sum(dxa_wide$treatment == "digital"), "\n")
cat("Stedlig:", sum(dxa_wide$treatment == "stedlig"), "\n\n")

# =============================================================================
# Ekvivalensgrenser (Brown et al., 2019 — 50 % av nedre KI-grense)
# =============================================================================
eq_lbm  <- 380    # gram
eq_fat  <- 0.35   # prosentpoeng

# =============================================================================
# Primært utfallsmål: Mager masse (LBM)
# =============================================================================
cat("=== PRIMÆRT UTFALLSMÅL: Mager masse (LBM, g) ===\n")

mod_lbm <- lm(LBM_post ~ treatment + LBM_pre, data = dxa_wide)
cat("\nANCOVA-koeffisienter:\n")
print(summary(mod_lbm)$coefficients)

tost_lbm <- hypotheses(
  mod_lbm,
  hypothesis = "treatmentstedlig = 0",
  equivalence = c(-eq_lbm, eq_lbm),
  conf_level  = 0.90
)
cat("\nTOST LBM (ekvivalensgrense ±", eq_lbm, "g):\n")
print(tost_lbm)

# =============================================================================
# Sekundært utfallsmål: Total fettprosent
# =============================================================================
cat("\n=== SEKUNDÆRT UTFALLSMÅL: Total fettprosent (%) ===\n")

mod_fat <- lm(fat_total_pct_post ~ treatment + fat_total_pct_pre, data = dxa_wide)
cat("\nANCOVA-koeffisienter:\n")
print(summary(mod_fat)$coefficients)

tost_fat <- hypotheses(
  mod_fat,
  hypothesis  = "treatmentstedlig = 0",
  equivalence = c(-eq_fat, eq_fat),
  conf_level  = 0.90
)
cat("\nTOST fettprosent (ekvivalensgrense ±", eq_fat, "%):\n")
print(tost_fat)
