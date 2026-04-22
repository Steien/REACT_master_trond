# =============================================================================
# 05_figurer.R
# Figurer for primært og sekundært utfallsmål
# - Boksplot: endring i LBM og fettprosent per gruppe
# - Ekvivalensplot: 90 % KI mot ekvivalensgrenser
# =============================================================================

library(dplyr)
library(ggplot2)

dxa      <- readRDS("data/processed/dxa_clean.rds")
bakgrunn <- readRDS("data/processed/bakgrunn_clean.rds")

dxa_wide <- dxa %>%
  select(id, time, LBM, fat_total_pct) %>%
  tidyr::pivot_wider(names_from = time, values_from = c(LBM, fat_total_pct)) %>%
  mutate(
    endring_lbm = LBM_post - LBM_pre,
    endring_fat = fat_total_pct_post - fat_total_pct_pre
  ) %>%
  left_join(bakgrunn %>% select(fp, treatment), by = c("id" = "fp")) %>%
  filter(!is.na(treatment)) %>%
  mutate(treatment = factor(treatment,
                            levels = c("digital", "stedlig"),
                            labels = c("Digital", "Stedlig")))

tema <- theme_classic(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    legend.position  = "none",
    plot.title       = element_text(size = 11, face = "bold"),
    plot.subtitle    = element_text(size = 9)
  )

farger <- c("Digital" = "#4472C4", "Stedlig" = "#ED7D31")

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Boksplot: endring LBM
# =============================================================================
p_boks_lbm <- ggplot(dxa_wide, aes(x = treatment, y = endring_lbm, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = farger) +
  labs(
    title    = "Figur 1. Endring i mager masse (LBM)",
    subtitle = "Pre til post per gruppe",
    x        = NULL,
    y        = "Endring i LBM (g)"
  ) +
  tema

# =============================================================================
# Boksplot: endring fettprosent
# =============================================================================
p_boks_fat <- ggplot(dxa_wide, aes(x = treatment, y = endring_fat, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = farger) +
  labs(
    title    = "Figur 3. Endring i total fettprosent",
    subtitle = "Pre til post per gruppe",
    x        = NULL,
    y        = "Endring i fettprosent (prosentpoeng)"
  ) +
  tema

# =============================================================================
# Ekvivalensplot: 90 % KI mot ekvivalensgrenser
# =============================================================================
ekv_data <- data.frame(
  utfall   = c("Mager masse (LBM)", "Fettprosent"),
  estimat  = c(537, -0.064),
  ki_lav   = c(-26.7, -0.841),
  ki_hoy   = c(1100, 0.712),
  eq_lav   = c(-380, -0.35),
  eq_hoy   = c(380, 0.35),
  enhet    = c("gram", "prosentpoeng")
)

# LBM ekvivalensplot
p_ekv_lbm <- ggplot(ekv_data[1, ], aes(y = utfall)) +
  geom_rect(aes(xmin = eq_lav, xmax = eq_hoy, ymin = -Inf, ymax = Inf),
            fill = "#d4edda", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = ki_lav, xmax = ki_hoy), height = 0.15, size = 1) +
  geom_point(aes(x = estimat), size = 3) +
  geom_vline(aes(xintercept = eq_lav), linetype = "dotted", color = "#28a745") +
  geom_vline(aes(xintercept = eq_hoy), linetype = "dotted", color = "#28a745") +
  labs(
    title    = "Figur 2. Ekvivalensplot — Mager masse (LBM)",
    subtitle = "90 % KI mot ekvivalensgrenser (±380 g, grønt område)",
    x        = "Estimert gruppeforskjell (g)",
    y        = NULL
  ) +
  tema +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Fettprosent ekvivalensplot
p_ekv_fat <- ggplot(ekv_data[2, ], aes(y = utfall)) +
  geom_rect(aes(xmin = eq_lav, xmax = eq_hoy, ymin = -Inf, ymax = Inf),
            fill = "#d4edda", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = ki_lav, xmax = ki_hoy), height = 0.15, size = 1) +
  geom_point(aes(x = estimat), size = 3) +
  geom_vline(aes(xintercept = eq_lav), linetype = "dotted", color = "#28a745") +
  geom_vline(aes(xintercept = eq_hoy), linetype = "dotted", color = "#28a745") +
  labs(
    title    = "Figur 4. Ekvivalensplot — Fettprosent",
    subtitle = "90 % KI mot ekvivalensgrenser (±0.35 %, grønt område)",
    x        = "Estimert gruppeforskjell (prosentpoeng)",
    y        = NULL
  ) +
  tema +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# =============================================================================
# Lagre figurer
# =============================================================================
ggsave("output/figures/boksplot_lbm.png",    p_boks_lbm, width = 5, height = 5, dpi = 250)
ggsave("output/figures/boksplot_fat.png",    p_boks_fat, width = 5, height = 5, dpi = 250)
ggsave("output/figures/ekvivalens_lbm.png",  p_ekv_lbm,  width = 6, height = 3, dpi = 250)
ggsave("output/figures/ekvivalens_fat.png",  p_ekv_fat,  width = 6, height = 3, dpi = 250)

cat("Lagret 4 figurer i output/figures/\n")
