library(readxl)
library(dplyr)

bak <- read_excel("data/raw/REACT_data_til_studenter.xlsx", sheet = "Bakgrunn", skip = 1) %>%
  filter(!is.na(fp)) %>%
  mutate(
    fp      = as.integer(fp),
    aar     = as.integer(`År`),
    dropout = !is.na(dropout) & dropout %in% c("y", "bytte")
  )

cat("=== TREATMENT-FORDELING ===\n")
cat("2024:\n")
print(table(bak$treatment[bak$aar == 2024], useNA = "ifany"))
cat("\n2025:\n")
print(table(bak$treatment[bak$aar == 2025], useNA = "ifany"))

cat("\n=== DROPOUT PER ARM ===\n")
cat("2024 (FALSE=ikke dropout, TRUE=dropout):\n")
print(table(bak$treatment[bak$aar == 2024], bak$dropout[bak$aar == 2024]))
cat("\n2025:\n")
print(table(bak$treatment[bak$aar == 2025], bak$dropout[bak$aar == 2025]))

cat("\n=== ALLE FP MED TREATMENT OG DROPOUT ===\n")
print(bak %>% select(fp, aar, treatment, dropout) %>% arrange(aar, fp), n = 100)
