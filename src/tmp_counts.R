bak <- readRDS("data/processed/bakgrunn_clean.rds")
dxa <- readRDS("data/processed/dxa_clean.rds")

bak$dropout_lgl <- as.logical(bak$dropout)
bak$dropout_lgl[bak$fp == 80] <- FALSE
bak$dropout_lgl[bak$fp == 77] <- TRUE
bak$dropout_lgl[bak$fp == 45] <- FALSE
bak$dropout_lgl[bak$fp == 46] <- TRUE
bak$dxa_komplett <- bak$fp %in% unique(dxa$id)

cat("Totalt inkludert RCT:", nrow(bak), "\n")
cat("\nPer arm:\n")
print(table(bak$treatment))
cat("\nDropout per arm:\n")
print(table(bak$treatment, bak$dropout_lgl))
cat("\nFullfort med komplett DXA per arm:\n")
print(table(bak$treatment, (!bak$dropout_lgl & bak$dxa_komplett)))
cat("\nDropout-aarsak:\n")
print(table(bak$dropout_reason, useNA="always"))
