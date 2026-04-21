bak <- readRDS("data/processed/bakgrunn_clean.rds")
dxa <- readRDS("data/processed/dxa_clean.rds")

bak$dropout_lgl <- as.logical(bak$dropout)
bak$dropout_lgl[bak$fp == 80] <- FALSE
bak$dropout_lgl[bak$fp == 77] <- TRUE
bak$dropout_lgl[bak$fp == 45] <- FALSE
bak$dropout_lgl[bak$fp == 46] <- TRUE
bak$dxa_komplett <- bak$fp %in% unique(dxa$id)

cat("=== ALLE FP MED STATUS ===\n")
status <- bak[, c("fp", "treatment", "dropout", "dropout_lgl", "dxa_komplett")]
status$orig_dropout <- as.logical(bak$dropout)
print(status[order(status$fp), ], row.names = FALSE)

cat("\n=== MANUELLE KORREKSJONER (forskjell mellom original og korrigert) ===\n")
endret <- status[status$orig_dropout != status$dropout_lgl, ]
print(endret)

cat("\n=== FULLFORT (ikke dropout) ===\n")
fullfort <- bak[!bak$dropout_lgl, ]
cat("Antall fullfort:", nrow(fullfort), "\n")
cat("FP-numre fullfort:", paste(sort(fullfort$fp), collapse=", "), "\n")
