## Characterization run: executes the CURRENT (pre-fix) upstream pipeline
## against the synthetic fixtures and freezes numeric output as a baseline.
## Purpose: catch unintended behavior changes during the later refactor.
## This is NOT a correctness test -- known bugs are expected to reproduce.

.libPaths(c("/home/claude/Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(moments)
})

pkg_root <- "/home/claude/actman-work"

for (f in list.files(file.path(pkg_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) {
  source(f)
}

fixtures_root <- file.path(pkg_root, "tests", "fixtures")
out_root <- file.path(pkg_root, "tests", "characterization", "output")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

run_one <- function(name, workdir, device) {
  message("=== Running baseline for: ", name, " (", device, ") ===")
  res <- tryCatch({
    ACTman(workdir = workdir, myACTdevice = device,
           iwantsleepanalysis = FALSE, plotactogram = FALSE,
           selectperiod = FALSE, movingwindow = FALSE,
           circadian_analysis = TRUE, nparACT_compare = FALSE,
           na_omit = FALSE, na_impute = FALSE, missings_report = FALSE,
           lengthcheck = TRUE, i_want_EWS = FALSE)
  }, error = function(e) {
    message("ERROR for ", name, ": ", conditionMessage(e))
    NULL
  })
  if (!is.null(res)) {
    saveRDS(res, file.path(out_root, paste0(name, "_overview.rds")))
    write.csv(res, file.path(out_root, paste0(name, "_overview.csv")), row.names = FALSE)
    message("Saved baseline for ", name, ": ", nrow(res), " row(s), ", ncol(res), " col(s)")
  }
  res
}

r1 <- run_one("actiwatch2", file.path(fixtures_root, "actiwatch2_sample"), "Actiwatch2")
r2 <- run_one("mw8", file.path(fixtures_root, "mw8_sample"), "MW8")

message("Done.")
