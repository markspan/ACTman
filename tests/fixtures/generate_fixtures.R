## Generates small synthetic actigraphy datasets used purely to exercise the
## ACTman pipeline for characterization testing. Not real participant data.

set.seed(42)

n_days <- 16L
n_min  <- n_days * 1440L
start  <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
times  <- seq(start, by = "min", length.out = n_min)

hour_of_day <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M")) / 60
## Circadian-ish pattern: low at night, high during the day, plus noise.
base_activity <- 40 + 35 * pmax(0, sin((hour_of_day - 7) / 24 * 2 * pi))
activity <- round(pmax(0, base_activity + rnorm(n_min, sd = 15)))

## Sprinkle a few missing values (~0.3%) to exercise NA handling.
na_idx <- sample(seq_len(n_min), size = floor(n_min * 0.003))
activity[na_idx] <- NA

date_str <- format(times, "%d/%m/%Y")
time_str <- format(times, "%H:%M:%S")

## --- Actiwatch2 fixture --------------------------------------------------
## Device export: read with header = FALSE, real data lives in columns 4:6.
## No metadata row here (kept out to avoid column-type coercion quirks in
## this synthetic generator); the header-row-drop branch in ACTman is
## exercised separately by a small preprocessing unit test instead.
aw2_dir <- "actiwatch2_sample"
dir.create(aw2_dir, showWarnings = FALSE)
data_rows <- data.frame(V1 = seq_len(n_min), V2 = "Epoch", V3 = "",
  V4 = date_str, V5 = time_str, V6 = activity)
write.table(data_rows, file.path(aw2_dir, "P01.csv"), sep = ",",
  row.names = FALSE, col.names = FALSE, quote = FALSE, na = "NA")

## --- MW8 fixture ----------------------------------------------------------
## Device export: metadata lines, a literal "Raw data:" marker, one header
## line, then Date,Time,Activity data lines (60 sec epochs, no ":30" stamps).
mw8_dir <- "mw8_sample"
dir.create(mw8_dir, showWarnings = FALSE)
lines <- c(
  "Device,MotionWatch8,",
  "Raw data:,,",
  "Date,Time,Activity",
  paste(date_str, time_str, ifelse(is.na(activity), "", activity), sep = ",")
)
writeLines(lines, file.path(mw8_dir, "P01.csv"))

cat("Fixtures written:\n")
cat(" -", file.path(aw2_dir, "P01.csv"), "\n")
cat(" -", file.path(mw8_dir, "P01.csv"), "\n")
