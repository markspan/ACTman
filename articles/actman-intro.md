# Getting started with ACTman

This vignette walks through a complete ACTman analysis using a real,
anonymized actigraphy recording bundled with the package, rather than
synthetic data. If you use ACTman in published work, please cite the
paper describing it:

> Kunkels, Y. K., Knapen, S. E., Zuidersma, M., Wichers, M., Riese, H.,
> & Emerencia, A. C. (2020). ACTman: Automated preprocessing and
> analysis of actigraphy data. *Journal of Science and Medicine in
> Sport*, 23(5), 481-486. <https://doi.org/10.1016/j.jsams.2019.11.009>

Read the paper: the [publisher’s
version](https://doi.org/10.1016/j.jsams.2019.11.009) requires access;
an [open-access copy is available via the University of Groningen’s Pure
repository](https://pure.rug.nl/ws/files/123896013/1_s2.0_S1440244019302877_main.pdf).

## The example data

ACTman bundles a real MotionWatch 8 (CamNtech) recording, in two forms:

- `system.file("extdata", "example-mw8-participant.csv", package = "ACTman")`:
  a ~7-day subset in genuine raw MW8 export format (device metadata
  lines, a `Raw data:` marker, then `Date,Time,Activity` rows at
  30-second epochs) – exactly what you’d get from a real MotionWare
  export, useful for seeing the full pipeline including file reading.
- `data(ACTdata.1)`: the complete ~33-day recording (2017-07-05 to
  2017-08-07, 94,721 observations), already extracted to the
  Date/Time/Activity form ACTman produces internally partway through its
  pipeline.

This vignette uses the raw-format file, since that’s what you’d actually
point
[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md) at
with your own data.

``` r

library(ACTman)

example_file <- system.file("extdata", "example-mw8-participant.csv", package = "ACTman")
workdir <- tempfile("actman-vignette-")
dir.create(workdir)
file.copy(example_file, file.path(workdir, "participant01.csv"))
#> [1] TRUE

list.files(workdir)
#> [1] "participant01.csv"
```

## Running the analysis

A single
[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md) call
reads the file, bins any 30-second epochs into 60-second epochs, checks
for missing data and trailing zero-activity, and computes the
non-parametric circadian rhythm variables:

``` r

result <- ACTman(
  workdir = workdir,
  myACTdevice = "MW8",
  circadian_analysis = TRUE,
  iwantsleepanalysis = FALSE,
  plotactogram = FALSE,
  lengthcheck = FALSE # this example file is only ~7 days; don't require 14
)
#> [1] "*** Start of Dataset 1 ***"
#> [1] ""
#> [1] "Dataset Name: participant01.csv"
#> [1] ""
#> [1] "Detecting Epoch Length......."
#> [1] "Warning: 30 sec. Epoch's Detected!"
#> [1] "Action: Binning 30 sec. Epochs in 60 sec. Epochs"
#> [1] ""
#> [1] "Task: Reporting NA's"
#> [1] "Number of NA's in this Dataset: 0"
#> [1] "This is: 0 % of the total number of observations!"
#> [1] ""
#> [1] "Task: Checking for Activity in Last 5 observations"
#> [1] "Task OK: Dataset contained Activity in Last 5 observations."
#> [1] ""
#> [1] "-------------------------------------- END OF DATASET 1 --- @ 100 % DONE --------------------------------------"

result
#> <actman_result>
#>   $overview:       1 file(s) x 21 column(s)
#>   $circadian:      1 file(s) x 7 column(s)
#>   $sleep:          NULL (iwantsleepanalysis = FALSE)
#>   $rolling_window: NULL (movingwindow = FALSE)
```

[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
always returns a single `actman_result` object; here, since we asked for
a circadian analysis only, `$circadian` is populated and
`$sleep`/`$rolling_window` are `NULL`.

## Interpreting the circadian rhythm variables

``` r

result$overview[, c("filename", "recordingtime2", "missings_perc")]
#>            filename recordingtime2 missings_perc
#> 1 participant01.csv              7             0
result$circadian
#>    IS   IV   RA    L5 L5_starttime    M10 M10_starttime
#> 1 0.5 0.81 0.84 43.54     00:08:00 502.43      07:45:00
```

- **IS** (Interdaily Stability) close to 1 means a highly repeatable
  24-hour activity pattern day after day; closer to 0 means little
  consistent daily structure.
- **IV** (Interdaily Variability) captures fragmentation: higher values
  mean more switching between rest and activity within a day.
- **L5**/**M10** are the average activity levels during the 5
  least-active and 10 most-active hours of the average day, with their
  respective onset times (`L5_starttime`, `M10_starttime`).
- **RA** (Relative Amplitude), `(M10 - L5) / (M10 + L5)`, summarizes the
  day/night activity contrast in a single normalized number.

See
[`vignette("actman-intro")`](https://markspan.github.io/ACTman/articles/actman-intro.md)’s
references, or
[`?circadian_metrics`](https://markspan.github.io/ACTman/reference/circadian_metrics.md),
for the methodological background (Van Someren et al., 1999; Witting et
al., 1990).

## Actogram

``` r

ACTman(
  workdir = workdir,
  myACTdevice = "MW8",
  circadian_analysis = FALSE,
  iwantsleepanalysis = FALSE,
  plotactogram = "24h",
  lengthcheck = FALSE
)
#> [1] "*** Start of Dataset 1 ***"
#> [1] ""
#> [1] "Dataset Name: participant01.csv"
#> [1] ""
#> [1] "Detecting Epoch Length......."
#> [1] "Warning: 30 sec. Epoch's Detected!"
#> [1] "Action: Binning 30 sec. Epochs in 60 sec. Epochs"
#> [1] ""
#> [1] "Task: Reporting NA's"
#> [1] "Number of NA's in this Dataset: 0"
#> [1] "This is: 0 % of the total number of observations!"
#> [1] ""
#> [1] "Task: Checking for Activity in Last 5 observations"
#> [1] "Task OK: Dataset contained Activity in Last 5 observations."
#> [1] ""
#> [1] "-------------------------------------- END OF DATASET 1 --- @ 100 % DONE --------------------------------------"
#> <actman_result>
#>   $overview:       1 file(s) x 21 column(s)
#>   $circadian:      NULL (circadian_analysis = FALSE)
#>   $sleep:          NULL (iwantsleepanalysis = FALSE)
#>   $rolling_window: NULL (movingwindow = FALSE)

actogram_file <- list.files(file.path(workdir, "Actograms"), full.names = TRUE, pattern = "\\.pdf$")[1]
actogram_file
#> [1] "/tmp/RtmpaU9v4R/actman-vignette-1b6a54280e6c/Actograms/Actigraphy Data - 24h Plot 1 .pdf"
```

[`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
writes a PDF (24h or 48h) to `workdir/Actograms`; open Actigraphy Data -
24h Plot 1 .pdf to see it (PDFs can’t be embedded inline in this
vignette, but see the actogram image in the package’s `README.md` for an
example of what it looks like).

## Moving-window analysis

For longer recordings, `movingwindow = TRUE` re-runs the circadian (and
early-warning-signal) calculations on a sliding window instead of the
whole recording at once, producing a time series of each metric. This
needs more days of data than the 7-day example file provides for a
window of the default size, so this example uses a 3-day window stepped
by 1 day:

``` r

rolling_result <- ACTman(
  workdir = workdir,
  myACTdevice = "MW8",
  circadian_analysis = FALSE,
  iwantsleepanalysis = FALSE,
  plotactogram = FALSE,
  movingwindow = TRUE,
  movingwindow.size = 3,
  movingwindow.jump = 1,
  lengthcheck = FALSE
)
#> [1] "*** Start of Dataset 1 ***"
#> [1] ""
#> [1] "Dataset Name: participant01.csv"
#> [1] ""
#> [1] "Detecting Epoch Length......."
#> [1] "Warning: 30 sec. Epoch's Detected!"
#> [1] "Action: Binning 30 sec. Epochs in 60 sec. Epochs"
#> [1] ""
#> [1] "Task: Reporting NA's"
#> [1] "Number of NA's in this Dataset: 0"
#> [1] "This is: 0 % of the total number of observations!"
#> [1] ""
#> [1] "Task: Checking for Activity in Last 5 observations"
#> [1] "Task OK: Dataset contained Activity in Last 5 observations."
#> [1] ""
#> [1] "---------------------------------------------------------------------------------"
#> [1] "Rolling window CRV analysis output - Window step: 0"
#> [1] "Begin time: 2017-07-05"
#> [1] "End time: 2017-07-08"
#> [1] "nOBS: 4320"
#> [1] ""
#> [1] "Circadian Rhythm Variables"
#> [1] "IS:  0.63"
#> [1] "IV:  0.84"
#> [1] "RA:  0.91"
#> [1] "L5:  25.35"
#> [1] "L5_starttime:  00:23:00"
#> [1] "M10:  564.93"
#> [1] "M10_starttime:  07:38:00"
#> [1] ""
#> [1] "Early-Warning Signals"
#> [1] "Mean:  333.14"
#> [1] "Variance:  325125.38"
#> [1] "SD:  570.2"
#> [1] "Coefficient of Variation:  171.16"
#> [1] "Skewness:  4.76"
#> [1] "Kurtosis:  47.31"
#> [1] "Autocorr at-lag-1:  0.78"
#> [1] "Autocorr at-lag-2:  0.63"
#> [1] "Autocorr at-lag-3:  0.54"
#> [1] "Autocorr at-lag-60:  0.13"
#> [1] "Autocorr at-lag-120:  0.07"
#> [1] "Time_to_Recovery:  27"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "Rolling window CRV analysis output - Window step: 1"
#> [1] "Begin time: 2017-07-06"
#> [1] "End time: 2017-07-09"
#> [1] "nOBS: 4321"
#> [1] ""
#> [1] "Circadian Rhythm Variables"
#> [1] "IS:  0.54"
#> [1] "IV:  0.74"
#> [1] "RA:  0.83"
#> [1] "L5:  40.55"
#> [1] "L5_starttime:  00:00:00"
#> [1] "M10:  445.55"
#> [1] "M10_starttime:  07:37:00"
#> [1] ""
#> [1] "Early-Warning Signals"
#> [1] "Mean:  286.13"
#> [1] "Variance:  242421.25"
#> [1] "SD:  492.36"
#> [1] "Coefficient of Variation:  172.08"
#> [1] "Skewness:  5.77"
#> [1] "Kurtosis:  74.31"
#> [1] "Autocorr at-lag-1:  0.75"
#> [1] "Autocorr at-lag-2:  0.59"
#> [1] "Autocorr at-lag-3:  0.52"
#> [1] "Autocorr at-lag-60:  0.09"
#> [1] "Autocorr at-lag-120:  0.05"
#> [1] "Time_to_Recovery:  28"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "Rolling window CRV analysis output - Window step: 2"
#> [1] "Begin time: 2017-07-07"
#> [1] "End time: 2017-07-10"
#> [1] "nOBS: 4321"
#> [1] ""
#> [1] "Circadian Rhythm Variables"
#> [1] "IS:  0.49"
#> [1] "IV:  0.76"
#> [1] "RA:  0.81"
#> [1] "L5:  53.78"
#> [1] "L5_starttime:  00:00:00"
#> [1] "M10:  511.33"
#> [1] "M10_starttime:  07:37:00"
#> [1] ""
#> [1] "Early-Warning Signals"
#> [1] "Mean:  321.73"
#> [1] "Variance:  293187.07"
#> [1] "SD:  541.47"
#> [1] "Coefficient of Variation:  168.3"
#> [1] "Skewness:  4.77"
#> [1] "Kurtosis:  52.07"
#> [1] "Autocorr at-lag-1:  0.75"
#> [1] "Autocorr at-lag-2:  0.61"
#> [1] "Autocorr at-lag-3:  0.54"
#> [1] "Autocorr at-lag-60:  0.11"
#> [1] "Autocorr at-lag-120:  0.07"
#> [1] "Time_to_Recovery:  32"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "Rolling window CRV analysis output - Window step: 3"
#> [1] "Begin time: 2017-07-08"
#> [1] "End time: 2017-07-11"
#> [1] "nOBS: 4321"
#> [1] ""
#> [1] "Circadian Rhythm Variables"
#> [1] "IS:  0.5"
#> [1] "IV:  0.66"
#> [1] "RA:  0.78"
#> [1] "L5:  54.95"
#> [1] "L5_starttime:  03:13:00"
#> [1] "M10:  447.2"
#> [1] "M10_starttime:  09:08:00"
#> [1] ""
#> [1] "Early-Warning Signals"
#> [1] "Mean:  270.11"
#> [1] "Variance:  187962.36"
#> [1] "SD:  433.55"
#> [1] "Coefficient of Variation:  160.51"
#> [1] "Skewness:  2.76"
#> [1] "Kurtosis:  13.48"
#> [1] "Autocorr at-lag-1:  0.7"
#> [1] "Autocorr at-lag-2:  0.58"
#> [1] "Autocorr at-lag-3:  0.52"
#> [1] "Autocorr at-lag-60:  0.19"
#> [1] "Autocorr at-lag-120:  0.11"
#> [1] "Time_to_Recovery:  61"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "Rolling window CRV analysis output - Window step: 4"
#> [1] "Begin time: 2017-07-09"
#> [1] "End time: 2017-07-12"
#> [1] "nOBS: 4321"
#> [1] ""
#> [1] "Circadian Rhythm Variables"
#> [1] "IS:  0.59"
#> [1] "IV:  1.03"
#> [1] "RA:  0.84"
#> [1] "L5:  47.49"
#> [1] "L5_starttime:  23:39:00"
#> [1] "M10:  554.39"
#> [1] "M10_starttime:  09:10:00"
#> [1] ""
#> [1] "Early-Warning Signals"
#> [1] "Mean:  305.61"
#> [1] "Variance:  319694.27"
#> [1] "SD:  565.42"
#> [1] "Coefficient of Variation:  185.01"
#> [1] "Skewness:  4.2"
#> [1] "Kurtosis:  35.18"
#> [1] "Autocorr at-lag-1:  0.75"
#> [1] "Autocorr at-lag-2:  0.62"
#> [1] "Autocorr at-lag-3:  0.59"
#> [1] "Autocorr at-lag-60:  0.18"
#> [1] "Autocorr at-lag-120:  0.06"
#> [1] "Time_to_Recovery:  46"
#> [1] "---------------------------------------------------------------------------------"
#> [1] "-------------------------------------- END OF DATASET 1 --- @ 100 % DONE --------------------------------------"

rolling_result$rolling_window[, c("starttime", "endtime", "IS", "IV", "RA", "Mean", "SD")]
#>             starttime             endtime   IS   IV   RA   Mean     SD
#> 1 2017-07-05 00:00:00 2017-07-08 00:00:00 0.63 0.84 0.91 333.14 570.20
#> 2 2017-07-06 00:00:00 2017-07-09 00:00:00 0.54 0.74 0.83 286.13 492.36
#> 3 2017-07-07 00:00:00 2017-07-10 00:00:00 0.49 0.76 0.81 321.73 541.47
#> 4 2017-07-08 00:00:00 2017-07-11 00:00:00 0.50 0.66 0.78 270.11 433.55
#> 5 2017-07-09 00:00:00 2017-07-12 00:00:00 0.59 1.03 0.84 305.61 565.42
```

Each row is one 3-day window; `IS`/`IV`/`RA` are the same circadian
variables as before, now tracked across the recording, alongside
early-warning-signal statistics (`Mean`, `SD`, autocorrelation, etc. –
see
[`?ews_metrics`](https://markspan.github.io/ACTman/reference/ews_metrics.md))
that have been explored as potential indicators of mood-state
transitions in longer recordings, though see Helmich et al. (2024) for a
critical perspective on the current evidence for this use.

## Sleep analysis

Sleep analysis (`iwantsleepanalysis = TRUE`) additionally requires a
sleeplog or event-marker file alongside the actigraphy data (see the
“Data formats” section of `README.md` for the expected format) – not
included with this particular example recording, so it isn’t
demonstrated here. See
[`?sleepdata_overview`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md)
and
[`?sleeplog_from_markers`](https://markspan.github.io/ACTman/reference/sleeplog_from_markers.md)
for details.

## Cleaning up
