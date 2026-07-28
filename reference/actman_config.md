# actman_config

Builds and validates a single configuration object for an \`ACTman()\`
run. Consolidates the parameter validation that used to be scattered
across the top of \`ACTman()\` and (for \`myACTdevice\`) re-checked
inside its per-file loop on every iteration, into one place that fails
fast, before any file is processed.

## Usage

``` r
actman_config(
  workdir,
  sleepdatadir = workdir,
  myACTdevice = "Actiwatch2",
  iwantsleepanalysis = FALSE,
  plotactogram = FALSE,
  selectperiod = FALSE,
  startperiod = NULL,
  daysperiod = FALSE,
  endperiod = NULL,
  movingwindow = FALSE,
  movingwindow.size = 14,
  movingwindow.jump = 1,
  circadian_analysis = TRUE,
  nparACT_compare = FALSE,
  na_omit = FALSE,
  na_impute = FALSE,
  missings_report = TRUE,
  lengthcheck = TRUE,
  i_want_EWS = FALSE,
  on_high_missings = c("continue", "abort"),
  on_missing_markers = c("median", "manual", "abort")
)
```

## Arguments

- workdir:

  The working directory of the script.

- sleepdatadir:

  An optional vector specifying the directory for actogram and sleep
  analysis data.

- myACTdevice:

  Name of the input device used. Should be either 'Actiwatch2' or 'MW8'.

- iwantsleepanalysis:

  Boolean value indicating whether sleep analysis should be performed.

- plotactogram:

  Value indicating if and what kind of actogram has to be plotted. Can
  be either '48h', '24h', or FALSE.

- selectperiod:

  Boolean value indicating whether a specific period has to be selected.

- startperiod:

  An optional vector specifying single or multiple period starts. Should
  be in the format "2016-10-03 00:00:00".

- daysperiod:

  An optional vector specifying the length in days of the period.

- endperiod:

  An optional argument that is a date string (format: "2016-10-03
  00:00:00"), denoting the end of the data subset to be analyzed. Only
  used if daysperiod is not specified.

- movingwindow:

  Boolean value indicating whether a moving window should be utilised.

- movingwindow.size:

  An optional vector specifying the length in days of the moving window.
  Default is 14 days.

- movingwindow.jump:

  An optional vector specifying the length of the jumps with which the
  moving window is shifted each iteration. Default is 1 day.

- circadian_analysis:

  Boolean value indicating whether non-parametric circadian rhythm
  analysis should be performed.

- nparACT_compare:

  Boolean value indicating that comparison with another actigraphy R
  package should be performed. If TRUE, the values for IS, IV, RA, L5,
  L5_starttime, M10, and M10_starttime of the nparACT_base_loop function
  are recorded in the returned overview variable.

- na_omit:

  Boolean value indicating whether NA's should be omitted.

- na_impute:

  Boolean value indicating whether NA's should be imputed.

- missings_report:

  Boolean value indicating whether missings promt should appear.

- lengthcheck:

  Boolean value. If TRUE, the dataset is shortened to the start date
  plus 14 days, and observations more than 14 days after the start date
  are removed.

- i_want_EWS:

  Boolean value indicating whether early-warning-signal statistics (see
  \`?ews_metrics\`) should be overlaid on the actogram plot. Requires
  \`movingwindow = TRUE\` in the same call, since it plots against the
  rolling-window results.

- on_high_missings:

  What to do when more than 0.01% of a dataset's activity values are
  missing and \`missings_report\` is TRUE. One of \`"continue"\`
  (default; proceed with the analysis and note the situation) or
  \`"abort"\` (stop processing this dataset). Replaces the old
  interactive \`readline()\` prompt so batch/CI runs never hang.

- on_missing_markers:

  What to do when the sleeplog derived from marker/button-press files
  has missing Bedtime/Gotup values. One of \`"median"\` (default; impute
  missing times with the median), \`"manual"\` (open an interactive
  editor via \`fix()\`; only usable in an interactive session), or
  \`"abort"\` (stop). Replaces the old interactive \`readline()\`
  prompt.

## Value

A list (S3 class \`actman_config\`) with one element per validated
parameter.

## Details

\`ACTman()\` calls this internally as its first step and keeps its own
argument list unchanged for backward compatibility, so existing calls to
\`ACTman(workdir = ..., myACTdevice = ..., ...)\` continue to work
exactly as before; \`actman_config()\` is exported so it can also be
built and inspected independently, e.g. for validating a set of
parameters before a long batch job, or for building one config and
reusing it across multiple similar \`ACTman()\`-style calls.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- actman_config(workdir = "~/actigraphy/study1", myACTdevice = "Actiwatch2")
} # }
```
