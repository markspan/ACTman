# run_rolling_window

Runs a moving-window circadian/EWS analysis: repeatedly windows the data
into overlapping (or adjacent) spans of `window` minutes, shifting by
`jump` minutes each step, and calls
[`nparcalc`](https://markspan.github.io/ACTman/reference/nparcalc.md) on
each window. Extracted from
[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)'s
previously inline `rollingwindow()` closure with no behavior change.

## Usage

``` r
run_rolling_window(x, window, jump, myACTdevice, ACTdata.1.sub, verbose = TRUE)
```

## Arguments

- x:

  The full activity data (Date/Time/Activity or Date/Activity).

- window:

  Window length in minutes (e.g. `1440 * movingwindow.size`).

- jump:

  Step size in minutes between successive windows (e.g.
  `1440 * movingwindow.jump`).

- myACTdevice:

  Name of the input device used ("Actiwatch2" or "MW8"), passed through
  to
  [`nparcalc`](https://markspan.github.io/ACTman/reference/nparcalc.md).

- ACTdata.1.sub:

  The full managed dataset for this file, used by
  [`nparcalc`](https://markspan.github.io/ACTman/reference/nparcalc.md)
  for device-specific end-of-data detection.

- verbose:

  Whether to print per-window progress and results to the console (as
  the original implementation always did). Default TRUE for
  backward-compatible console output.

## Value

A data frame with one row per window and columns: `starttime`,
`endtime`, `IS`, `IV`, `RA`, `L5`, `L5_starttime`, `M10`,
`M10_starttime`, `Mean`, `Variance`, `SD`, `Coeff_of_Var`, `Skewness`,
`Kurtosis`, `Autocorr_lag1/2/3/60/120`, `Time_to_Recovery`.
