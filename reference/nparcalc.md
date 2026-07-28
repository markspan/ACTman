# nparcalc

Device- and moving-window-aware entry point for circadian rhythm and
early-warning-signal analysis. Handles column normalization and
windowing (which portion of the recording to analyze, depending on
device and whether a moving window is in use), then delegates the actual
statistics to
[`circadian_metrics`](https://markspan.github.io/ACTman/reference/circadian_metrics.md)
(IS, IV, RA, L5, M10) and
[`ews_metrics`](https://markspan.github.io/ACTman/reference/ews_metrics.md)
(Mean, Variance, SD, CoV, Skewness, Kurtosis, autocorrelation, time to
recovery). Kept as a single entry point for backward compatibility with
existing callers/scripts.

## Usage

``` r
nparcalc(myACTdevice, movingwindow, CRV.data, ACTdata.1.sub, out = NULL)
```

## Arguments

- myACTdevice:

  Name of the input device used. Should be either Actiwatch2 or MW8.

- movingwindow:

  A boolean indicating whether moving window is used.

- CRV.data:

  CRV data

- ACTdata.1.sub:

  Managed data set

- out:

  Optional. When movingwindow is TRUE, this is the current window of
  data.

## Value

A named list combining the outputs of
[`circadian_metrics`](https://markspan.github.io/ACTman/reference/circadian_metrics.md)
and
[`ews_metrics`](https://markspan.github.io/ACTman/reference/ews_metrics.md),
plus `CRV_data` (the windowed data actually used for the calculations).

## See also

[`circadian_metrics`](https://markspan.github.io/ACTman/reference/circadian_metrics.md),
[`ews_metrics`](https://markspan.github.io/ACTman/reference/ews_metrics.md)
