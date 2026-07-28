# ews_metrics

Calculates a set of distributional and time-series "early warning
signal" (EWS) statistics on an activity series that has already been
windowed to the period of interest (see
[`nparcalc`](https://markspan.github.io/ACTman/reference/nparcalc.md)).
Pure function: no file I/O, no printing, no global state.

## Usage

``` r
ews_metrics(CRV.data)
```

## Arguments

- CRV.data:

  A data frame with (at least) an `Activity` column, already windowed to
  the period of interest.

## Value

A named list with elements `Mean`, `Variance`, `SD`, `CoV` (coefficient
of variation, %), `Skewness`, `Kurtosis`, `Autocorr` (lag-1) /
`Autocorr_lag2` / `Autocorr_lag3` / `Autocorr_lag60` /
`Autocorr_lag120`, and `Time_to_Recovery` (the first lag, in minutes, at
which the autocorrelation function drops below 0.2; capped at
`MAX_AUTOCORR_LAG` if it never does within the computed range).

## Details

\## Background

Early warning signals are statistical indicators, originally developed
in the study of complex dynamical systems (e.g. ecosystem collapse),
that can precede a critical transition – rising variance, rising
autocorrelation, and slower recovery from perturbation ("critical
slowing down") are classic examples. They have been explored as possible
early indicators of impending state transitions in mood or health (e.g.
an approaching depressive episode) using actigraphy data specifically
(Kunkels et al., 2021, in bipolar disorder; Kunkels et al., 2023, during
antidepressant discontinuation). Helmich et al. (2024) provide a
critical perspective on this broader literature, arguing that current
empirical and theoretical support for using early-warning-signal-based
critical slowing down to predict changes in mental health symptoms is
limited, and that these limitations should be kept in mind when
interpreting the EWS statistics below.

## References

Helmich, M. A., Schreuder, M. J., Bringmann, L. F., Riese, H., Snippe,
E., & Smit, A. C. (2024). Slow down and be critical before using early
warning signals in psychopathology. *Nature Reviews Psychology*, 3,
767-780.

Kunkels, Y. K., Riese, H., Knapen, S. E., Riemersma-van der Lek, R. F.,
George, S. V., van Roon, A. M., Schoevers, R. A., & Wichers, M. (2021).
Efficacy of early warning signals and spectral periodicity for
predicting transitions in bipolar patients: an actigraphy study.
*Translational Psychiatry*, 11, 350.

Kunkels, Y. K., Smit, A. C., Minaeva, O., Snippe, E., George, S. V., van
Roon, A. M., Wichers, M., & Riese, H. (2023). Risk ahead:
actigraphy-based early-warning signals of increases in depressive
symptoms during antidepressant discontinuation. *Clinical Psychological
Science*, 11(5), 942-953.
