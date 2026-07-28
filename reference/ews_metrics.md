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
slowing down") are classic examples. Applied to actigraphy, they have
been explored as possible early indicators of impending state
transitions in mood or health (e.g. an approaching depressive episode).

## References

Scheffer, M., Bascompte, J., Brock, W. A., Brovkin, V., Carpenter, S.
R., Dakos, V., Held, H., van Nes, E. H., Rietkerk, M., & Sugihara, G.
(2009). Early-warning signals for critical transitions. *Nature*,
461(7260), 53-59.

Van de Leemput, I. A., Wichers, M., Cramer, A. O. J., Borsboom, D.,
Tuerlinckx, F., Kuppens, P., van Nes, E. H., Viechtbauer, W., Giltay, E.
J., Aggen, S. H., Derom, C., Jacobs, N., Kendler, K. S., van der Maas,
H. L. J., Neale, M. C., Peeters, F., Thiery, E., Zachar, P., & Scheffer,
M. (2014). Critical slowing down as early warning for the onset and
termination of depression. *Proceedings of the National Academy of
Sciences*, 111(1), 87-92.
