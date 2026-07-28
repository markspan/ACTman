# circadian_metrics

Calculates the non-parametric circadian rhythm variables IS, IV, RA, L5
(with its start time), and M10 (with its start time) from an activity
series that has already been windowed to the period of interest (see
[`nparcalc`](https://markspan.github.io/ACTman/reference/nparcalc.md),
which handles device-specific windowing and calls this function). This
is a pure function: it does not read files, print progress, or depend on
global state.

## Usage

``` r
circadian_metrics(CRV.data, movingwindow = FALSE)
```

## Arguments

- CRV.data:

  A data frame with (at least) `Date` and `Activity` columns, already
  windowed to the period of interest and ordered chronologically at
  1-minute resolution.

- movingwindow:

  Boolean. Whether this call is part of a moving-window analysis;
  affects whether the final aggregated hour is trimmed (kept for exact
  behavioral parity with the original combined implementation).

## Value

A named list with elements `IS`, `IV`, `RA`, `L5`, `L5_starttime`,
`M10`, `M10_starttime`.

## Details

\## Background

These are standard non-parametric measures of rest-activity rhythms used
in chronobiology and circadian rhythm research:

\- \*\*IS (Interdaily Stability)\*\*: quantifies how similar the 24-hour
activity pattern is from day to day. Ranges from 0 (no stable pattern)
towards 1 (highly stable/repeated pattern). Computed as the ratio of the
variance of the average 24-hour pattern to the overall variance. -
\*\*IV (Interdaily Variability)\*\*: quantifies the fragmentation of the
rhythm, i.e. how much activity changes from one hour to the next
relative to the day-to-day variance. Higher values indicate a more
fragmented rhythm. - \*\*L5\*\*: the average activity level during the 5
(clock-)hour period of lowest activity in the average 24-hour profile,
with `L5_starttime` marking when that period begins. Represents the
"trough" of the rhythm (typically nocturnal sleep). - \*\*M10\*\*: the
average activity level during the 10 (clock-)hour period of highest
activity, with `M10_starttime` marking when it begins. Represents the
"peak" of the daytime activity period. - \*\*RA (Relative
Amplitude)\*\*: `(M10 - L5) / (M10 + L5)`, a normalized measure of the
day/night activity contrast. Ranges from 0 (no contrast) to 1 (maximal
contrast).

## References

Van Someren, E. J. W., Swaab, D. F., Colenda, C. C., Cohen, W., McCall,
W. V., & Rosenquist, P. B. (1999). Bright light therapy: improved
sensitivity to its effects on rest-activity rhythms in Alzheimer
patients by application of nonparametric methods. *Chronobiology
International*, 16(4), 505-518.

Witting, W., Kwa, I. H., Eikelenboom, P., Mirmiran, M., & Swaab, D. F.
(1990). Alterations in the circadian rest-activity rhythm in aging and
Alzheimer's disease. *Biological Psychiatry*, 27(6), 563-572.
