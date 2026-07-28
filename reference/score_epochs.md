# score_epochs

Computes epoch-by-epoch wake/sleep and mobile/immobile classifications
for a night's activity data, plus the "sleep chance" / "wakeup chance"
rolling indicators used to locate sleep onset and sleep offset. Pure
function: takes a data frame in, returns it with additional columns,
with no file I/O or side effects.

## Usage

``` r
score_epochs(aaa)
```

## Arguments

- aaa:

  A data frame for one night's data, with an `Activity..MW.counts.`
  column (numeric activity counts).

## Value

The same data frame with added columns: `score`, `WakeSleep` (1 = awake,
0 = asleep), `MobileImmobile` (1 = mobile, 0 = immobile),
`epoch.sleep.chance`, `sleep.chance`, and `wakeup.chance`.

## Details

\## Background

The wake/sleep `score` is a neighbor-weighted combination of the current
epoch's activity count and its immediate neighbors (2 minutes on either
side, with the closer neighbor weighted more heavily), thresholded at 20
("Medium Sensitivity" in the convention used by several actigraphy
scoring algorithms of this family; 40 would be "Low Sensitivity", 20
"High Sensitivity"). Per Kunkels et al. (2020) – the paper describing
ACTman itself – this specific weighting scheme is based on CamNtech's
own MotionWare "Information Bulletin No. 3" for the MotionWatch 8 device
(CamNtech, 2013); it is in the same general family of approach as the
independently-developed Cole-Kripke algorithm (Cole et al., 1992).

`sleep.chance` and `wakeup.chance` are simple rolling sums of a
binarized "was activity above threshold" indicator over the
next/previous 9 and 4 epochs respectively, used downstream to locate the
first sustained quiet period after bedtime (sleep onset) and the first
sustained active period before/after the scheduled wake time (sleep
offset).

## References

Kunkels, Y. K., Knapen, S. E., Zuidersma, M., Wichers, M., Riese, H., &
Emerencia, A. C. (2020). ACTman: Automated preprocessing and analysis of
actigraphy data. *Journal of Science and Medicine in Sport*, 23(5),
481-486.

CamNtech (2013). Information Bulletin No. 3: Sleep Analysis Algorithms.
MotionWare software documentation, CamNtech Ltd.

Cole, R. J., Kripke, D. F., Gruen, W., Mullaney, D. J., & Gillin, J. C.
(1992). Automatic sleep/wake identification from wrist activity.
*Sleep*, 15(5), 461-469.
