# plot_actogram

Function to plot 48 hour Actograms.

## Usage

``` r
plot_actogram(
  workdir,
  ACTdata.1.sub,
  i,
  plotactogram,
  rollingwindow.results,
  i_want_EWS
)
```

## Arguments

- workdir:

  the working directory as supplied to ACTman.

- ACTdata.1.sub:

  The managed data set

- i:

  The index of the current file in ACTdata.files

- plotactogram:

  Value indicating if and what kind of actogram has to be plotted. Can
  be either '48h', '24h', or FALSE

- rollingwindow.results:

  The moving-window results data frame produced by
  \`run_rolling_window()\` (as called from \`ACTman(movingwindow = TRUE,
  ...)\`), or \`NA\` if no moving-window analysis was run. Only used
  (and required) when \`i_want_EWS = TRUE\`.

- i_want_EWS:

  Boolean value indicating whether early-warning-signal statistics
  should be overlaid as an additional plot. Requires
  \`rollingwindow.results\` to be a real (non-NA) result.
