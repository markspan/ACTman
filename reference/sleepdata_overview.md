# sleepdata_overview

Calculate sleep data

## Usage

``` r
sleepdata_overview(
  workdir,
  actdata,
  i,
  lengthcheck,
  ACTdata.files,
  on_missing_markers = c("median", "manual", "abort")
)
```

## Arguments

- workdir:

  The directory where the sleep files are located.

- actdata:

  The activity data.

- i:

  The index of the current file in ACTdata.files

- lengthcheck:

  Boolean value. If TRUE, the dataset is shortened to the start date
  plus 14 days, and observations more than 14 days after the start date
  are removed.

- ACTdata.files:

  The current file in ACTdata.files

- on_missing_markers:

  What to do when the sleeplog generated from marker files has missing
  Bedtime/Gotup values. One of \`"median"\` (default), \`"manual"\`, or
  \`"abort"\`. See \`?ACTman\`. Only used when a sleeplog has to be
  generated from marker files.

## Value

Returns a sleepdata overview.
