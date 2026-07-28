# sleeplog_from_markers

Calculate sleeplog from markers

## Usage

``` r
sleeplog_from_markers(
  workdir,
  i,
  ACTdata.files,
  on_missing_markers = c("median", "manual", "abort")
)
```

## Arguments

- workdir:

  The directory where the sleep files are located.

- i:

  The index of the current file in ACTdata.files

- ACTdata.files:

  The current file in ACTdata.files

- on_missing_markers:

  What to do when derived Bedtime/Gotup markers are missing. One of
  \`"median"\` (default; impute with the median time), \`"manual"\`
  (open an interactive editor via \`fix()\`; only usable in an
  interactive session), or \`"abort"\`.

## Value

Returns a sleeplog
