# actman_paths

Builds a small structure holding every path ACTman needs, computed once
(and normalized to an absolute path) from the user-supplied working
directory. This replaces the previous pattern of repeated
[`setwd()`](https://rdrr.io/r/base/getwd.html) calls scattered across
the package, which left the R session's working directory mutated after
any error partway through a run, and made every downstream
[`list.files()`](https://rdrr.io/r/base/list.files.html)/[`read.csv()`](https://rdrr.io/r/utils/read.table.html)
call implicitly depend on "wherever the process happens to be right now"
rather than an explicit path. No function in this package changes the
process's working directory any more; every read/write goes through an
`actman_paths` object instead.

## Usage

``` r
actman_paths(workdir, sleepdatadir = workdir)
```

## Arguments

- workdir:

  The main data directory. May be given as a relative or absolute path;
  it is normalized to an absolute path (and must exist).

- sleepdatadir:

  Optional separate directory for sleep-analysis inputs/outputs.
  Defaults to `workdir`. Normalized to an absolute path if it exists;
  left as given (with a warning suppressed) if it doesn't yet exist,
  since some workflows create it after the fact.

## Value

A list (S3 class `actman_paths`) with elements `workdir`,
`sleepdatadir`, `managed_dir` ("Managed Datasets" under `workdir`),
`results_dir` ("Results" under `workdir`), and `actogram_dir`
("Actograms" under `workdir`).
