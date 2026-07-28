# ensure_dir

Creates a directory (recursively, silently if it already exists) and
returns the path invisibly, so it can be used inline:
`write.csv(x, file.path(ensure_dir(paths$results_dir), "out.csv"))`.

## Usage

``` r
ensure_dir(path)
```

## Arguments

- path:

  Directory path to ensure exists.

## Value

`path`, invisibly.
