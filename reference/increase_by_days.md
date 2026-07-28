# increase_by_days

Adds (or subtracts, for negative `nr_days`) whole calendar days to a
date/time, preserving wall-clock time across daylight-saving-time
transitions.

## Usage

``` r
increase_by_days(timeobj, nr_days)
```

## Arguments

- timeobj:

  A POSIXct object or a character string parseable by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

- nr_days:

  Integer number of days to add (negative to subtract).

## Value

A POSIXct object, `nr_days` calendar days after (or before) `timeobj`,
at the same wall-clock time.

## Details

Uses lubridate's `Period` arithmetic (`days()`), which is calendar-based
rather than duration-based: adding "3 days" always lands on the same
clock time 3 calendar days later, even if a DST transition falls in
between. Plain `POSIXct + (nr_days * 86400)` arithmetic, by contrast,
adds a fixed number of seconds and will land an hour off across a DST
transition.

## Examples

``` r
increase_by_days("2025-03-10 10:00:00", 5)
#> [1] "2025-03-15 10:00:00 UTC"
increase_by_days("2025-03-10 10:00:00", -3)
#> [1] "2025-03-07 10:00:00 UTC"
```
