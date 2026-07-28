# roundup_power_10

Rounds a value up to the nearest power of 10 (e.g. 342 -\> 1000, 8 -\>
10). Used to pick a "clean" y-axis upper limit for actogram/EWS plots.

## Usage

``` r
roundup_power_10(x)
```

## Arguments

- x:

  A positive numeric value.

## Value

The smallest power of 10 that is \>= x.

## Examples

``` r
if (FALSE) { # \dontrun{
roundup_power_10(342)  # 1000
roundup_power_10(8)    # 10
} # }
```
