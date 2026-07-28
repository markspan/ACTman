## R CMD check's static code analysis cannot see two categories of names
## used in this package, and would otherwise (incorrectly) flag them as
## undefined global variables:
##
## - `Activity..MW.counts.` is a data frame column referenced unquoted
##   inside a dplyr::mutate() call in score_epochs() (standard tidyverse
##   non-standard evaluation; the column exists on the data frame at
##   runtime, but static analysis can't know that).
## - `day.1.2`, `day.2.3`, and `day2` are variable names created dynamically
##   via assign()/eval(parse(...)) in plot_actogram()'s 48h-plot branch,
##   rather than through ordinary assignment.
##
## Declaring them here (the standard base-R mechanism for this situation,
## also used by e.g. dplyr itself) suppresses the false-positive NOTE
## without disabling the check for genuine undefined-variable typos
## elsewhere in the package.
utils::globalVariables(c("Activity..MW.counts.", "day.1.2", "day.2.3", "day2"))
