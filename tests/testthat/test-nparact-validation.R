## Scientific validation harness: cross-checks ACTman's own non-parametric
## circadian metrics (circadian_metrics()/nparcalc()) against the
## independent, peer-reviewed nparACT package (Blume et al., 2016), which
## implements the same published method (Van Someren et al., 1999) from a
## separate codebase. This is a genuine cross-validation against real data,
## not just internal consistency checking.
##
## nparACT is a Suggests (not Imports) dependency, since it's only needed
## for the optional nparACT_compare = TRUE feature; these tests skip
## cleanly when it isn't installed.
##
## Tolerances below reflect the actual observed agreement between the two
## independent implementations on the bundled real example recording (see
## NEWS.md and README's "Known limitations" for the full comparison table
## and discussion) -- they are deliberately set close to, not far looser
## than, that observed agreement, so this test's job is to catch a
## *regression* in that agreement, not to hide the current level of
## disagreement behind a wide tolerance.

test_that("ACTman's circadian metrics agree with the independent nparACT package within documented tolerances", {
  skip_if_not_installed("nparACT")

  fixture_dir <- withr::local_tempdir()
  file.copy(
    system.file("extdata", "example-mw8-participant.csv", package = "ACTman"),
    file.path(fixture_dir, "participant01.csv")
  )

  result <- suppressWarnings(suppressMessages(
    ACTman(workdir = fixture_dir, myACTdevice = "MW8", circadian_analysis = TRUE,
          nparACT_compare = TRUE, iwantsleepanalysis = FALSE, plotactogram = FALSE,
          lengthcheck = FALSE)
  ))

  ours <- result$overview
  ## L5_starttime/M10_starttime are prefixed r2.* only when nparACT_compare
  ## is requested alongside circadian_analysis (see ACTman()'s column
  ## renaming logic); IS/IV/RA/L5/M10 without the r2. prefix are nparACT's.
  expect_true(all(c("IS", "IV", "RA", "L5", "M10", "r2.IS", "r2.IV", "r2.RA", "r2.L5", "r2.M10") %in% names(ours)))

  ## RA and L5 agree closely between implementations.
  expect_equal(ours$RA, ours$r2.RA, tolerance = 0.05)
  expect_equal(ours$L5, ours$r2.L5, tolerance = ours$L5 * 0.15)
  expect_equal(ours$M10, ours$r2.M10, tolerance = ours$M10 * 0.15)

  ## L5_starttime matches exactly on this recording.
  expect_equal(ours$L5_starttime, ours$r2.L5_starttime)

  ## IS and IV show more meaningful (but bounded) divergence between the
  ## two independent implementations -- likely differing edge-case/
  ## windowing conventions rather than a bug in either, but not yet
  ## root-caused; tracked in README's "Known limitations". This assertion exists to catch a
  ## *worsening* of that divergence, not to claim closer agreement than
  ## is actually observed.
  expect_equal(ours$IS, ours$r2.IS, tolerance = 0.15)
  expect_equal(ours$IV, ours$r2.IV, tolerance = 0.3)

  ## M10_starttime shows the largest divergence observed (~1.5 hours on
  ## this recording) of any metric compared here. Investigated against
  ## nparACT's own source (nparACT_auxfunctions1.R, nparACT_RAfunctions.R):
  ## both packages use a structurally identical approach (average activity
  ## per minute-of-day across all days, then a 1440-minute sliding window
  ## search via which.min()/which.max()), so this isn't a fundamentally
  ## different method -- the most likely cause is a difference in exactly
  ## which minute each implementation treats as "position 1" of the
  ## 1440-minute cycle (day-boundary/alignment handling), not yet
  ## pinned down further. Not asserted for exact or close agreement here
  ## (would make this test flaky/misleading); recorded so a future
  ## investigation has a concrete, reproducible number and a specific
  ## hypothesis to start from, and printed so it's visible in test output
  ## rather than silently passing.
  message(sprintf(
    "M10_starttime: ACTman = %s, nparACT = %s (documented divergence, see README)",
    ours$M10_starttime, ours$r2.M10_starttime
  ))
})

test_that("ACTman errors clearly when nparACT_compare = TRUE but nparACT isn't installed", {
  skip_if(requireNamespace("nparACT", quietly = TRUE), "nparACT is installed in this environment")

  fixture_dir <- withr::local_tempdir()
  file.create(file.path(fixture_dir, "P01.csv"))

  expect_error(
    ACTman(workdir = fixture_dir, myACTdevice = "Actiwatch2", nparACT_compare = TRUE),
    "nparACT_compare = TRUE requires the 'nparACT' package"
  )
})
