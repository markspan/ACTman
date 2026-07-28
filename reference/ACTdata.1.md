# Example actigraphy data

A real, anonymized MotionWatch 8 (CamNtech) actigraphy recording: ~33
days (2017-07-05 to 2017-08-07) at 30-second epochs (94,721
observations), already extracted to the 3-column Date/Time/Activity form
\`ACTman()\` produces internally after reading a raw MW8 export (i.e.
this is what \`ACTdata.1.sub\` looks like partway through the pipeline,
not a raw device file – for a raw-format example file usable directly
with \`ACTman(myACTdevice = "MW8", ...)\`, see \`system.file("extdata",
"example-mw8-participant.csv", package = "ACTman")\`, a ~7-day subset of
the same recording in real MW8 export format). No missing values. See
\`vignette("actman-intro")\` for a full walkthrough.

## Format

A data frame with 94,721 rows and 3 variables:

- A:

  Date, as `"YYYY-MM-DD"`

- B:

  Time, as `"HH:MM:SS"` (30-second epochs)

- C:

  Activity count for that epoch

## Details

**Provenance:** included by the package's original author, Yoram
Kunkels, as example data (added 2017; see package `LICENSE`, which lists
him as copyright holder).

## References

Kunkels, Y. K., Knapen, S. E., Zuidersma, M., Wichers, M., Riese, H., &
Emerencia, A. C. (2020). ACTman: Automated preprocessing and analysis of
actigraphy data. *Journal of Science and Medicine in Sport*, 23(5),
481-486.

## Author

Yoram K. Kunkels <y.k.kunkels@umcg.nl>
