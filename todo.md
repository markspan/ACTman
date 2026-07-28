# To-do list for ACTman

- [x] Update daylight-saving-time method -- done via `lubridate::days()` in
      `increase_by_days()` (Item 3/Phase 4, see NEWS.md).
- [ ] Make `plot_actogram()` responsive to data length, instead of the
      current method limiting (and requiring) it to 14 days.
- [ ] For sleep variables, select the night in the sleeplog based on date,
      instead of row number.
- [ ] Report actogram/data files based on filename, instead of iteration
      number.
- [ ] Match sleeplog with data file based on filename, instead of iteration
      number.
- [ ] Rename the very terse variable names in `sleep_summary.R`'s per-night
      loop (`aaa`, `tempp`, `sleepend`, `rownr.*`) once test coverage of its
      edge-case branches is strong enough to do so safely (see NEWS.md,
      Phase 5's "known awkward patterns identified but intentionally not
      changed").
- [ ] Replace `actogram.R`'s `assign()`/`eval(parse())` per-day variable
      construction with a list (`days[[i]]`).
- [ ] Add test coverage for the EWS-overlay-on-real-rolling-window-data path
      in `plot_actogram()` (`i_want_EWS = TRUE` with real, non-`NA` rolling
      window results) -- currently only the error-path (`NA` results) is
      tested; see the `# nolint` block in `actogram.R`.
