#' actman_paths
#'
#' Builds a small structure holding every path ACTman needs, computed once
#' (and normalized to an absolute path) from the user-supplied working
#' directory. This replaces the previous pattern of repeated \code{setwd()}
#' calls scattered across the package, which left the R session's working
#' directory mutated after any error partway through a run, and made every
#' downstream \code{list.files()}/\code{read.csv()} call implicitly depend
#' on "wherever the process happens to be right now" rather than an explicit
#' path. No function in this package changes the process's working
#' directory any more; every read/write goes through an \code{actman_paths}
#' object instead.
#'
#' @param workdir The main data directory. May be given as a relative or
#'   absolute path; it is normalized to an absolute path (and must exist).
#' @param sleepdatadir Optional separate directory for sleep-analysis
#'   inputs/outputs. Defaults to \code{workdir}. Normalized to an absolute
#'   path if it exists; left as given (with a warning suppressed) if it
#'   doesn't yet exist, since some workflows create it after the fact.
#'
#' @return A list (S3 class \code{actman_paths}) with elements
#'   \code{workdir}, \code{sleepdatadir}, \code{managed_dir} ("Managed
#'   Datasets" under \code{workdir}), \code{results_dir} ("Results" under
#'   \code{workdir}), and \code{actogram_dir} ("Actograms" under
#'   \code{workdir}).
#' @keywords internal
actman_paths <- function(workdir, sleepdatadir = workdir) {
  workdir <- normalizePath(workdir, mustWork = TRUE)
  sleepdatadir <- tryCatch(
    normalizePath(sleepdatadir, mustWork = TRUE),
    error = function(e) normalizePath(sleepdatadir, mustWork = FALSE)
  )

  paths <- list(
    workdir = workdir,
    sleepdatadir = sleepdatadir,
    managed_dir = file.path(workdir, "Managed Datasets"),
    results_dir = file.path(workdir, "Results"),
    actogram_dir = file.path(workdir, "Actograms")
  )
  class(paths) <- "actman_paths"
  paths
}

#' ensure_dir
#'
#' Creates a directory (recursively, silently if it already exists) and
#' returns the path invisibly, so it can be used inline:
#' \code{write.csv(x, file.path(ensure_dir(paths$results_dir), "out.csv"))}.
#'
#' @param path Directory path to ensure exists.
#' @return \code{path}, invisibly.
#' @keywords internal
ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}
