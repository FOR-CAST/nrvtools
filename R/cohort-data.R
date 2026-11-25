#' Write one summary year's `cohortData` to a partitioned parquet
#'
#' Writes `cd` to `root/year=<year>/part-0.parquet`, where `<year>` is `year` padded to four digits
#' (`700` gives `year=0700`), so that one `root` holds a replicate's `cohortData` at every summary
#' year as an arrow dataset partitioned by simulation time. The year is carried by the folder name
#' only: no `year` column is added, and reading the file gives back the columns of `cd`.
#'
#' The write is atomic, as for [write_nrv_parquet()]: the parquet is written to a temporary name in
#' the destination folder and renamed into place, so a reader never sees a partial file. An existing
#' part file for the same year is replaced. A zero-row `cd` is written (it keeps its columns), since
#' "no cohorts" is a valid state.
#'
#' @param cd A `data.frame` (e.g., a `data.table`) of cohort data.
#' @param root Directory under which the `year=<year>` folder is created (recursively, if needed).
#' @param year One finite, non-negative whole number (integer or double): the simulation year.
#'
#' @return The written parquet path (character, length 1).
#'
#' @seealso [read_cohort_data()]
#'
#' @export
write_cohort_parquet <- function(cd, root, year) {
  if (!is.data.frame(cd)) {
    stop("write_cohort_parquet(): `cd` must be a data.frame.", call. = FALSE)
  }
  if (
    !(is.numeric(year) && length(year) == 1L) || !is.finite(year) || year < 0 || year != round(year)
  ) {
    stop(
      "write_cohort_parquet(): `year` must be one finite, non-negative whole number.",
      call. = FALSE
    )
  }
  dst_dir <- file.path(root, paste0("year=", sprintf("%04d", as.integer(year))))
  fs::dir_create(dst_dir)
  dst <- file.path(dst_dir, "part-0.parquet")
  tmp <- tempfile(tmpdir = dst_dir, fileext = ".parquet.tmp")
  arrow::write_parquet(cd, tmp)
  if (!file.rename(tmp, dst)) {
    unlink(tmp)
    stop("write_cohort_parquet(): failed to publish ", dst, call. = FALSE)
  }
  dst
}

#' Read one year's `cohortData` from a `.parquet` or `.qs2` file
#'
#' Reads a `cohortData` file written either by [write_cohort_parquet()] (`.parquet`) or as a
#' serialized `.qs2` object (older runs), chosen by the file extension (case-insensitive).
#' Reading a `.qs2` file requires the suggested package \pkg{qs2}.
#'
#' @param x Path of one `.parquet` or `.qs2` file.
#' @param cols `NULL` (all columns) or a character vector of the columns to return, in the order
#'   given. A parquet file reads only those columns.
#'
#' @return A `data.table`, to which columns can be added by reference with `:=`.
#'
#' @seealso [write_cohort_parquet()]
#'
#' @export
read_cohort_data <- function(x, cols = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    shown <- if (is.atomic(x)) {
      paste(utils::head(as.character(x), 3L), collapse = ", ")
    } else {
      paste0("<", class(x)[1L], ">")
    }
    stop(
      "read_cohort_data(): `x` must be a single file path (length 1), not ",
      class(x)[1L],
      " of length ",
      length(x),
      ": ",
      substr(shown, 1L, 200L),
      call. = FALSE
    )
  }
  if (!file.exists(x)) {
    stop("read_cohort_data(): file not found: ", x, call. = FALSE)
  }
  if (!is.null(cols) && !is.character(cols)) {
    stop("read_cohort_data(): `cols` must be NULL or a character vector.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(x))
  if (ext == "parquet") {
    if (!is.null(cols)) {
      have <- names(arrow::open_dataset(x, format = "parquet")$schema)
      .checkCohortCols(cols, have, x)
    }
    out <- if (is.null(cols)) {
      arrow::read_parquet(x)
    } else {
      arrow::read_parquet(x, col_select = dplyr::all_of(cols))
    }
  } else if (ext == "qs2") {
    rlang::check_installed("qs2", reason = "to read `.qs2` cohort data.")
    out <- qs2::qs_read(x)
    if (!is.null(cols)) {
      .checkCohortCols(cols, names(out), x)
      out <- out[, cols, with = FALSE]
    }
  } else {
    stop("read_cohort_data(): `x` must be a `.parquet` or `.qs2` file: ", x, call. = FALSE)
  }
  out <- data.table::as.data.table(out)
  out <- data.table::setalloccol(out)
  out
}

.checkCohortCols <- function(cols, have, x) {
  absent <- setdiff(cols, have)
  if (length(absent)) {
    stop(
      "read_cohort_data(): column(s) not in ",
      x,
      ": ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(NULL)
}
