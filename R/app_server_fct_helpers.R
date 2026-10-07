#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)


#' Synchronize removals with raw data row count
#'
#' @description Resize a removals data frame to match the row count of the raw
#' data, preserving existing columns when possible.
#'
#' @param raw_df A data frame containing the current raw dataset.
#' @param removals_df An optional data frame of removal flags to preserve and
#'   resize.
#'
#' @return A data frame with the same number of rows as `raw_df`.
#'
#' @noRd
sync_removals <- function(raw_df, removals_df = NULL) {
  req_rows <- nrow(raw_df)

  if (is.null(removals_df) || !is.data.frame(removals_df)) {
    return(as.data.frame(matrix(FALSE, nrow = req_rows, ncol = 0)))
  }

  old_names <- names(removals_df)

  new_removals <- as.data.frame(matrix(
    FALSE,
    nrow = req_rows,
    ncol = ncol(removals_df)
  ))
  names(new_removals) <- old_names

  if (nrow(removals_df) > 0 && ncol(removals_df) > 0) {
    n_copy <- min(nrow(removals_df), req_rows)
    new_removals[seq_len(n_copy), seq_len(ncol(removals_df))] <- removals_df[
      seq_len(n_copy),
      seq_len(ncol(removals_df)),
      drop = FALSE
    ]
  }

  new_removals
}
