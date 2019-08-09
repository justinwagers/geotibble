

tsibble <- function(..., key = NULL, index, regular = TRUE, .drop = TRUE) {
  stopifnot(is_logical(regular, n = 1))
  dots <- list2(...)
  if (has_length(dots, 1) && is.data.frame(dots[[1]])) {
    abort("Must not be a data frame, do you want `as_tsibble()`?")
  }
  tbl <- tibble(!!!dots)
  index <- enquo(index)
  build_tsibble(tbl,
                key = !!enquo(key), index = !!index, interval = regular,
                .drop = .drop
  )
}
