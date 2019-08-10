
#
 function(..., key = NULL, index, regular = TRUE, .drop = TRUE) {
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


 build_navtibble <- function (x, key = NULL, key_data = NULL, index, index2 = index,
           ordered = NULL, interval = TRUE, validate = TRUE, .drop = key_drop_default(x))
 {
   is_key_data <- !is_null(key_data)
   if (is_key_data) {
     assert_key_data(key_data)
     key <- head(names(key_data), -1L)
   }
   key_sym <- use_id(x, !!enquo(key))
   key_vars <- syms(unname(key_sym))
   tbl <- as_tibble(x)
   qindex <- enquo(index)
   index <- validate_index(tbl, !!qindex)
   index2 <- enquo(index2)
   if (identical(qindex, index2)) {
     index2 <- index
   }
   else {
     index2 <- validate_index(tbl, !!index2)
   }
   idx_chr <- c(index, index2)
   is_index_in_keys <- intersect(idx_chr, key_vars)
   if (is_false(is_empty(is_index_in_keys))) {
     abort(sprintf("Column `%s` can't be both index and key.",
                   idx_chr[[1]]))
   }
   if (NROW(tbl) == 0 || is_null(ordered)) {
     tbl <- arrange(tbl, !!!key_vars, !!sym(index))
     ordered <- TRUE
   }
   if (!is_key_data) {
     key_data <- group_data(group_by(tbl, !!!key_vars, .drop = .drop))
   }
   if (is_false(ordered)) {
     indices <- tbl[[index]]
     idx_rows <- lapply(key_data[[".rows"]], function(x) indices[x])
     actually_ordered <- all(vapply(idx_rows, validate_order,
                                    logical(1)))
     if (is_false(actually_ordered)) {
       idx_txt <- backticks(index)
       key_txt <- lapply(key_vars, expr_label)
       warn(sprintf(paste_inline("Unspecified temporal ordering may yield unexpected results.",
                                 "Suggest to sort by %s first."), comma(c(key_txt,
                                                                          idx_txt), sep = "")))
     }
     ordered <- actually_ordered
   }
   if (validate) {
     tbl <- validate_tsibble(data = tbl, key = key_vars, key_data = key_data,
                             index = index)
   }
   build_tsibble_meta(tbl, key_data = key_data, index = index,
                      index2 = index2, ordered = ordered, interval = interval)
 }
