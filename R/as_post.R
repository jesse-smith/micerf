as_post = function(x, mice_rf = NULL) {
  x = as.list(x)
  is_null = vapply(x, is.null, logical(1L))
  is_na = is.na(x)
  x = x[!(is_null | is_na)]
  if (NROW(x) == 0L) return(NULL)

  x = lapply(x, function(expr) {
    if (test_formula(expr)) {
      e = tail(paste0(expr), 1L)
    }
    if (test_character(expr)) {
      e = str2expression(expr)
    }
    return(as.expression(expr))
  })

  assert_post(x, mice_rf = mice_rf)
}


assert_post = function(x, mice_rf = NULL) {
  if (is.null(x)) return(invisible(x))
  if (!is.null(mice_rf)) assertMiceRF(mice_rf)

  assert_list(x, types = "expression", any.missing = FALSE, names = "unique")
  assert_names(x, subset.of = colnames(mice_rf$data))
}
