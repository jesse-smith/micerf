make_where = function(data, where = c("missing", "observed", "all")) {
  where = match.arg(where)
  data = as_data(data)

  switch(
    where,
    "missing" = is.na(data),
    "observed" = !is.na(data),
    "all" = matrix(
      TRUE,
      nrow = NROW(data),
      ncol = NCOL(data),
      dimnames = list(rownames(data), colnames(data))
    )
  )
}


as_where = function(x, data = NULL) {
  if (!(is.logical(x) && is.matrix(x))) {
    x = as.matrix(x)
    x = matrix(
      as.logical(x),
      nrow = NROW(x),
      ncol = NCOL(x),
      dimnames = dimnames(x)
    )
  }
  assert_where(x, data = data)
  x
}


assert_where = function(x, data = NULL) {
  if (!is.null(data)) data = as_data(data)
  assert_matrix(
    x,
    mode = "logical",
    any.missing = FALSE,
    nrows = nrow(data),
    ncols = ncol(data)
  )
}
