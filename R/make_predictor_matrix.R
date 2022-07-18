make_predictor_matrix = function(data) {
  if (R6::is.R6(data) && inherits(data, "Mids")) {
    return(data$predictor_matrix)
  }
  data = as_data(data)
  predictor_matrix = matrix(
    TRUE,
    nrow = NCOL(data),
    ncol = NCOL(data),
    dimnames = list(colnames(data), colnames(data))
  )
  diag(predictor_matrix) = FALSE
  return(predictor_matrix)
}


as_predictor_matrix = function(x, data = NULL) {
  if (!(is.logical(x) && is.matrix(x))) {
    x = as.matrix(x)
    x = matrix(
      as.logical(x),
      nrow = NROW(x),
      ncol = NCOL(x),
      dimnames = dimnames(x)
    )
  }
  if (!is.null(data)) {
    data = as_data(data)
    dimnames(x) = list(colnames(data), colnames(data))
  }
  assert_predictor_matrix(x, data = data)
  x
}


assert_predictor_matrix = function(x, data = NULL) {
  if (!is.null(data)) assert_data(data)
  assert_matrix(
    x,
    mode = "logical",
    any.missing = FALSE,
    nrows = ncol(data),
    ncols = ncol(data)
  )
  if (any(diag(x))) {
    stop("Predictor matrix contains variables that are their own predictors")
  }
  return(invisible(x))
}
