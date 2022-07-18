get_seed = function() {
  seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(seed)) {
    runif(1L)
    seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }
  return(seed)
}
