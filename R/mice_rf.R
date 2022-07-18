mice_rf <- function(
  data,
  m = 5L,
  predictor_matrix = make_predictor_matrix(data),
  where = make_where(data),
  monotone = FALSE,
  post = NULL,
  num_trees = 100L,
  mtry_ratio = NULL,
  it_max = 50L,
  it_step = 5L,
  rhat_thresh = 1.05,
  rhat_it = it_step,
  chunk_size = 1L,
  seed = NULL,
  ...
) {

}
