as_data = function(x) {
  x = as.data.table(x)
  assert_data(x)
  return(x)
}


assert_data = function(x) {
  assert_data_table(
    x,
    types = c("logical", "numeric", "character", "factor"),
    min.rows = 1L,
    min.cols = 1L,
    col.names = "unique"
  )
}
