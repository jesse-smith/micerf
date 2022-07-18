init_imp = function(imp, m, data, where, seed = NULL) {
  if (test_imp(imp, m = m, data = data, where = where)) return(imp)
  col_names = colnames(data)
  names(col_names) = col_names
  m_names = as.character(seq_len(m))

  old_seed = .Random.seed
  on.exit({.Random.seed = old_seed}, add = TRUE)

  imp = lapply(col_names, function(nm) {
    nm = as.vector(nm)
    w = where[,nm]
    x = data[!w, ..nm][[1L]]
    if (sum(w) == 0L || length(x) == 0L) {
      dt = data.table()
      for (m_nm in m_names) {
        set(dt, j = m_nm, value = x[0L])
      }
    } else {
      set.seed(seed)
      dt = as.data.table(matrix(
        sample(x, size = sum(w) * m, replace = TRUE),
        nrow = sum(w),
        ncol = m,
        dimnames = list(NULL, m_names)
      ))
    }

    return(dt)
  })

  return(imp)
}


test_imp = function(x, m, data, where) {
  assert_count(m, positive = TRUE)
  assert_data(data)
  assert_where(where, data = data)

  true = test_list(
    x,
    types = "data.table",
    any.missing = FALSE,
    len = NCOL(data)
  )
  if (true) true = identical(names(x), colnames(data))
  if (!true) return(FALSE)

  m_names = as.character(seq_len(m))
  for (nm in colnames(data)) {
    true = test_data_table(
      x[[nm]],
      any.missing = FALSE,
      nrows = sum(where[,nm]),
      ncols = m
    )
    if (true) true = identical(names(x[[nm]]), m_names)
    if (!true) return(FALSE)
  }
  return(true)
}
