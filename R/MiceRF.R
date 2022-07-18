#' `{R6}` Class Containing the State of a MICE Model + Data Set
#'
#' `MiceRF` is an `R6` class implementing Multivariate Imputation using Chained
#' Equations with Random Forests. The structure is similar to the `{mice}`
#' package's `mids` object, but follows an object-oriented programming
#' paradigm. For a functional implementation, see \code{\link[mice_rf]}.
MiceRF = R6::R6Class(
  "MiceRF",
  public = list(
    #' @description
    #' Initialize a `MiceRF` object
    #' @inheritParams mice_rf
    #' @return A new `Mids` object
    initialize = function(
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
      rhat_it = it_step,
      rhat_thresh = 1.05,
      chunk_size = 1L,
      seed = NULL,
      ...
    ) {
      private$.iteration = 0L
      self$data = data
      self$m = m
      self$predictor_matrix = predictor_matrix
      self$where = where
      self$monotone = monotone
      self$it_max = it_max
      self$it_step = it_step
      self$rhat_it = rhat_it
      self$rhat_thresh = rhat_thresh
      self$chunk_size = chunk_size
      self$seed = seed
      self$post = post
      self$imp
      return(invisible(self))
    },
    rhat = function(
      iters = NULL,
      params = c("mean", "var", "error")
    ) {
      params = match.arg(params, several.ok = TRUE)
      if (!is.null(iters)) {
        iters = asInteger(
          iters,
          lower = 0L,
          upper = self$iteration,
          min.len = 0L,
          max.len = self$iteration
        )
      }

    }
  ),
  active = list(
    #' @field data `[data.table]` The data to impute. Can only be modified
    #'   (by reference or assignment) while `self$iteration == 0`.
    data = function(value) {
      if (missing(value)) {
        if (private$.iteration == 0L) {
          return(private$.data)
        } else {
          return(copy(private$.data))
        }
      }
      if (private$.iteration > 0L) {
        stop("self$data is read-only once self$iteration > 0")
      }
      private$.data = as_data(value)
    },
    #' @field imp `[list(data.table)]` A list containing the current imputed
    #'   values for each variable and chain. Read-only.
    imp = function(value) {
      if (missing(value)) {
        private$.imp = init_imp(
          private$.imp,
          m = private$.m,
          data = private$.data,
          where = private$.where,
          seed = private$.seed
        )
        return(private$.imp)
      }
      stop("self$imp is read-only")
    },
    #' @field m `[integer(1)]` The number of chains (imputed data sets)
    #'   to create. Can only be modified while `self$iteration == 0`.
    m = function(value) {
      if (missing(value)) return(private$.m)
      if (private$.iteration > 0L) {
        stop("self$m is read-only once self$iteration > 0")
      }
      private$.m = as.vector(asCount(value, positive = TRUE))
    },
    #' @field predictor_matrix `[matrix(logical)]` A matrix with rows
    #'   representing response variables (to impute) and columns representing
    #'   predictor variables (to use in imputation). If assigning,
    #'   must be a square matrix with the same number of rows and columns as
    #'   variables in `self$data`, and row/column ordering must match column
    #'   order in `self$data`.
    predictor_matrix = function(value) {
      if (missing(value)) return(private$.predictor_matrix)
      private$.predictor_matrix = as_predictor_matrix(value, data = private$.data)
    },
    #' @field where `[matrix(logical)]` A matrix of the same dimensions as
    #'   `self$data` indicating what values to impute (`TRUE`) or ignore
    #'   (`FALSE`). Can only be modified while `self$iteration == 0`.
    where = function(value) {
      if (missing(value)) return(private$.where)
      if (private$.iteration > 0L) {
        stop("self$where is read-only once self$iteration > 0")
      }
      private$.where = as_where(value, data = private$.data)
    },
    #' @field monotone `[logical(1)]` A boolean indicating whether imputation
    #'   should occur monotonically (in increasing order of missingness)
    #'   regardless of the ordering of the data.
    monotone = function(value) {
      if (missing(value)) return(private$.monotone)
      private$.monotone = as.vector(assert_logical(
        value,
        any.missing = FALSE,
        len = 1L
      ))
    },
    #' @field post `[named list(expression)]` A named list of expressions that
    #'   are applied to each variable by name immediately after it is imputed.
    #'   These expressions are essentially function bodies. They can access
    #'   public fields of the `MiceRF` by name
    #'   (excluding `self$imp` and `self$post`), as well as each variable
    #'   (with the current imputed values in that chain) by name.
    #'   No other objects are accessible to the expressions.
    #'   Each expression should return an object of the same type as the
    #'   named variable; the length of the object should either be `1`
    #'   (which is recycled), `NROW(self$imp[[variable]])`,
    #'   or `NROW(self$data)`. Only imputed values of the named variable are
    #'   modified. `post` can also be `NULL`, which is the default.
    post = function(value) {
      if (missing(value)) return(private$.post)
      private$.post = as_post(value, mice_rf = mice_rf)
    },
    #' @field iteration `[integer(1)]` The number of iterations performed. Can
    #'   only be assigned a value greater than or equal to the current value.
    iteration = function(value) {
      if (missing(value)) return(private$.iteration)
      private$.iteration = as.vector(asInt(value, lower = private$.iteration))
    },
    #' @field it_max `[integer(1)]` The maximum number of iterations to perform.
    #'   Can only be assigned a value greater than or equal to the current value
    #'   of `self$iteration`.
    it_max = function(value) {
      if (missing(value)) return(private$.it_max)
      private$.it_max = asInt(value, lower = self$iteration)
    },
    #' @field it_max `[integer(1)]` The number of iterations to perform in a
    #'   chunk. Iterating within chunks reduces the number of times data must
    #'   be transferred between processes in a parallel setup; this can be
    #'   particularly beneficial when running on a cluster. However, convergence
    #'   will only be checked every `self$it_step` iterations, so higher values
    #'   can increase the number of iterations performed. Must be at least `1`.
    it_step = function(value) {
      if (missing(value)) return(private$.it_step)
      private$.it_step = asCount(value, positive = TRUE)
    },
    #' @field rhat_it `[integer(1)]` The number of recent iterations to
    #'   calculate R-hat statistics for when checking for convergence. Larger
    #'   values are less likely to yield false positives but can also greatly
    #'   increase the number of iterations needed for convergence.
    #'   Must be at least `1`.
    rhat_it = function(value) {
      if (missing(value)) return(private$.rhat_it)
      private$.rhat_it = asCount(value, positive = TRUE)
    },
    #' @field rhat_thresh `[double(1)]` The R-hat threshold used for convergence
    #'   checks. R-hat values for the most recent `self$rhat_it` iterations must
    #'   be less than this value for all variables and chain parameters.
    rhat_thresh = function(value) {
      if (missing(value)) return(private$.rhat_thresh)
      private$.rhat_thresh = as.double(assert_number(value, lower = 0))
    },
    #' @field chunk_size `[integer(1)]` The average number of chains to assign
    #'   to a `future`. Can only be modified while `self$iteration == 0`.
    chunk_size = function(value) {
      if (missing(value)) return(private$.chunk_size)
      if (private$.iteration > 0L) {
        stop("self$chunk_size is read-only once self$iteration > 0")
      }
      private$.chunk_size = as.vector(asCount(value, positive = TRUE))
    },
    #' @field seed `[integer(1)]` Seed for random number generation.
    #'   Can be `NULL`.
    seed = function(value) {
      if (missing(value)) return(private$.seed)
      private$.seed = if (is.null(value)) NULL else as.vector(asInt(value))
    },
    #' @field chain_mean `[array(double)]` A 3D array containing means for each
    #'   variable, chain, and iteration. Un-ordered categorical variables are
    #'   first one-hot-encoded.
    chain_mean = function(value) {
      if (missing(value)) return(private$.chain_mean)
      stop("self$chain_mean is read-only")
    },
    #' @field chain_var `[array(double)]` A 3D array containing variances for
    #'   each variable, chain, and iteration. Un-ordered categorical variables
    #'   are first one-hot-encoded.
    chain_var = function(value) {
      if (missing(value)) return(private$.chain_var)
      stop("self$chain_var is read-only")
    },
    #' @field chain_error `[array(double)]` A 3D array containing out-of-bag
    #'   prediction error for each variable, chain, and iteration. For
    #'   classification, this is the fraction of miss-classified samples; for
    #'   regression, the mean squared error; for survival, one minus Harrell's
    #'   C-index.
    chain_error = function(value) {
      if (missing(value)) return(private$.chain_error)
      stop("self$chain_error is read-only")
    },
    #' @field rhat_max `[double(1)]` The maximum R-hat statistic for the
    #'   `self$rhat_it` most recent iterations across all imputed variables and
    #'   chain parameters. `NA_real_` if any of the R-hat statistics are missing
    #'   values.
    rhat_max = function(value) {
      if (missing(value)) {
        return(max(
          private$.chain_mean,
          private$.chain_var,
          private$.chain_error
        ))
      }
      stop("self$rhat is read-only")
    }
  ),
  private = list(
    .data = NULL,
    .imp = NULL,
    .m = NULL,
    .predictor_matrix = NULL,
    .where = NULL,
    .monotone = NULL,
    .post = NULL,
    .iteration = NULL,
    .it_max = NULL,
    .it_step = NULL,
    .rhat_it = NULL,
    .rhat_thresh = NULL,
    .chunk_size = NULL,
    .seed = NULL,
    .last_seed_value = NULL,
    .chain_mean = NULL,
    .chain_var = NULL,
    .chain_error = NULL,
    .rhat_mean = NULL,
    .rhat_var = NULL,
    .rhat_error = NULL
  )
)


assertMiceRF = function(x) {
  assertR6(
    x,
    classes = "MiceRF",
    public = c(
      names(MiceRF$public_methods),
      names(MiceRF$public_fields),
      names(MiceRF$active)
    ),
    private = c(names(MiceRF$private_methods), names(MiceRF$private_fields))
  )
}
