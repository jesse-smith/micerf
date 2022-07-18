recursive_dependencies <- function(pkgs, available = available.packages()) {
  # Clean input
  pkgs <- gsub("[^a-zA-Z0-9.].*", "", trimws(pkgs))
  pkgs <- pkgs[!pkgs %in% c("R", "")]
  pkgs <- sort(unique(pkgs))

  # Restrict available to needed information
  available <- available[,c("Package", "Depends", "Imports", "LinkingTo")]

  # Copy input and restrict to available packages
  input <- pkgs
  pkgs <- intersect(pkgs, available[, "Package"])
  if (!identical(pkgs, input)) {
    if (length(pkgs) == 0L) stop("No input packages available")
    not_available <- paste0("'", setdiff(input, pkgs), "'", collapse = ",")
    warning(paste("Packages", not_available, "are not available"))
  }

  # Loop
  changing <- TRUE
  while(changing) {
    prev <- pkgs

    new <- unlist(strsplit(as.vector(available[pkgs,]), ","))
    new <- gsub("[^a-zA-Z0-9.].*", "", trimws(new))
    new <- new[!new %in% c("R", "")]
    new <- intersect(new, available[, "Package"])

    pkgs <- sort(unique(c(pkgs, new)))

    changing <- !identical(prev, pkgs)
  }
  pkgs
}


licenses <- function(pkgs, available = available.packages()) {
  pkgs <- recursive_dependencies(pkgs, available = available)
  sort(unique(available[pkgs, "License"]))
}
