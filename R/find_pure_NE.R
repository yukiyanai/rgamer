#' @title Find pure-strategy Nash equilibria
#' @description \code{find_pure_NE()} finds pure-strategy Nash equilibria.
#' @return A character string showing the pure-strategy Nash equilibria.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
find_pure_NE <- function(game) {

  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  n_rows <- length(s1)
  n_cols <- length(s2)
  mat1 <- game$mat$matrix1
  mat2 <- game$mat$matrix2

  pureNE <- matrix(NA, nrow = n_rows, ncol = n_cols)
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      pureNE[i, j] <- (mat1[i, j] == max(mat1[, j]) & mat2[i, j] == max(mat2[i, ]))
    }
  }

  pureNE_index <- which(pureNE, arr.ind = TRUE)
  if (length(pureNE_index) == 0) {
    psNE <- NULL
  } else {
    psNE <- paste0("(", s1[pureNE_index[,1]], ", ", s2[pureNE_index[, 2]], ")")
  }

  return(psNE)
}
