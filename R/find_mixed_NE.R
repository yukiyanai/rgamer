#' @title Find a mixed-strategy Nash equilibrium.
#' @description \code{find_mixed_NE} finds a mixed-strategy Nash equilibrium of
#'     a normal-form game with discrete-choice strategies.
#' @return A list of the probabilities given to each strategy that specifies
#'     the mixed-strategy Nash equilibrium.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#'     The game's type must be "matrix".
#' @seealso \code{\link{normal_form}}
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
find_mixed_NE <- function(game) {

  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  p1 <- game$payoff[[1]]
  p2 <- game$payoff[[2]]
  n_rows <- length(s1)
  n_cols <- length(s2)

  mat1 <- game$mat$matrix1
  mat2 <- game$mat$matrix2

  a1 <- matrix(NA, nrow = n_cols, ncol = n_rows)
  b1 <- matrix(NA, nrow = n_cols, ncol = 1)
  a1[1, ] <- 1
  b1[1, 1] <- 1
  for (i in 2:n_cols) {
    a1[i, ] <- mat2[, (i - 1)] - mat2[, i]
    b1[i, 1] <- 0
  }
  prob1 <- tryCatch({
    solve(a1, b1)
  }, error = function(e) {
    NULL
  })

  a2 <- matrix(NA, nrow = n_rows, ncol = n_cols)
  b2 <- matrix(NA, nrow = n_rows, ncol = 1)
  a2[1, ] <- 1
  b2[1, 1] <- 1
  for (i in 2:n_rows) {
    a2[i, ] <- mat1[(i - 1), ] - mat1[i,]
    b2[i, 1] <- 0
  }
  prob2 <- tryCatch({
    solve(a2, b2)
  }, error = function(e) {
    NULL
  })


  if (is.null(prob1) | is.null(prob2)) {
    msNE <- NULL
  } else {
    prob1 <- as.vector(prob1)
    prob2 <- as.vector(prob2)

    if (isFALSE(all.equal(sum(prob1), 1)) | isFALSE(all.equal(sum(prob2), 1))) {
      warning("Obtained probabilities don't sum to 1. It might be a wrong answer.")
    }

    msNE <- list(s1 = prob1, s2 = prob2)
  }

  return(msNE)
}
