#' @title Search backward-induction equilibria numerically
#' @description \code{gridsearch_backward()} numerically finds
#'     backward-induction equilibrium outcomes in a leader-follower game with
#'     two players by grid search.
#' @return A data frame containing the subgame perfect equilibrium outcomes
#'     and its payoffs.
#' @param f1 An R function of the leaders' payoff function.
#' @param f2 An R function of the follower's payoff function.
#' @param x_vec The vector containing the grid of x, which represents
#'     the leader's choice.
#' @param y_vec The vector containing the grid of y, which represents
#'     the follower's choice.
#' @param pars The vector of parameter names if the game's type is "function."
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
gridsearch_backward <- function(f1, f2, x_vec, y_vec, pars = NULL) {

  x <- y <- p1 <- p2 <- NULL

  bigd <- expand.grid(x = x_vec,
                      y = y_vec) |>
    dplyr::arrange(x)

  if (!is.null(pars)) {
    names(bigd) <- pars
  }

  bigd$p2 <- bigd |>
    purrr::pmap(.f = f2) |>
    unlist()

  if (!is.null(pars)) {
    names(bigd)[1:2] <- c("x", "y")
  }

  best_p2 <- data.frame(
    x = rep(NA, length(x_vec)),
    y = rep(NA, length(x_vec)),
    p2 = rep(NA, length(x_vec)))

  for (i in seq_along(x_vec)) {
    smld <- bigd |>
      dplyr::filter(x == x_vec[i])
    p2_vals <- smld |>
      dplyr::pull(p2) |>
      unlist()
    ind <- which.max(p2_vals)
    best_p2$x[i] <- x_vec[i]
    best_p2$y[i] <- smld$y[ind]
    best_p2$p2[i] <- smld$p2[ind]
  }

  if (!is.null(pars)) {
    names(best_p2)[1:2] <- pars
  }

  best_p2$p1 <- best_p2 |>
    dplyr::select(-p2) |>
    purrr::pmap(.f = f1) |>
    unlist()

  if (!is.null(pars)) {
    names(best_p2)[1:2] <- c("x", "y")
  }

  best_p2[which.max(best_p2$p1),] |>
    dplyr::select(x, y, p1, p2)
}
