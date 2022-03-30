#' @title Find Nash equilibria of a sequential-form game with payoffs defined
#'     by R functions
#' @description \code{solve_nfg_fcn()} finds the pair of best responses when
#'     payoff functions are provided as R functions.
#' @return A list containing the pair of the best response correspondence (NE)
#'     and the plot of best response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso [normal_form()]
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in
#'     \code{game$payoff$payoffs1} and \code{game$payoff$payoffs2} that should
#'     be treated as constants, if any.  If \code{cons1} and \code{cons2} are
#'     exactly same, you can specify \code{cons_common} instead of specifying
#'     both \code{cons1} and \code{cons2}.
#' @param precision A natural number specifying the precision of numerical
#'     approximation. The value n approximately means that the approximation is
#'     correct up to the Nth decimal place. The default value is 1.
#' @param quietly A logical value to determine if the equilibrium will be kept
#'     in the returned list without being printed on screen. Default is
#'     \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_seq_fcn <- function(game,
                          cons1 = NULL,
                          cons2 = NULL,
                          cons_common = NULL,
                          precision = 1L,
                          quietly = FALSE) {

  pars <- NULL

  players <- game$player

  par1_lim <- game$strategy[[1]]
  par2_lim <- game$strategy[[2]]

  delta <- 10^(-(precision + 2))

  if (is.null(cons1) & is.null(cons2) & is.null(cons_common)) {
    if (length(game$constants) == 2) {
      cons1 <- game$constants[[1]]
      cons2 <- game$constants[[2]]
    } else {
      cons_common <- game$constants[[1]]
    }
  }

  if (!is.null(cons_common)) {
    cons1 <- cons_common
    cons2 <- cons_common
  }

  if (!is.null(cons1)) {
    ff1 <- function(...) {
      cons1 %>%
        as.data.frame() %>%
        purrr::pmap(.f = game$payoff$payoffs1, ...)
    }
  } else {
    ff1 <- game$payoff$payoffs1
  }

  if (!is.null(cons2)) {
    ff2 <- function(...) {
      cons2 %>%
        as.data.frame() %>%
        purrr::pmap(.f = game$payoff$payoffs2, ...)
    }
  } else {
    ff2 <- game$payoff$payoffs2
  }

  par1_seq <- seq(from = par1_lim[1],
                  to = par1_lim[2],
                  length.out = 50)
  par2_seq <- seq(from = par2_lim[1],
                  to = par2_lim[2],
                  length.out = 50)

  while (TRUE) {
    dif <- max(par1_seq[2] - par1_seq[1],
               par2_seq[2] - par2_seq[1])
    out <- gridsearch_backward(f1 = ff1,
                               f2 = ff2,
                               x_vec = par1_seq,
                               y_vec = par2_seq,
                               pars = pars)

    if (dif < delta) {
      break
    } else {
      x_bottom <- mean(c(par1_seq[1], out$x))
      x_top <- mean(c(par1_seq[50], out$x))
      par1_seq <- seq(from = x_bottom,
                      to = x_top,
                      length.out = 50)

      y_bottom <- mean(c(par2_seq[1], out$y))
      y_top <- mean(c(par2_seq[50], out$y))
      par2_seq <- seq(from = y_bottom,
                      to = y_top,
                      length.out = 50)
    }
  }

  x <- round(out$x, precision)
  y <- round(out$y, precision)

  df_out <- data.frame(x, y)
  names(df_out) <- pars

  payoff1 <- df_out %>%
    purrr::pmap(.f = ff1) %>%
    unlist()
  payoff2 <- df_out %>%
    purrr::pmap(.f = ff2) %>%
    unlist()

  NE <- paste0("(",
                format(x, nsmall = precision), ", ",
                format(y, nsmall = precision), ")")

  payoff = paste0("(",
                  payoff1, ", ",
                  payoff2, ")")

  if (!quietly) message("NE outcome: ", NE)

  message("The NE shown here were numerically obtained and can be slightly different from the analytical solution (if any).")

  return(list(NE = NE, payoff = payoff))
}
