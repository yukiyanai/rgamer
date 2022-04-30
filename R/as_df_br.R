#' @title Create a data frame of best responses.
#' @description \code{as_df_br()} creates a list of data frames of each player's
#'      best responses in a given parameter space.
#' @return A list containing two data frames, each of which is a player's best
#'     response to the other.
#' @param players A character vector specifying the two players of the game.
#' @param payoffs1 An R function describing Player 1's payoff.
#' @param payoffs2 An R function describing Player 2's payoff.
#' @param pars A character vector of length 2, which specifies the parameters
#'     each player chooses.
#' @param par1_lim A numerical vector of length 2, which specifies the range of
#'     the first parameter.
#' @param par2_lim A numerical vector of length 2, which specifies the range of
#'     the second parameter.
#' @param cons1 A named list of parameters contained in \code{payoffs1} that
#'     should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{payoffs2} that
#'     should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in payoffs1 and
#'     payoffs2 that should be treated as constants, if any. If \code{cons1} and
#'      \code{cons2} are exactly same, you can specify \code{cons_common}
#'      instead of \code{cons1} and \code{cons2}.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
as_df_br <- function(players,
                     payoffs1,
                     payoffs2,
                     pars,
                     par1_lim,
                     par2_lim,
                     cons1 = NULL,
                     cons2 = NULL,
                     cons_common = NULL) {

  payoff1 <- payoff2 <- x <- y <- NULL

  range1 <- par1_lim[2] - par1_lim[1]
  range2 <- par2_lim[2] - par2_lim[1]
  grid1 <- 10^(ceiling(log10(range1)) - 3)
  grid2 <- 10^(ceiling(log10(range2)) - 3)
  par1_vec <- seq(from = par1_lim[1], to = par1_lim[2], by = grid1)
  par2_vec <- seq(from = par2_lim[1], to = par2_lim[2], by = grid2)

  df0 <- expand.grid(par1_vec, par2_vec)
  names(df0) <- pars

  if (!is.null(cons1) & !is.null(cons2)) {

    df0b <- df0
    cons_names1 <- names(cons1)
    cons_length1 <- length(cons1)
    cons_names2 <- names(cons2)
    cons_length2 <- length(cons2)
    for (i in 1:cons_length1) {
      df0[, i + 2] <- cons1[[i]]
    }
    for (i in 1:cons_length2) {
      df0b[, i + 2] <- cons2[[i]]
    }
    names(df0) <- c(pars, cons_names1)
    names(df0b) <- c(pars, cons_names2)

  } else if (!is.null(cons_common)) {

    cons_names <- names(cons_common)
    cons_length <- length(cons_common)
    for (i in 1:cons_length) {
      df0[, i + 2] <- cons_common[[i]]
    }
    names(df0) <- c(pars, cons_names)
    df0b <- df0

  } else if (!is.null(cons1)) {

    df0b <- df0
    cons_names1 <- names(cons1)
    cons_length1 <- length(cons1)
    for (i in 1:cons_length1) {
      df0[, i + 2] <- cons1[[i]]
    }
    names(df0) <- c(pars, cons_names1)

  } else if (!is.null(cons2)) {

    df0b <- df0
    cons_names2 <- names(cons2)
    cons_length2 <- length(cons2)
    for (i in 1:cons_length2) {
      df0b[, i + 2] <- cons2[[i]]
    }
    names(df0b) <- c(pars, cons_names2)

  } else {

    df0b <- df0

  }

  df0$payoff1 <- df0 |>
    purrr::pmap(.f = payoffs1) |>
    unlist()
  df0$payoff2 <- df0b |>
    purrr::pmap(.f = payoffs2) |>
    unlist()

  df0 <- df0 |>
    dplyr::select(tidyselect::all_of(pars), payoff1, payoff2) |>
    dplyr::filter(!is.nan(payoff1), !is.nan(payoff2))
  names(df0)[1:2] <- c("x", "y")

  df1 <- df0 |>
    dplyr::group_by(y) |>
    dplyr::summarize(x = x[which.max(payoff1)],
                     payoff = max(payoff1),
                     .groups = "drop") |>
    dplyr::mutate(player = players[1])

  df2 <- df0 |>
    dplyr::group_by(x) |>
    dplyr::summarize(y = y[which.max(payoff2)],
                     payoff = max(payoff2),
                     .groups = "drop") |>
    dplyr::mutate(player = players[2])

  return(list(df1 = df1, df2 = df2))
}
