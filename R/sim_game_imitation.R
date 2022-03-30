#' @title Play a normal-form game by simulation (imitation)
#' @description \code{sim_game_imitation()} simulates plays expected in a
#'     normal-form game.
#' @details Simulate plays expected in a normal-form game defined by
#'     \code{normal_form()} when each player imitates the other player's
#'     previous action.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param n_periods A positive integer specifying how many times the game is
#'     played within each sample.
#' @param init1 Player 1's first strategy. If not specified, a strategy is
#'     randomly selected from the player's strategy set.
#' @param init2 Player 2's first strategy. If not specified, a strategy is
#'     randomly selected from the player's strategy set.
#' @param omega A numeric value in [0, 1] to control the degree of inertia in
#'     each player's behavior. If \code{omega = 1}, each player does not change
#'     their choices over time. If \code{omega = 0}, each player does not stick
#'     to their previous choice at all.
#' @param eta A numeric value in [0, 1] to control the degree of randomness in
#'     each player's behavior. If \code{eta = 1}, each player chooses their
#'     strategy completely at random. If \code{eta = 0}, each player chooses the
#'     best strategy based on the opponent's behavior in the previous period.
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @return data.frame containing the history of the game played.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @importFrom magrittr %>%
#' @noRd
sim_game_imitation <- function(game,
                               n_periods,
                               init1 = NULL,
                               init2 = NULL,
                               omega = 0,
                               eta = 0.1,
                               cons1 = NULL,
                               cons2 = NULL) {

  play1 <- rep(NA, n_periods)
  play2 <- rep(NA, n_periods)

  if (game$type == "matrix") {

    s1 <- game$strategy$s1
    s2 <- game$strategy$s2

    # for the first round
    if (is.null(init1)) play1[1] <- sample(s1, size = 1)
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- sample(s2, size = 1)
    else play2[1] <- init2

    for (i in 2:n_periods) {
      ## Player 1
      if (stats::runif(1) < omega) {
        play1[i] <- play1[i - 1]
      } else {
        df1 <- game$df %>%
          dplyr::filter(s1 == play1[i -1],
                        s2 == play2[i - 1])

        if (stats::runif(1) < eta) {
          play1[i] <- sample(s1, size = 1)
        } else {
          if (df1$payoff1[1] > df1$payoff2[1]) play1[i] <- play1[i - 1]
          else if (df1$payoff1[1] < df1$payoff2[1]) play1[i] <- play2[i - 1]
          else play1[i] <- sample(c(play1[i -1], play2[i - 1]), size = 1)
        }
      }

      ## Player 2
      if (stats::runif(1) < omega) {
        play2[i] <- play2[i - 1]
      } else {
        df2 <- game$df %>%
          dplyr::filter(s1 == play1[i - 1],
                        s2 == play2[i - 1])

        if (stats::runif(1) < eta) {
          play2[i] <- sample(s2, size = 1)
        } else {
          if (df2$payoff1[1] > df2$payoff2[1]) play2[i] <- play1[i - 1]
          else if (df2$payoff1[1] < df2$payoff2[1]) play2[i] <- play2[i - 1]
          else play2[i] <- sample(c(play1[i -1], play2[i - 1]), size = 1)
        }

      }
    }

  } else if (game$type == "char_function") {

    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    if (is.null(init1)) play1[1] <- stats::runif(1, min = s1[1], max = s1[2])
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- stats::runif(1, min = s2[1], max = s2[2])
    else play2[1] <- init2

    for (i in 2:n_periods) {
      f1 <- game$payoff$payoffs1 %>%
        stringr::str_replace(game$pars[2],
                             as.character(play2[i - 1])) %>%
        stringr::str_replace_all(game$pars[1], "XXX") %>%
        str2expression()
      f2 <- game$payoff$payoffs2 %>%
        stringr::str_replace(game$pars[1],
                             as.character(play1[i - 1])) %>%
        stringr::str_replace_all(game$pars[2], "YYY") %>%
        str2expression()

      pp1 <- eval(f1, envir = list(XXX = play1[i - 1]))
      pp2 <- eval(f2, envir = list(YYY = play2[i - 1]))

      ## Player 1
      if (stats::runif(1) < omega) {
        play1[i] <- play1[i - 1]
      } else {
        if (stats::runif(1) < eta) {
          play1[i] <- stats::runif(1, min = s1[1], max = s1[2])
        } else {
          if (pp1 > pp2) play1[i] <- play1[i - 1]
          else if (pp1 < pp2) play1[i] <- play2[i - 1]
          else play1[i] <- mean(c(play1[i - 1], play2[i - 1]))
        }
      }

      ## Player 2
      if (stats::runif(1) < omega) {
        play2[i] <- play2[i - 1]
      } else {
        if (stats::runif(1) < eta) {
          play2[i] <- stats::runif(1, min = s2[1], max = s2[2])
        } else {
          if (pp1 > pp2) play2[i] <- play1[i - 1]
          else if (pp1 < pp2) play2[i] <- play2[i - 1]
          else play2[i] <- mean(c(play1[i - 1], play2[i - 1]))
        }
      }
    }

  } else {

    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    if (is.null(init1)) play1[1] <- stats::runif(1, min = s1[1], max = s1[2])
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- stats::runif(1, min = s2[1], max = s2[2])
    else play2[1] <- init2

    for (i in 2:n_periods) {

      f1 <- function(XXX) {
        if (is.null(cons1)) {
          arg_list <- list(XXX, play2[i - 1])
          names(arg_list) <- game$pars
        } else {
          arg_list <- c(cons1, XXX, play2[i - 1])
          names(arg_list) <- c(names(cons1), game$pars)
        }
        purrr::pmap(.l = arg_list,
                    .f = game$payoff$payoffs1)
      }

      f2 <- function(YYY) {
        if (is.null(cons2)) {
          arg_list <- list(play1[i - 1], YYY)
          names(arg_list) <- game$pars
        } else {
          arg_list <- c(cons2, play1[i - 1], YYY)
          names(arg_list) <- c(names(cons2), game$pars)
        }
        purrr::pmap(.l = arg_list,
                    .f = game$payoff$payoffs2)
      }

      pp1 <- unlist(f1(play1[i - 1]))
      pp2 <- unlist(f2(play2[i - 1]))

      ## Player 1
      if (stats::runif(1) < omega) {
        play1[i] <- play1[i - 1]
      } else {
        if (stats::runif(1) < eta) {
          play1[i] <- stats::runif(1, min = s1[1], max = s1[2])
        } else {
          if (pp1 > pp2) play1[i] <- play1[i - 1]
          else if (pp1 < pp2) play1[i] <- play2[i - 1]
          else play1[i] <- mean(c(play1[i - 1], play2[i - 1]))
        }
      }

      ## Player 2
      if (stats::runif(1) < omega) {
        play2[i] <- play2[i - 1]
      } else {
        if (stats::runif(1) < eta) {
          play2[i] <- stats::runif(1, min = s2[1], max = s2[2])
        } else {
          if (pp1 > pp2) play2[i] <- play1[i - 1]
          else if (pp1 < pp2) play2[i] <- play2[i - 1]
          else play2[i] <- mean(c(play1[i - 1], play2[i - 1]))
        }
      }
    }
  }

  return(data.frame(play1 = play1,
                    play2 = play2,
                    period = 1:n_periods))
}
