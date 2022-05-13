#' @title Play a normal-form game by simulation (alternate best response)
#' @description \code{sim_game_abr()} simulates plays expected in a normal-form
#'    game.
#' @details Simulate plays expected in a normal-form game defined by
#'   \code{normal_form()} when each player alternately moves and each chose the
#'   best response to the opponent's previous move.
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
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @return data.frame containing the history of the game played.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
sim_game_abr <- function(game,
                         n_periods,
                         init1 = NULL,
                         init2 = NULL,
                         omega = 0,
                         cons1 = NULL,
                         cons2 = NULL) {

  payoff1 <- payoff2 <- NULL

  np2 <- n_periods * 2

  play1 <- rep(NA, np2)
  play2 <- rep(NA, np2)

  if (game$type == "matrix") {

    s1 <- game$strategy$s1
    s2 <- game$strategy$s2

    n1 <- length(s1)
    n2 <- length(s2)

    pi1 <- game$mat$matrix1
    pi2 <- game$mat$matrix2

    # for the first round
    if (is.null(init1)) {
      play1[1] <- sample(1:n1, size = 1)
    } else {
      play1[1] <- which(s1 == init1)
    }
    if (is.null(init2)) {
      play2[1] <- sample(1:n2, size = 1)
    } else {
      play2[1] <- which(s2 == init2)
    }

    for (t in 2:np2) {
      if (t %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < omega) {
          play1[t] <- play1[t - 1]
        } else {
          pi1t <- pi1[, play2[t -1]]
          pi1t <- pi1t == max(pi1t)
          if (sum(pi1t) == 1) {
            play1[t] <- which(pi1t)
          } else {
            play1[t] <- sample(which(pi1t), size = 1)
          }
        }
        ## Player 2
        play2[t] <- play2[t - 1]
      } else {
        ## Player 1
        play1[t] <- play1[t - 1]
        ## Player 2
        if (stats::runif(1) < omega) {
          play2[t] <- play2[t - 1]
        } else {
          pi2t <- pi2[play1[t -1], ]
          pi2t <- pi2t == max(pi2t)
          if (sum(pi2t) == 1) {
            play2[t] <- which(pi2t)
          } else {
            play2[t] <- sample(which(pi2t), size = 1)
          }
        }
      }
    }
    play1 <- s1[play1]
    play2 <- s2[play2]
    play2[1] <- NA

  } else if (game$type == "char_function") {
    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    if (is.null(init1)) play1[1] <- stats::runif(1, min = s1[1], max = s1[2])
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- stats::runif(1, min = s2[1], max = s2[2])
    else play2[1] <- init2

    for (i in 2:np2) {
      if (i %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < omega) {
          play1[i] <- play1[i - 1]
        } else {
          f1 <- game$payoff$payoffs1 |>
            stringr::str_replace(game$pars[2],
                                 as.character(play2[i - 1])) |>
            stringr::str_replace_all(game$pars[1], "XXX") |>
            str2expression()
          fd1 <- function(x) {
            eval(stats::D(f1, name = "XXX"),
                 envir = list(XXX = x))
          }
          play1[i] <- try(stats::uniroot(fd1, interval = s1)$root,
                          silent = TRUE)
          if (methods::is(play1[i], "character")) play1[i] <- play1[i - 1]
          play1 <- as.numeric(play1)
        }
        ## Player 2
        play2[i] <- play2[i - 1]
      } else {
        ## Player 1
        play1[i] <- play1[i - 1]
        ## Player 2
        if (stats::runif(1) < omega) {
          play2[i] <- play2[i - 1]
        } else {
          f2 <- game$payoff$payoffs2 |>
            stringr::str_replace(game$pars[1],
                                 as.character(play1[i - 1])) |>
            stringr::str_replace_all(game$pars[2], "YYY") |>
            str2expression()
          fd2 <- function(y) {
            eval(stats::D(f2, name = "YYY"),
                 envir = list(YYY = y))
          }
          play2[i] <- try(stats::uniroot(fd2, interval = s2)$root,
                          silent = TRUE)
          if (methods::is(play2[i], "character")) play2[i] <- play2[i - 1]
          play2 <- as.numeric(play2)
        }
      }
    }
    play2[1] <- NA

  } else {
    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    if (is.null(init1)) play1[1] <- stats::runif(1, min = s1[1], max = s1[2])
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- stats::runif(1, min = s2[1], max = s2[2])
    else play2[1] <- init2

    for (i in 2:np2) {
      if (i %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < omega) {
          play1[i] <- play1[i - 1]
        } else {
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
          play1[i] <- try(
            stats::optim(par = stats::median(s1),
                         fn = f1,
                         method = "L-BFGS-B",
                         lower = s1[1],
                         upper = s1[2],
                         control = list(fnscale = -1))$par,
            silent = TRUE)
          if (methods::is(play1[i], "try-error")) play1[i] <- play1[i - 1]
        }
        ## Player2
        play2[i] <- play2[i - 1]
      } else {
        ## Player 1
        play1[i] <- play1[i - 1]
        ## Player 2
        if (stats::runif(1) < omega) {
          play2[i] <- play2[i - 1]
        } else {
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
          play2[i] <- try(
            stats::optim(par = stats::median(s2),
                         fn = f2,
                         method = "L-BFGS-B",
                         lower = s2[1],
                         upper = s2[2],
                         control = list(fnscale = -1))$par,
            silent = TRUE)
          if (methods::is(play2[i], "try-error")) play2[i] <- play2[i - 1]
        }
      }
    }
    play2[1] <- NA
  }

  return(data.frame(play1 = play1,
                    play2 = play2,
                    period = 1:np2,
                    moved = rep(game$player, n_periods)))
}
