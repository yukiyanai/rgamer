#' @title Play a normal-form game by simulation (alternate best response)
#' @description \code{simu_game_abr()} simulates plays expected in a normal-form
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
#' @param rho A numeric value in [0, 1] to control the degree of inertia in each
#'     player's behavior. If \code{rho = 1}, each player does not change their
#'     choices over time. If \code{rho = 0}, each player does not stick to
#'     their previous choice at all.
#' @param cons1 A named list of parameters contained in \code{game$payoff$p1}
#'     that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2}
#'     that should be treated as constants, if any.
#' @return data.frame containing the history of the game played.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
#' @importFrom magrittr %>%
simu_game_abr <- function(game,
                          n_periods,
                          init1 = NULL,
                          init2 = NULL,
                          rho   = 0,
                          cons1 = NULL,
                          cons2 = NULL) {

  p1 <- p2 <- NULL

  n2 <- n_periods * 2

  play1 <- rep(NA, n2)
  play2 <- rep(NA, n2)

  if (game$type == "matrix") {

    s1 <- game$strategy$s1
    s2 <- game$strategy$s2

    # for the first round
    if (is.null(init1)) play1[1] <- sample(s1, size = 1)
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- sample(s2, size = 1)
    else play2[1] <- init2

    for (i in 2:n2) {
      if (i %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < rho) {
          play1[i] <- play1[i - 1]
        } else {
          df1 <- game$df %>%
            dplyr::filter(s2 == play2[i - 1]) %>%
            dplyr::filter(p1 == max(p1))
          if (nrow(df1) > 1) df1 <- dplyr::slice_sample(df1, 1)
          play1[i] <- df1$s1[1]
        }
        ## Player 2
        play2[i] <- play2[i - 1]
      } else {
        ## Player 1
        play1[i] <- play1[i - 1]
        ## Player 2
        if (stats::runif(1) < rho) {
          play2[i] <- play2[i - 1]
        } else {
          df2 <- game$df %>%
            dplyr::filter(s1 == play1[i - 1]) %>%
            dplyr::filter(p2 == max(p2))
          if (nrow(df2) > 1) df2 <- dplyr::slice_sample(df2, 1)
          play2[i] <- df2$s2[1]
        }
      }
    }
    play2[1] <- NA

  } else if (game$type == "char_function") {
    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    if (is.null(init1)) play1[1] <- stats::runif(1, min = s1[1], max = s1[2])
    else play1[1] <- init1
    if (is.null(init2)) play2[1] <- stats::runif(1, min = s2[1], max = s2[2])
    else play2[1] <- init2

    for (i in 2:n2) {
      if (i %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < rho) {
          play1[i] <- play1[i - 1]
        } else {
          f1 <- game$payoff$p1 %>%
            stringr::str_replace(game$pars[2],
                                 as.character(play2[i - 1])) %>%
            stringr::str_replace_all(game$pars[1], "XXX") %>%
            str2expression()
          fd1 <- function(x) {
            eval(stats::D(f1, name = "XXX"),
                 envir = list(XXX = x))
          }
          play1[i] <- try(stats::uniroot(fd1, interval = s1)$root,
                          silent = TRUE)
          if (class(play1[i]) == "character") play1[i] <- play1[i - 1]
          play1 <- as.numeric(play1)
        }
        ## Player 2
        play2[i] <- play2[i - 1]
      } else {
        ## Player 1
        play1[i] <- play1[i - 1]
        ## Player 2
        if (stats::runif(1) < rho) {
          play2[i] <- play2[i - 1]
        } else {
          f2 <- game$payoff$p2 %>%
            stringr::str_replace(game$pars[1],
                                 as.character(play1[i - 1])) %>%
            stringr::str_replace_all(game$pars[2], "YYY") %>%
            str2expression()
          fd2 <- function(y) {
            eval(stats::D(f2, name = "YYY"),
                 envir = list(YYY = y))
          }
          play2[i] <- try(stats::uniroot(fd2, interval = s2)$root,
                          silent = TRUE)
          if (class(play2[i]) == "character") play2[i] <- play2[i - 1]
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

    for (i in 2:n2) {
      if (i %% 2 != 0) {
        ## Player 1
        if (stats::runif(1) < rho) {
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
                        .f = game$payoff$p1)
          }
          play1[i] <- try(
            stats::optim(par = stats::median(s1),
                         fn = f1,
                         method = "L-BFGS-B",
                         lower = s1[1],
                         upper = s1[2],
                         control = list(fnscale = -1))$par,
            silent = TRUE)
          if (class(play1[i]) == "try-error") play1[i] <- play1[i - 1]
        }
        ## Player2
        play2[i] <- play2[i - 1]
      } else {
        ## Player 1
        play1[i] <- play1[i - 1]
        ## Player 2
        if (stats::runif(1) < rho) {
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
                        .f = game$payoff$p2)
          }
          play2[i] <- try(
            stats::optim(par = stats::median(s2),
                         fn = f2,
                         method = "L-BFGS-B",
                         lower = s2[1],
                         upper = s2[2],
                         control = list(fnscale = -1))$par,
            silent = TRUE)
          if (class(play2[i]) == "try-error") play2[i] <- play2[i - 1]
        }
      }
    }
    play2[1] <- NA
  }

  return(data.frame(play1  = play1,
                    play2  = play2,
                    period = 1:n2,
                    moved = rep(game$player, n_periods)))
}
