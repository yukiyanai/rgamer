#' @title Plays a normal-form game by simulation (softly best response)
#' @description \code{simu_game()} simulates plays expected in a normal-form game.
#' @details Simulate plays expected in a normal-form game defined by \code{normal_form()} when
#'   each player choose the action that is more likely to improve their payoff given the other
#'   player's previous action.
#' @param game An object of \code{normal_form} class defined by \code{normal_form()}.
#' @param n_periods A positive integer specifying how many times the game is played within each sample.
#' @param rho A numeric value in [0, 1] to control the degree of inertia in each player's behavior. If \code{rho = 1},
#'    each player does not change their choices over time. If \code{rho = 0}, each player does not stick to their
#'    previous choice at all.
#' @param lambda A positive value controlling the weight of the best response to the previous move of the opponent.
#' #' @param cons1 A named list of parameters contained in \code{game$payoff$p1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2} that should be treated as constants, if any.
#' @return data.frame containing the history of the game played.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @importFrom magrittr %>%
simu_game_sbr <- function(game,
                          n_periods,
                          rho = 0.2,
                          lambda = 1,
                          cons1 = NULL,
                          cons2 = NULL) {

  play1 <- rep(NA, n_periods)
  play2 <- rep(NA, n_periods)

  if (game$type == "matrix") {

    s1 <- game$strategy$s1
    s2 <- game$strategy$s2

    # for the first round
    play1[1] <- sample(s1, size = 1)
    play2[1] <- sample(s2, size = 1)

    for (i in 2:n_periods) {
      ## Player 1
      if (runif(1) < rho) {
        play1[i] <- play1[i - 1]
      } else {
        p <- game$df %>%
          dplyr::filter(s2 == play2[i - 1]) %>%
          dplyr::pull(p1)
        p <- exp(lambda * p)
        p <- p / sum(p)
        play1[i] <- sample(s1, size = 1, prob = p)
      }

      ## Player 2
      if (runif(1) < rho) {
        play2[i] <- play2[i - 1]
      } else {
        ###
        p <- game$df %>%
          dplyr::filter(s1 == play1[i - 1]) %>%
          dplyr::pull(p2)
        p <- exp(lambda * p)
        p <- p / sum(p)
        play2[i] <- sample(s2, size = 1, prob = p)
      }
    }

  } else if (game$type == "char_function") {

    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    play1[1] <- runif(1, min = s1[1], max = s1[2])
    play2[1] <- runif(1, min = s2[1], max = s2[2])

    for (i in 2:n_periods) {
      ## Player 1
      if (runif(1) < rho) {
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
        br <- try(uniroot(fd1, interval = s1)$root,
                        silent = TRUE)
        if (class(br) == "character") {
          play1[i] <- play1[i - 1]
        } else {
          left_len <- round((br - s1[1]) / (s1[2] - s1[1]) * 100, 0)
          left <- seq(from = s1[1],
                      to = br,
                      length.out = left_len)
          right <- seq(from = br,
                       to = s1[2],
                       length.out = 101 - left_len)[-1]
          s_vec <- c(left, right)
          p <- eval(f1, envir = list(XXX = s_vec))
          p <- exp(p * lambda)
          p <- p / sum(p)
          play1[i] <- sample(s_vec, size = 1, prob = p)
        }

        play1 <- as.numeric(play1)
      }

      ## Player 2
      if (runif(1) < rho) {
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
        br <- try(uniroot(fd2, interval = s2)$root,
                        silent = TRUE)
        if (class(br) == "character") {
          play2[i] <- play2[i - 1]
        } else {
          left_len <- round((br - s2[1]) / (s2[2] - s2[1]) * 100, 0)
          left <- seq(from = s2[1],
                      to = br,
                      length.out = left_len)
          right <- seq(from = br,
                       to = s2[2],
                       length.out = 101 - left_len)[-1]
          s_vec <- c(left, right)
          p <- eval(f2, envir = list(YYY = s_vec))
          p <- exp(p * lambda)
          p <- p / sum(p)
          play2[i] <- sample(s_vec, size = 1, prob = p)
        }

        play2 <- as.numeric(play2)
      }
    }

  } else {

    # for the first round
    s1 <- game$strategy$s1
    s2 <- game$strategy$s2
    play1[1] <- runif(1, min = s1[1], max = s1[2])
    play2[1] <- runif(1, min = s2[1], max = s2[2])

    for (i in 2:n_periods) {

      ## Player 1
      if (runif(1) < rho) {
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
        br <- try(optim(par = median(s1),
                              fn = f1,
                              method = "L-BFGS-B",
                              lower = s1[1],
                              upper = s1[2],
                              control = list(fnscale = -1))$par,
                        silent = TRUE)
        if (class(br) == "try-error") {
          play1[i] <- play1[i - 1]
        } else {
          left_len <- round((br - s1[1]) / (s1[2] - s1[1]) * 100, 0)
          left <- seq(from = s1[1],
                      to = br,
                      length.out = left_len)
          right <- seq(from = br,
                       to = s1[2],
                       length.out = 101 - left_len)[-1]
          s_vec <- c(left, right)
          p <- unlist(sapply(s_vec, f1))
          p <- exp(p * lambda)
          p <- p / sum(p)
          play1[i] <- sample(s_vec, size = 1, prob = p)
        }
        play1 <- as.numeric(play1)
      }

      ## Player 2
      if (runif(1) < rho) {
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
        br <- try(optim(par = median(s2),
                              fn = f2,
                              method = "L-BFGS-B",
                              lower = s2[1],
                              upper = s2[2],
                              control = list(fnscale = -1))$par,
                        silent = TRUE)
        if (class(br) == "try-error") {
          play2[i] <- play2[i - 1]
        } else {
          left_len <- round((br - s2[1]) / (s2[2] - s2[1]) * 100, 0)
          left <- seq(from = s2[1],
                      to = br,
                      length.out = left_len)
          right <- seq(from = br,
                       to = s2[2],
                       length.out = 101 - left_len)[-1]
          s_vec <- c(left, right)
          p <- unlist(sapply(s_vec, f2))
          p <- exp(p * lambda)
          p <- p / sum(p)
          play2[i] <- sample(s_vec, size = 1, prob = p)
        }
        play2 <- as.numeric(play2)
      }
    }

  }


  return(data.frame(play1 = play1,
                    play2 = play2,
                    period = 1:n_periods))

}
