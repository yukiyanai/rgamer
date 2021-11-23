#' @title Find payoff for a specific pair of strategies.
#' @description \code{get_payoff_normal} finds payoffs for a specified pair of
#'     strategies. It works for both normal_form and sequential_form classes.
#' @param game A "normal_form" class object created by \code{normal_form()} or
#'     a "sequential_form" class object created by \code{seq_form()}.
#' @param actions A list of strategies to which the payoffs correspond. Each
#'     strategy must be defined in the game.
#' @param cons1 A named list of parameters contained in \code{game$payoff$p1}
#'     that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2}
#'     that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in
#'     \code{game$payoff$p1} and \code{game$payoff$p2} that should be treated as
#'     constants, if any. If \code{cons1} and \code{cons2} are exactly same, you
#'     can specify \code{cons_common} instead of specifying both \code{cons1}
#'     and \code{cons2}.
#' @return A list containing payoffs.
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
get_payoff_normal <- function(game,
                              actions,
                              cons1 = NULL,
                              cons2 = NULL,
                              cons_common = NULL) {

  s1 <- s2 <- NULL

  if (length(actions) != 2)
    stop("actions must be a list of two elements")

  if (!is.null(names(actions))) {
    if (!(names(actions)[1] %in% game$player))
      stop(paste(names(actions)[1], "is not a player of the game."))
    if (!(names(actions)[2] %in% game$player))
      stop(paste(names(actions)[2], "is not a player of the game."))

    actions <- list(actions[[game$player[1]]],
                    actions[[game$player[2]]])
  }

  if (game$type == "matrix") {

    if (!(actions[[1]] %in% game$strategy$s1))
      stop(paste(actions[[1]], "is not an available strategy for", game$player[1]))
    if (!(actions[[2]] %in% game$strategy$s2))
      stop(paste(actions[[2]], "is not an available strategy for", game$player[2]))

    df <- game$df
    df <- df %>%
      dplyr::filter(s1 == actions[[1]],
                    s2 == actions[[2]])
    p1 <- df$p1
    p2 <- df$p2

  } else {

    if (!(is.numeric(actions[[1]]) & is.numeric(actions[[2]])))
      stop("Each element of actions must be a number.")

    if (actions[[1]] < game$strategy$s1[1] | actions[[1]] > game$strategy$s1[2])
      stop(paste(actions[[1]], "is out of the strategy range for", game$player[1]))
    if (actions[[2]] < game$strategy$s2[1] | actions[[2]] > game$strategy$s2[2])
      stop(paste(actions[[2]], "is out of the strategy range for", game$player[2]))

    ## transform "char_function" into "function"
    if (game$type == "char_function") {
      ff_list <- char2function(game$payoff[[1]],
                               game$payoff[[2]],
                               game$pars)

      game <- normal_form(players = game$player,
                          p1 = ff_list[[1]],
                          p2 = ff_list[[2]],
                          pars = c("x", "y"),
                          par1_lim = game$strategy[[1]],
                          par2_lim = game$strategy[[2]])
    }

    f1 <- game$payoff$p1
    f2 <- game$payoff$p2

    if (!is.null(cons_common)) cons1 <- cons2 <- cons_common

    if (!is.null(cons1)) {
      v <- c(actions[[1]], actions[[2]], as.vector(cons1))
      names(v) <- c(game$pars, names(cons1))
      p1 <- purrr::pmap(v, f1) %>% unlist()
    } else {
      p1 <- f1(actions[[1]], actions[[2]])
    }

    if (!is.null(cons2)) {
      v <- c(actions[[1]], actions[[2]], as.vector(cons2))
      names(v) <- c(game$pars, names(cons2))
      p2 <- purrr::pmap(v, f2) %>% unlist()
    } else {
      p2 <- f2(actions[[1]], actions[[2]])
    }
  }

  payoffs <- c(p1, p2)
  names(payoffs) <- game$player
  return(list(payoffs = payoffs))
}
