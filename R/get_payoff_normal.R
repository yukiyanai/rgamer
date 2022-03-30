#' @title Find payoff for a specific pair of strategies.
#' @description \code{get_payoff_normal} finds payoffs for a specified pair of
#'     strategies. It works for both normal_form and sequential_form classes.
#' @param game A "normal_form" class object created by \code{normal_form()} or
#'     a "sequential_form" class object created by \code{seq_form()}.
#' @param actions A list of strategies to which the payoffs correspond. Each
#'     strategy must be defined in the game.
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in
#'     \code{game$payoff$payoffs1} and \code{game$payoff$payoffs2} that should
#'     be treated as constants, if any. If \code{cons1} and \code{cons2} are
#'     exactly same, you can specify \code{cons_common} instead of specifying
#'     both \code{cons1} and \code{cons2}.
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
    payoffs1 <- df$payoffs1
    payoffs2 <- df$payoffs2

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
                          payoffs1 = ff_list[[1]],
                          payoffs2 = ff_list[[2]],
                          pars = c("x", "y"),
                          par1_lim = game$strategy[[1]],
                          par2_lim = game$strategy[[2]])
    }

    f1 <- game$payoff$payoffs1
    f2 <- game$payoff$payoffs2

    if (!is.null(cons_common)) cons1 <- cons2 <- cons_common

    if (!is.null(cons1)) {
      v <- c(actions[[1]], actions[[2]], as.vector(cons1))
      names(v) <- c(game$pars, names(cons1))
      payoffs1 <- purrr::pmap(v, f1) %>% unlist()
    } else {
      payoffs1 <- f1(actions[[1]], actions[[2]])
    }

    if (!is.null(cons2)) {
      v <- c(actions[[1]], actions[[2]], as.vector(cons2))
      names(v) <- c(game$pars, names(cons2))
      payoffs2 <- purrr::pmap(v, f2) %>% unlist()
    } else {
      payofss2 <- f2(actions[[1]], actions[[2]])
    }
  }

  payoffs <- c(payoffs1, payoffs2)
  names(payoffs) <- game$player
  return(list(payoffs = payoffs))
}
