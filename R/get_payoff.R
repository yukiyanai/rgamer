#' @title Get payoffs corresponding to specified strategies or actions.
#' @description \code{get_payoff} returns the vector of payoffs corresponding to
#'     a specified pair of strategies or a set of action profiles.
#' @param game A game defined by one of \code{normal_form()}, \code{seq_form()},
#'     or \code{extensive_form()}.
#' @param actions A list of strategies or actions to which the payoffs
#'     correspond. Each strategy or action must be defined in the game.
#' @param cons1 A named list of parameters contained in \code{game$payoff$p1}
#'     that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2}
#'     that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in
#'     \code{game$payoff$p1} and \code{game$payoff$p2} that should be treated as
#'     constants, if any. If \code{cons1} and \code{cons2} are exactly same, you
#'     can specify \code{cons_common} instead of specifying both \code{cons1}
#'     and \code{cons2}.
#' @return A vector of payoffs.
#' @importFrom magrittr %>%
#' @include get_payoff_normal.R get_payoff_extensive.R
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
get_payoff <- function(game,
                       actions,
                       cons1,
                       cons2,
                       cons_common) {

  if (class(game) %in% c("normal_form", "sequential_form")) {
    out <- get_payoff_normal(game, actions, cons1, cons2, cons_common)
  } else if (class(game) == "extensive_form") {
    out <- get_payoff_extensive(game, actions)
  } else {
    stop("game must be one of 'normal_form', 'sequential_form', or 'extensive_form'.")
  }

  return(out)
}
