#' @title Transform an extensive-form game into a normal-form game.
#' @description \code{to_matrix} transoforms a two-person extensive-form game
#'     into a normal-form game
#'     a specified pair of strategies or a set of action profiles.
#' @param game A two-person extensive-form game defined by
#'     \code{extensive_form()}.
#' @return A normal_form class object.
#' @importFrom magrittr %>%
#' @include get_payoff.R
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
to_matrix <- function(game) {

  if (!(class(game) %in% c("extensive_form", "subgame")))
    stop("game must be an 'extensive_form' or a 'subgame'")

  p_vec <- game$player
  u_player <- unique(p_vec)

  if (length(u_player) != 2)
    stop("This function only works with a two-person game")

  ## get payoffs
  actions1 <- game$action_prof[[1]]
  actions2 <- game$action_prof[[2]]
  payoff1 <- payoff2 <- NULL
  for (i in 1:length(actions1)) {
    for (j in 1:length(actions2)) {
      action_to_pass <- list(actions1[[i]], actions2[[j]])
      names(action_to_pass) <- u_player
      payoffs <- get_payoff(game, actions = action_to_pass)$payoffs
      payoff1 <- c(payoff1, payoffs[1])
      payoff2 <- c(payoff2, payoffs[2])
    }
  }

  nfg <- normal_form(
    players = u_player,
    s1 = game$strategy[[1]],
    s2 = game$strategy[[2]],
    p1 = payoff1,
    p2 = payoff2,
    byrow = TRUE)

  return(nfg)
}
