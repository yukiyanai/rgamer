#' @title Transform an extensive-form game into a normal-form game.
#' @description \code{to_matrix} transforms a two-person extensive-form game
#'     into a normal-form game
#'     a specified pair of strategies or a set of action profiles.
#' @param game A two-person extensive-form game defined by
#'     \code{extensive_form()}.
#' @return A normal_form class object.
#' @include get_payoff.R
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
to_matrix <- function(game) {

  id <- player <- NULL

  if (!(class(game) %in% c("extensive_form", "subgame", "restricted_game")))
    stop("game must be an 'extensive_form', a 'subgame', or a 'restricted_game'")

  p_vec <- game$player
  u_player <- unique(p_vec)

  if (length(u_player) != 2)
    stop("This function only works with a two-person game")

  node_to_play <- list()
  u_players <- unique(game$player)
  for (i in 1:length(u_players)) {
    node_to_play[[i]] <- game$data$node |>
      dplyr::filter(player == u_players[i]) |>
      dplyr::pull(id)
  }
  names(node_to_play) <- u_players
  strategies <- extensive_strategy(player = game$player,
                                   action_list = game$action,
                                   info_sets = game$info_sets,
                                   info_sets_player = game$info_sets_player,
                                   node_to_play = node_to_play)
  ## get payoffs
  actions1 <- strategies$action_profile[[1]]
  actions2 <- strategies$action_profile[[2]]
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
    s1 = strategies$strategy$s1,
    s2 = strategies$strategy$s2,
    payoffs1 = payoff1,
    payoffs2 = payoff2,
    byrow = TRUE)
  return(nfg)
}
