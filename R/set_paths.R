#' @title Define the paths of a game tree
#' @description \code{set_paths} creates the data frame of paths (edges) of a
#'     tree of an extensive-form game.
#' @param players_vec A vector of unique players.
#' @param n_choice A list of the number of choices at each node. An element of the list must
#'   correspond to each sequence (including the terminal node as the final sequence). Each element must
#'   be a numeric vector whose length equals the number of nodes at the specific sequence.
#' @param payoffs A named list of payoffs. Each element of the list must be a
#'     numeric vector of payoffs for a player. The names of the elements must
#'     match the names of the players specified  by \code{players}.
#' @return A data frame where each row corresponds to each path (edge; branch)
#'     of the game tree.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
set_paths <- function(players_vec, n_choice, actions) {

  n_choice_vec <- unlist(n_choice)
  n_path <- sum(n_choice_vec)
  nonzero_choice <- n_choice_vec[n_choice_vec != 0]
  nonzero_index <- which(n_choice_vec != 0)

  data.frame(
    id = 1:n_path,
    player = rep(players_vec, nonzero_choice),
    s = unlist(actions),
    node_from = rep(nonzero_index, nonzero_choice),
    node_to = 2:(n_path + 1)
  )

}
