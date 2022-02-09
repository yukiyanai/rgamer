#' @title Format a restricted game
#' @description \code{format_restricted} formats a restricted game so that it
#'     can be passed to \code{backward_induction()} and \code{spe()}.
#' @param game A restricted game created by \code{restrict_action}.
#' @return A list containing the formatted game and the game tree with
#'     restricted action
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
format_restricted <- function(game) {

  new_game <- list()
  new_game$player <- game$player
  new_game$action <- game$action
  new_game$strategy <- game$strategy
  new_game$action_prof <- game$action_prof
  new_game$payoff <- game$payoff
  new_game$info_sets <- game$info_sets
  new_game$tree_params <- game$tree_params

  df_node <- game$data$node
  df_nid <- data.frame(old = df_node$id,
                       new = 1:nrow(df_node))

  df_path <- game$data$path
  df_pid <- data.frame(old = df_path$id,
                       new = 1:nrow(df_path))

  df_node$id <- df_nid$new
  df_path$id <- df_pid$new
  for (i in 1:nrow(df_path)) {
    for (j in 1:nrow(df_nid)) {
      if (df_path$node_from[i] == df_nid$old[j])
        df_path$node_from[i] <- df_nid$new[j]
      if (df_path$node_to[i] == df_nid$old[j])
        df_path$node_to[i] <- df_nid$new[j]
    }
  }

  new_game$data$node <- df_node
  new_game$data$path <- df_path

  list(game = new_game,
       old_tree = game$tree)
}
