#' @title Color specified paths in the game tree.
#' @description \code{draw_path} colors paths corresponding to the
#'     user-specified action profiles in an extensive-form game.
#' @param game A game defined by \code{extensive_form()}.
#' @param actions A list of actions of players.
#' @return A ggplot object of the game tree.
#' @include draw_tree.R get_payoff_extensive.R
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
draw_path <- function(game, actions) {

  player <- node_from <- s <- player_color <- info_group <- NULL

  if (is.null(names(actions))) {
    names(actions) <- unique(game$player)
  } else {
    for (i in 1:length(actions)) {
      if (!(names(actions)[i] %in% game$player))
        stop(paste(names(actions)[i], "is not a player of the game."))
    }
  }

  df_path <- game$data$path

  df_selected <- tibble::tibble(NULL)
  for (i in 1:length(actions)) {
    df_sub <- df_path |>
      dplyr::filter(player == names(actions)[i])

    df_sub_sub <- tibble::tibble(NULL)
    sub_nodes <- unique(df_sub$node_from)
    for (j in 1:length(actions[[i]])) {
      df_sub_j <- df_sub |>
        dplyr::filter(node_from == sub_nodes[j]) |>
        dplyr::filter(s == actions[[i]][j])
      df_sub_sub <- dplyr::bind_rows(df_sub_sub, df_sub_j)
    }
    df_selected <- dplyr::bind_rows(df_selected, df_sub_sub)
  }
  df_selected$played <- TRUE

  payoffs <- get_payoff(game, actions = actions)
  reached <- payoffs$reached
  payoffs <- payoffs$payoffs
  message(paste0("The game reaches at ", reached, ". \nPayoffs:"))
  print(payoffs)

  draw_tree(df_path = df_path,
            df_node = game$data$node,
            df_sol = df_selected,
            direction = game$tree_param$direction,
            show_node_id = game$tree_param$show_node_id,
            info_sets = game$info_sets,
            info_line = game$tree_para$info_line,
            color_palette = game$tree_param$color_palette,
            family = game$tree_param$family,
            size_player = game$tree_param$size_player,
            size_payoff = game$tree_param$size_payoff,
            size_action = game$tree_param$size_action,
            size_node_id = game$tree_param$size_node_id,
            size_terminal = game$tree_param$size_terminal,
            scale = game$tree_param$scale)
}
