#' @title Backward induction for an extensive-form game.
#' @description \code{backward indcuntion} finds backward-induction solutions of
#'      an extensive-form game defined by \code{extensive_form()}.
#' @param game An "extensive_form" class object created by
#'     \code{extensive_form()}.
#' @param restriction TRUE if the game has restricted sets of actions.
#' @return A list of backward-induction solution(s) and game tree(s) with
#'     marked paths.
#' @include draw_tree.R
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
backward_induction <- function(game, restriction = FALSE) {

  node_from <- node_to <- id <- played <- NULL
  type <- player <- NULL

  if (!is.null(game$info_sets)) {
    if (max(sapply(game$info_sets, length)) > 1)
      stop("This is not a perfect-information game.")
  }

  df_node <- game$data$node
  df_path <- game$data$path
  df_play <- df_node %>%
    dplyr::filter(type == "play")

  u_players <- df_path %>%
    dplyr::pull(player) %>%
    unique()

  df_node_list <- list(df_node)
  df_played_list <- tibble::tibble(NULL) %>% list()

  node_id_vec <- df_path %>%
    dplyr::arrange(dplyr::desc(node_from)) %>%
    dplyr::pull(node_from) %>%
    unique()

  for (i in node_id_vec) {
    df_sub <- df_path %>%
      dplyr::filter(node_from == i)

    check_player <- df_sub %>% dplyr::pull(player) %>% unique()
    check_payoff <- df_sub %>% dplyr::pull(node_to)

    df_node_tmp <- df_played_tmp <- list()

    for (j in 1:length(df_node_list)) {
      df_subgame <- df_node_list[[j]] %>%
        dplyr::filter(id %in% check_payoff) %>%
        dplyr::select(dplyr::all_of(check_player), id)
      max_v <- max(df_subgame[, 1])
      choice <- df_subgame$id[df_subgame[, 1] == max_v]

      if (length(choice) > 1) {
        df_node_list_new <- list()
        df_played_list_new <- list()
        for (k in choice) {
          df_node_list_0 <- list(df_node_list[[j]])
          df_played_list_0 <- list(df_played_list[[j]])
          for (v in 1:length(df_node_list_0)) {
            df_node_list_0[[v]][i, u_players] <- df_node_list_0[[v]][k, u_players]
            df_node_list_new <- c(df_node_list_new, df_node_list_0)
            df_played_list_0[[v]] <- df_sub %>%
              dplyr::mutate(played = ifelse(node_to == k, TRUE, FALSE)) %>%
              dplyr::bind_rows(df_played_list_0[[v]])
            df_played_list_new <- c(df_played_list_new, df_played_list_0)
          }
        }
        df_node_tmp <- c(df_node_tmp, df_node_list_new)
        df_played_tmp <- c(df_played_tmp, df_played_list_new)

      } else {
        df_node_list[[j]][i, u_players] <- df_node_list[[j]][choice, u_players]
        df_played_list[[j]] <- df_sub %>%
          dplyr::mutate(played = ifelse(node_to == choice, TRUE, FALSE)) %>%
          dplyr::bind_rows(df_played_list[[j]])

        df_node_tmp <- c(df_node_tmp, list(df_node_list[[j]]))
        df_played_tmp <- c(df_played_tmp, list(df_played_list[[j]]))
      }
    }
    df_node_list <- df_node_tmp
    df_played_list <- df_played_tmp
  }

  n_sols <- length(df_node_list)

  df_path_list <- list()
  for (j in 1:n_sols) {
    df_path_list[[j]] <- df_path %>%
      dplyr::mutate(played = df_played_list[[j]]$played)
  }

  make_df_sol <- function(df_played) {
    df_sol <- df_played %>%
      dplyr::filter(played) %>%
      dplyr::arrange(node_from)
    return(df_sol)
  }
  df_sol_list <- lapply(df_played_list, make_df_sol)

  find_sol <- function(df_sol, u_players) {
    n_players <- length(u_players)
    sol <- rep(NA, n_players)
    for (i in seq_along(u_players)) {
      df_p <- df_sol %>%
        dplyr::filter(player == u_players[i])
      s <- df_p %>%
        dplyr::pull(s)
      s_mid <- paste(s, collapse = ", ")
      sol[i] <- paste0("(", s_mid, ")")
    }
    sol <- sol %>%
      paste(collapse = ", ")
    sol <- paste0("[", sol, "]")
    return(sol)
  }
  sols_list <- lapply(df_sol_list,
                      find_sol,
                      u_players = u_players)

  list_trees <- function(x, ...) {
    draw_tree(df_path = df_path_list[[x]],
              df_sol = df_sol_list[[x]],
              ...)
  }
  bi_trees <- lapply(1:n_sols,
                     list_trees,
                     df_node = df_node,
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
                     scale = game$tree_param$scale,
                     restriction = restriction)

  list(sol = sols_list, sol_tree = bi_trees)
}
