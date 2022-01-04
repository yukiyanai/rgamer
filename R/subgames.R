#' @title Find subgames of an extensive-form game
#' @description \code{subgames} finds subgames contained in an extensive-form
#'     game.
#' @details This function finds subgames of an extensive-form game defined by
#'     \code{extensive_form()} and returns the list of subgames. The node IDs of
#'     each subgame displayed with the game tree are taken from the original
#'     extensive-form game. In other words, the node ID of a subgame in the tree
#'     does not necessarily begins with 1.
#' @param game An extensive-form game defined by \code{extensive_form()}.
#' @param quietly If TRUE, a message telling how many subgames are found will
#'     not be displaed.
#' @return A list of subgames.
#' @include draw_tree.R
#' @importFrom magrittr %>%
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
subgames <- function(game, quietly = FALSE) {

  type <- node_from <- id <- x <- n <- player <- n_each <- new_id <- NULL

  subgame_list <- list(game)

  df_path <- game$data$path
  df_node <- game$data$node
  df_payoff <- df_node %>%
    dplyr::filter(type == "payoff") %>%
    dplyr::mutate(index = 1:dplyr::n())

  player_nodes <- unique(df_path$node_from)
  player_nodes <- player_nodes[-1]

  for (j in seq_along(player_nodes)) {
    sg_flag <- TRUE
    subnodes <- player_nodes[j]
    subgame_path <- df_path %>%
      dplyr::filter(node_from %in% subnodes)
    while (TRUE) {
      df_path_sub <- df_path %>%
        dplyr::filter(node_from %in% subnodes)
      subnodes <- df_path_sub$node_to
      df_add <- df_path %>%
        dplyr::filter(node_from %in% subnodes)
      subgame_path <- dplyr::bind_rows(subgame_path, df_add)
      if (nrow(df_add) == 0) break
    }

    ## Check if all nodes of an given info set is included
    if (!is.null(game$info_set)) {
      subg_info_set <- list()
      for (s in seq_along(game$info_set)) {
        tgt_info_set <- game$info_set[[s]]
        n_in <- sum(tgt_info_set %in% subgame_path$node_from)
        if (n_in == length(tgt_info_set)) {
          subg_info_set <- c(subg_info_set, list(tgt_info_set))
        } else if (n_in > 0) {
          sg_flag <- FALSE
          break
        }
      }
      if (length(subg_info_set) == 0) subg_info_set <- NULL
    } else {
      subg_info_set <- NULL
    }

    if (sg_flag) {

      subg_node_play <- df_node %>%
        dplyr::filter(type == "play") %>%
        dplyr::filter(id %in% subgame_path$node_from)

      subg_node_payoff <- df_node %>%
        dplyr::filter(type == "payoff") %>%
        dplyr::filter(id %in% subgame_path$node_to)

      subgame_node <- dplyr::bind_rows(subg_node_play, subg_node_payoff) %>%
        dplyr::mutate(new_id = 1:dplyr::n())

      ## Extract subgame players
      subg_p_str <- subgame_node %>%
        dplyr::select(x, player) %>%
        dplyr::group_by(x) %>%
        dplyr::summarize(n_each = dplyr::n()) %>%
        dplyr::pull(n_each)
      subg_players <- subgame_node %>%
        dplyr::pull(player)
      subg_player_list <- list()
      end_index <- 0
      for (i in seq_along(subg_p_str)) {
        start_index <- end_index + 1
        if (subg_p_str[i] == 1) {
          subg_player_list[[i]] <- subg_players[start_index]
          end_index <- start_index
        } else {
          subg_player_list[[i]] <- subg_players[start_index:(end_index + subg_p_str[i])]
          end_index <- end_index + subg_p_str[i]
        }
      }

      ## Extract subgame actions
      subg_nodes_unique <- subgame_path %>%
        dplyr::pull(node_from) %>%
        unique()
      subg_action_list <- list()
      for (i in seq_along(subg_nodes_unique)) {
        subg_action_list[[i]] <- subgame_path %>%
          dplyr::filter(node_from == subg_nodes_unique[i]) %>%
          dplyr::pull(s)
      }

      ## Extract subgame payoffs
      subg_players_unique <- subg_player_list %>%
        unlist() %>%
        unique() %>%
        stats::na.omit()
      subg_payoff_list <- list()
      for (i in seq_along(subg_players_unique)) {
        subg_payoff_list[[i]] <- subg_node_payoff %>%
          dplyr::pull(subg_players_unique[i])
      }
      names(subg_payoff_list) <- subg_players_unique

      ## info set adjustment
      subg_info_set_original <- subg_info_set
      if (!is.null(subg_info_set)) {
        for (s in seq_along(subg_info_set)) {
          old_id <- subg_info_set[[s]]
          subg_info_set[[s]] <- subgame_node %>%
            dplyr::filter(id %in% old_id) %>%
            dplyr::pull(new_id)
        }
      }

      subgame <- extensive_form(
        players = subg_player_list,
        actions = subg_action_list,
        payoffs = subg_payoff_list,
        info_set = subg_info_set,
        direction = game$tree_params$direction,
        show_tree = FALSE
      )
      subgame$subgame$path <- subgame_path
      subgame$subgame$node <- subgame_node
      subgame$subgame$info_set <- subg_info_set_original
      subgame$tree <- draw_tree(
        df_path = subgame_path,
        df_node = subgame_node,
        info_set = subg_info_set,
        direction = game$tree_params$direction
      )

      attr(subgame, "class") <- "subgame"
      subgame_list <- c(subgame_list, list(subgame))
    }
  }

  n_subgames <- length(subgame_list)

  if (!quietly) {
    if (n_subgames == 1) {
      message("The game has 1 subgame, which is the game itself.")
    } else {
      message("The game has ", n_subgames, " subgames.")
    }
  }

  return(subgame_list)
}
