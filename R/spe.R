#' @title Find SPE of an extensive-form game.
#' @description \code{spe} finds subgame perfect equilibria of an
#'      an extensive-form game defined by \code{extensive_form()}.
#' @param game An "extensive_form" class object created by
#'     \code{extensive_form()}.
#' @param restriction TRUE if the game has restricted sets of actions.
#' @return A list of subgame perfect equilibria  and game tree(s) with marked
#'     paths.
#' @include backward_induction.R find_pure_NE.R draw_tree.R subgames.R
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
spe <- function(game, restriction = FALSE) {

  type <- id <- player <- player_id <- is_id <- is_seq <- psne <- NULL

  if (restriction) {
    fgame <- format_restricted(game)
    game <- fgame$game
    df_pid <- fgame$df_pid
    df_nid <- fgame$df_nid
  }

  if (is.null(game$info_sets)) { # perfect information
    if (!("NATURE" %in% game$player)) {
      return (backward_induction(game))
    } else {
      message("Sorry. Functionality to find equilibria with 'NATURE' nodes is under construction.")
    }

  } else { # imperfect information

    u_players <- unique(game$player)
    if (length(u_players) > 2) {
      stop("SPE cannot be found for an imperfect-info game with more than 2 players.")
    }

    sg <- subgames(game, quietly = TRUE)
    PSNE_list <- play_nodes_list <- df_psne_list <- vector("list", length(sg))

    for (i in seq_along(sg)) {
      g <- sg[[i]]

      if (i == 1) {
        df_node <- g$data$node
        df_path <- g$data$path
        info_sets <- g$info_sets
      } else{
        df_node <- g$subgame$node
        df_path <- g$subgame$path
        info_sets <- g$subgame$info_sets
      }

      if (length(unique(g$player)) == 1) {
        bw <- backward_induction(g)$sol
        PSNE <- unlist(bw)
        player_nodes <- df_node |>
          dplyr::filter(type == "play") |>
          dplyr::pull(id) |>
          list()
        names(player_nodes) <- g$player[1]

        df_player_node <- df_node |>
          dplyr::filter(type == "play") |>
          dplyr::select(player, id) |>
          dplyr::mutate(player_id = ifelse(player == u_players[1], 1, 2))
      } else {
        g_nf <- to_matrix(g)
        PSNE <- find_pure_NE(g_nf)

        player_nodes <- list()
        for (p in u_players) {
          player_nodes[[p]] <- df_node |>
            dplyr::filter(type == "play",
                          player == p) |>
            dplyr::pull(id)
        }

        df_player_node <- df_node |>
          dplyr::filter(type == "play") |>
          dplyr::select(player, id) |>
          dplyr::mutate(player_id = ifelse(player == u_players[1], 1, 2)) |>
          dplyr::arrange(player_id)
      }

      PSNE_list[[i]] <- PSNE

      df_psne <- vector("list", length(PSNE))
      for (j in seq_along(PSNE)) {
        sol <- PSNE[j]
        sol_v <- sol |>
          stringr::str_replace_all("\\(", "") |>
          stringr::str_replace_all("\\)", "") |>
          stringr::str_replace_all("\\[", "") |>
          stringr::str_replace_all("\\]", "") |>
          stringr::str_replace_all(" ", "") |>
          stringr::str_split(pattern = ",") |>
          unlist()

        nr0 <- nrow(df_player_node)
        df_player_node$is_id <- rep(NA, nr0)
        for (k in 1:nr0) {
          for (m in seq_along(info_sets)) {
            if (df_player_node$id[k] %in% info_sets[[m]])
              df_player_node$is_id[k] <- m
          }
        }

        df_player_node$is_seq <- rep(NA, nr0)
        counter <- 0
        for (k in 1:nr0) {
          if (is.na(df_player_node$is_id[k])) {
            counter <- counter + 1
            df_player_node$is_seq[k] <- counter
          } else {
             if (df_player_node$id[k] == info_sets[[df_player_node$is_id[k]]][1]) {
               counter <- counter + 1
               df_player_node$is_seq[k] <- counter
             } else {
               df_player_node$is_seq[k] <- counter
             }
          }
        }

        df_psne[[j]] <- df_player_node |>
          dplyr::mutate(psne = sol_v[df_player_node$is_seq]) |>
          dplyr::select(!c(is_id, is_seq))
      }

      df_psne_list[[i]] <- df_psne
    }

    n_sg <- length(sg)
    if (n_sg == 1) {
      SPE <- PSNE_list[[1]] |> as.list()
      spe_sol_list <- vector("list", length(SPE))
      for (i in 1:length(SPE)) {
        df_path_tmp <- game$data$path |>
          dplyr::mutate(s = stringr::str_replace_all(s, " ", ""))
        spe_sol_list[[i]] <- df_psne_list[[1]][[i]] |>
          dplyr::select(player, psne) |>
          dplyr::rename(s = psne) |>
          dplyr::left_join(df_path_tmp, by = c("player", "s"))
      }
    } else {
      n_psne <- sapply(df_psne_list, length)

      index_list <- vector("list", n_sg - 1)
      for (j in seq_along(index_list)) {
        index_list[[j]] <- 1:n_psne[j + 1]
      }

      df_index <- expand.grid(index_list)

      whole <- df_psne_list[[1]]
      n_cand <- length(whole)

      spe_id <- NULL

      if (n_cand > 0) {
        for (i in 1:n_cand) {
          df_whole <- whole[[i]]
          nr_whole <- nrow(df_whole)
          for (j in 1:nrow(df_index)) {
            df_sub <- data.frame()
            for (k in 1:ncol(df_index)) {
              df_sub <- df_sub |>
                dplyr::bind_rows(df_psne_list[[k + 1]][df_index[j, k]])
            }
            df_stack <- dplyr::bind_rows(df_whole, df_sub)
            df_check <- dplyr::distinct(df_stack)
            nr_check <- nrow(df_check)

            if (nr_check == nr_whole) {
              spe_id <- c(spe_id, i)
            }
          }
        }
      }

      SPE <- spe_sol_list <- vector("list", length(spe_id))

      if (length(spe_id) > 0) {
        for (i in 1:length(spe_id)) {
          SPE[[i]] <- PSNE_list[[1]][[spe_id[i]]]
          df_path_tmp <- game$data$path |>
            dplyr::mutate(s = stringr::str_replace_all(s, " ", ""))
          spe_sol_list[[i]] <- df_psne_list[[1]][[spe_id[i]]] |>
            dplyr::select(id, player, psne) |>
            dplyr::rename(node_from = id,
                          s = psne) |>
            dplyr::left_join(df_path_tmp, by = c("node_from", "player", "s"))
        }
      }
    }

    if (length(SPE) == 0) {
      message("No pure-strategy SPE.")
      return(NULL)
    } else {
      list_trees <- function(x, ...) {
        draw_tree(df_path = game$data$path,
                  df_sol = spe_sol_list[[x]],
                  ...)
      }

      if (!restriction) {
        new_info_sets <- game$info_sets
      } else {
        new_info_sets <- list()
        kept_nodes <- df_nid$old[df_nid$new %in% game$data$node$id]
        for (s in 1:length(game$info_sets)) {
          if (all(game$info_sets[[s]] %in% kept_nodes)) {
            new_info_sets <- c(new_info_sets, game$info_sets[[s]])
          }
        }
        if (length(new_info_sets) == 0) new_info_sets <- NULL
      }

      spe_trees <- lapply(1:length(spe_sol_list),
                          list_trees,
                          df_node = game$data$node,
                          direction = game$tree_param$direction,
                          show_node_id = game$tree_param$show_node_id,
                          info_sets = new_info_sets,
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

      return(list(sol = SPE, sol_tree = spe_trees))
    }
  }
}
