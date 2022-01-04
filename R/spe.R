#' @title Find SPE of an extensive-form game.
#' @description \code{spe} finds subgame perfect equilibria of an
#'      an extensive-form game defined by \code{extensive_form()}.
#' @param game An "extensive_form" class object created by
#'     \code{extensive_form()}.
#' @return A list of subgame perfect equilibria  and game tree(s) with marked
#'     paths.
#' @include backward_induction.R find_pure_NE.R draw_tree.R subgames.R
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
spe <- function(game) {

  type <- id <- player <- player_id <- is_id <- is_seq <- psne <- NULL

  if (is.null(game$info_set)) { # perfect information
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
        info_set <- g$info_set
      } else{
        df_node <- g$subgame$node
        df_path <- g$subgame$path
        info_set <- g$subgame$info_set
      }

      if (length(unique(g$player)) == 1) {
        bw <- backward_induction(g)$sol
        PSNE <- unlist(bw)
        player_nodes <- df_node %>%
          dplyr::filter(type == "play") %>%
          dplyr::pull(id) %>%
          list()
        names(player_nodes) <- g$player[1]

        df_player_node <- df_node %>%
          dplyr::filter(type == "play") %>%
          dplyr::select(player, id) %>%
          dplyr::mutate(player_id = ifelse(player == u_players[1], 1, 2))
      } else {
        g_nf <- to_matrix(g)
        PSNE <- find_pure_NE(g_nf)

        #for (s in seq_along(PSNE)) {
        #  PSNE[s] <- PSNE[s] %>%
        #    stringr::str_replace("\\(\\(", "\\[\\(") %>%
        #    stringr::str_replace("\\)\\)", "\\)\\]")
        #}

        player_nodes <- list()
        for (p in u_players) {
          player_nodes[[p]] <- df_node %>%
            dplyr::filter(type == "play",
                          player == p) %>%
            dplyr::pull(id)
        }

        df_player_node <- df_node %>%
          dplyr::filter(type == "play") %>%
          dplyr::select(player, id) %>%
          dplyr::mutate(player_id = ifelse(player == u_players[1], 1, 2)) %>%
          dplyr::arrange(player_id)
      }

      PSNE_list[[i]] <- PSNE

      df_psne <- vector("list", length(PSNE))
      for (j in seq_along(PSNE)) {
        sol <- PSNE[j]
        sol_v <- sol %>%
          stringr::str_replace_all("\\(", "") %>%
          stringr::str_replace_all("\\)", "") %>%
          stringr::str_replace_all("\\[", "") %>%
          stringr::str_replace_all("\\]", "") %>%
          stringr::str_replace_all(" ", "") %>%
          stringr::str_split(pattern = ",") %>%
          unlist()

        nr0 <- nrow(df_player_node)
        df_player_node$is_id <- rep(NA, nr0)
        for (k in 1:nr0) {
          for (m in seq_along(info_set)) {
            if (df_player_node$id[k] %in% info_set[[m]])
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
             if (df_player_node$id[k] == info_set[[df_player_node$is_id[k]]][1]) {
               counter <- counter + 1
               df_player_node$is_seq[k] <- counter
             } else {
               df_player_node$is_seq[k] <- counter
             }
          }
        }

        df_psne[[j]] <- df_player_node %>%
          dplyr::mutate(psne = sol_v[df_player_node$is_seq]) %>%
          dplyr::select(!c(is_id, is_seq))
      }

      df_psne_list[[i]] <- df_psne
    }

    n_sg <- length(sg)
    if (n_sg == 1) {
      SPE <- PSNE_list[[1]] %>% as.list()
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
      for (i in 1:n_cand) {
        df_whole <- whole[[i]]
        nr_whole <- nrow(df_whole)
        for (j in 1:nrow(df_index)) {
          df_sub <- data.frame()
          for (k in 1:ncol(df_index)) {
            df_sub <- df_sub %>%
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

      SPE <- spe_sol_list <- vector("list", length(spe_id))
      for (i in 1:length(spe_id)) {
        SPE[[i]] <- PSNE_list[[1]][[spe_id[i]]]
        spe_sol_list[[i]] <- df_psne_list[[1]][[spe_id[i]]] %>%
          dplyr::select(player, psne) %>%
          dplyr::rename(s = psne) %>%
          dplyr::left_join(game$data$path, by = c("player", "s"))
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

      spe_trees <- lapply(1:length(SPE),
                          list_trees,
                          df_node = game$data$node,
                          direction = game$tree_param$direction,
                          show_node_id = game$tree_param$show_node_id,
                          info_set = game$info_set,
                          info_line = game$tree_para$info_line,
                          color_palette = game$tree_param$color_palette,
                          family = game$tree_param$family,
                          size_player = game$tree_param$size_player,
                          size_payoff = game$tree_param$size_payoff,
                          size_action = game$tree_param$size_action,
                          size_node_id = game$tree_param$size_node_id,
                          size_terminal = game$tree_param$size_terminal,
                          scale = game$tree_param$scale)

      return(list(sol = SPE, sol_tree = spe_trees))
    }
  }
}