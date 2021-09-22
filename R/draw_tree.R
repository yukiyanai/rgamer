#' @title Draw a game tree of an extensive-form game.
#' @description \code{draw_tree} draws a game tree of an extensive-form game
#'     defined by \code{extensive_form()}.
#' @param df_path A data frame containing the information about the path (i.e.,
#'     edges) of the tree.
#' @param df_node A data frame containing the information about the nodes of
#'     the tree.
#' @param df_sol A data frame containg the solution path of the tree. If
#'     \code{df_sol = NULL} (which is default), the game tree is drawn without
#'     no path colored.  If \code{df_sol} is passed, the solution paths will
#'     be colored.
#' @param show_node_id A logical value. If \code{TRUE}, the node numbers are
#'     displayed in the figure. Default is \code{TRUE}.
#' @param info_set A list of information sets.
#' @param info_line Line type to connect nodes in an information set. Either
#'     \code{"solid"} or \code{"dashed"}. Default to \code{"solid"}.
#' @param direction The direction to which a game tree grows.
#'     The value must be one of:
#'     \code{"right"},
#'     \code{"up"},
#'     \code{"down"},
#'     \code{"bidirectional"},
#'     \code{"horizonal"}, and
#'     \code{"vertical"}.
#'     Default is \code{"down"}.
#' @param color_palette A color palette to be used. Default is "Set1".
#' @param family A font family to be used in the tree.
#' @param size_player Font size for the players' names. Default is 4.
#' @param size_payoff Font size for the payoffs. Default is 4.
#' @param size_action Font size for the action displayed by each edge. Default
#'     is 4.
#' @param size_node_id Size of the node id. Default is 4.
#' @param size_terminal Size of the terminal node. Default is 2.
#' @param scale Scale \code{player_size}, \code{payoff_size},
#'     \code{action_size}, \code{noden_size}, \code{terminal_size}. It must be a
#'      positive number.
#' @return A ggplot object of a game tree.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
draw_tree <- function(df_path,
                      df_node,
                      df_sol = NULL,
                      show_node_id = TRUE,
                      info_set = NULL,
                      info_line = "solid",
                      direction = "down",
                      color_palette = "Set1",
                      family = NULL,
                      size_player = 4,
                      size_payoff = 4,
                      size_action = 4,
                      size_node_id = 4,
                      size_terminal = 2,
                      scale = NULL) {

  type <- node_from <- node_to <- id <- match_id <- NULL
  left <- x <- x_s <- x_m <- x_e <- y <- y_s <- y_m <- y_e <- NULL
  player <- payoff <- s <- NULL
  player_color <- info_group <- NULL

  df_play <- df_node %>%
    dplyr::filter(type == "play")

  df_payoff <- df_node %>%
    dplyr::filter(type == "payoff")

  if (direction == "bidirectional") {

    n_first_choice <- df_path %>%
      dplyr::filter(node_from == 1) %>%
      nrow()

    if (n_first_choice != 2) {
      stop("The first node must have two actions for a 'bidrectional' tree")
    }
    df_path0 <- df_path
    df_path$left <- rep(NA, nrow(df_path))
    df_path$left[1:2] <- 0:1
    for (i in 3:nrow(df_path)) {
      node_origin2 <- df_path$node_from[i]
      while (node_origin2 > 2) {
        df_search <- dplyr::filter(df_path, node_to == node_origin2)
        node_origin2 <- df_search$node_from[1]
      }
      df_path$left[i] <- ifelse(node_origin2 == 2, 0, 1)
    }
    y_adj_right <- with(df_path, y_s[1] - y_e[1])
    y_adj_left <-  with(df_path, y_s[2] - y_e[2])
    df_path <- df_path %>%
      dplyr::mutate(x_s = ifelse(left == 0, x_s, -x_s),
                    x_e = ifelse(left == 0, x_e, -x_e))

    df_path_top2 <- df_path[1:2,] %>%
      dplyr::mutate(y_e = y_s)
    df_path_rem <- df_path[-(1:2),] %>%
      dplyr::mutate(y_s = ifelse(left == 0, y_s + y_adj_right, y_s + y_adj_left),
                    y_e = ifelse(left == 0, y_e + y_adj_right, y_e + y_adj_left))
    df_path <- dplyr::bind_rows(df_path_top2, df_path_rem) %>%
      dplyr::mutate(x_m = 3/4 * x_s + 1/4  * x_e,
                    y_m = 1/2 * y_s + 1/2 * y_e,
                    y_m = ifelse(y_m == y_e, y_m + 1.5, y_m))

    ## Adjust payoff positions
    df_payoff <- df_path0 %>%
      dplyr::rename(match_id = id) %>%
      dplyr::select(match_id, x_e, y_e) %>%
      dplyr::right_join(df_payoff, by = c("x_e" = "x", "y_e" = "y")) %>%
      dplyr::select(-c(x_e, y_e))
    df_payoff <- df_path %>%
      dplyr::rename(x = x_e, y = y_e, match_id = id) %>%
      dplyr::select(x, y, match_id, left) %>%
      dplyr::right_join(df_payoff, by = "match_id")

    ## Adjust node positions
    df_play <- df_path0 %>%
      dplyr::rename(match_id = id) %>%
      dplyr::select(match_id, x_s, y_s) %>%
      dplyr::right_join(df_play, by = c("x_s" = "x", "y_s" = "y")) %>%
      dplyr::select(-c(x_s, y_s))
    df_play <- df_path %>%
      dplyr::rename(x = x_s, y = y_s, match_id = id) %>%
      dplyr::select(x, y, match_id, left) %>%
      dplyr::right_join(df_play, by = "match_id") %>%
      dplyr::select(-match_id) %>%
      dplyr::distinct()

    df_node <- dplyr::bind_rows(df_play, df_payoff)
  }

  if (!is.null(df_sol)) {
    df_sol$player_color <- as.integer(factor(df_sol$player))
    tree <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = df_path,
                            ggplot2::aes(x = x_s,
                                         xend = x_e,
                                         y = y_s,
                                         yend = y_e)) +
      ggplot2::geom_segment(data = df_sol,
                            ggplot2::aes(x = x_s,
                                         xend = x_e,
                                         y = y_s,
                                         yend = y_e,
                                         color = as.factor(player_color)),
                            size = 2)
  } else {
    tree <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = df_path,
                            ggplot2::aes(x = x_s,
                                         xend = x_e,
                                         y = y_s,
                                         yend = y_e))
  }

  if (direction == "up") {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         nudge_x = 5,
                         size = size_payoff) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_continuous(NULL, breaks = NULL) +
      ggplot2::scale_y_reverse(NULL, breaks = NULL)
  } else if (direction == "down") {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         nudge_x = -5,
                         size = size_payoff) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_reverse(NULL, breaks = NULL) +
      ggplot2::scale_y_reverse(NULL, breaks = NULL)
  } else if (direction == "right") {
    df_payoff <- df_payoff %>%
        dplyr::mutate(x = x + 5)

    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         size = size_payoff) +
      ggplot2::scale_x_continuous(NULL, breaks = NULL) +
      ggplot2::scale_y_continuous(NULL, breaks = NULL)
  } else if (direction == "horizontal") {
    df_payoff <- df_payoff %>%
      dplyr::mutate(x = x + 5)

    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         size = size_payoff) +
      ggplot2::scale_x_continuous(NULL, breaks = NULL) +
      ggplot2::scale_y_continuous(NULL, breaks = NULL)

  } else if (direction == "vertical") {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         nudge_x = -5,
                         size = size_payoff) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_reverse(NULL, breaks = NULL) +
      ggplot2::scale_y_reverse(NULL, breaks = NULL)
  } else {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = payoff),
                         nudge_x = -5,
                         size = size_payoff) +
      ggplot2::scale_x_reverse(NULL, breaks = NULL) +
      ggplot2::scale_y_continuous(NULL, breaks = NULL)
  }


  df_play$player_color <- as.integer(factor(df_play$player))
  p_length <- length(unique(df_play$player))

  if (!is.null(info_set)) {

    x_dif <- 100 / (length(unique(df_node$x)) - 1)
    y_dif <- 100 / (length(unique(df_node$y)) - 1)


    n_info_sets <- length(info_set)
    for (i in 1:n_info_sets) {
      info_set_i <- info_set[[i]]
      n_pairs <- length(info_set_i) - 1
      for (j in 1:n_pairs) {
        info_pair <- info_set_i[j:(j + 1)]
        df_info <- df_node[info_pair, ]

        info_x <- df_info$x
        if (info_x[1] == info_x[2]) {
          info_x <- c(info_x[1], info_x[1] - x_dif / 5, info_x[2])
        } else {
          info_x <- c(info_x[1], sum(info_x) / 2, info_x[2])
        }

        info_y <- df_info$y
        if (info_y[1] == info_y[2]) {
          info_y <- c(info_y[1], info_y[1] - y_dif / 5, info_y[2])
        } else {
          info_y <- c(info_y[1], sum(info_y) / 2, info_y[2])
        }

        bezier_df <- data.frame(x = info_x,
                                y = info_y,
                                info_group = as.character(i + p_length))

        if (info_line == "solid") {
          tree <- tree +
            ggforce::geom_bezier(data = bezier_df,
                                 ggplot2::aes(x = x,
                                              y = y,
                                              color = info_group,
                                              group = "cubic"),
                                 linetype = "solid",
                                 alpha = 0.5,
                                 size = 2)
        } else {
          tree <- tree +
            ggforce::geom_bezier(data = bezier_df,
                                 ggplot2::aes(x = x,
                                              y = y,
                                              color = info_group,
                                              group = "cubic"),
                                 linetype = "dotdash",
                                 size = 1.2)
        }
      }
    }
  }

  if (is.null(family)) {
    tree <- tree +
      ggplot2::geom_label(data = df_play,
                          ggplot2::aes(x = x, y = y,
                                       label = player,
                                       color = as.factor(player_color)),

                          size = size_payoff) +
      ggplot2::geom_text(data = df_path,
                         ggplot2::aes(x = x_m,
                                      y = y_m,
                                      label = s),
                         size = size_action) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::scale_color_brewer(palette = color_palette,
                                  guide = "none")
  } else {
    tree <- tree +
      ggplot2::geom_label(data = df_play,
                          ggplot2::aes(x = x, y = y,
                                       label = player,
                                       color = as.factor(player_color)),

                          size = size_player,
                          family = family) +
      ggplot2::geom_text(data = df_path,
                         ggplot2::aes(x = x_m,

                                      y = y_m,
                                      label = s),
                         size = size_action,
                         family = family) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     text = ggplot2::element_text(family = family)) +
      ggplot2::scale_color_brewer(palette = color_palette,
                                  guide = "none")
  }

  if (show_node_id) {
    df_node <- df_node %>%
      dplyr::mutate(id = paste0("n", id))
    tree <- tree +
      ggplot2::geom_label(data = df_node,
                          ggplot2::aes(x = x,
                                       y = y + 5,
                                       label = id),
                          color = "black",
                          size = size_node_id) +
      ggplot2::geom_point(data = df_node %>% dplyr::filter(type == "payoff"),
                          ggplot2::aes(x = x, y = y),
                          color = "black",
                          size = size_terminal)
  }

  return(tree)
}
