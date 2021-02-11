#' @title Define and solve a extensive-form (or strategic-form) game
#' @description \code{extensive_form} defines an extensive-form game
#' @details
#' @param players A list of players. Each element of the list must be a character string
#'     or a vector of character strings at a specific sequence.
#' @param n_node A numeric vector of the number of nodes at a specific sequence.
#' @param n_choice A list of the number of choices at each node. Each element of the list must be
#'     a numeric vector of the numbers of choices at a specific sequence. You must assign \code{0}
#'     for the terminal (payoff) nodes.
#' @param strategy A list of strategies. Each element of the list must be a vector of character strings
#'     that corresponds to a specific node.
#' @param payoff A named list of payoffs. Each element of the list must be a numeric vector of
#'     payoffs for a player. The names of the elements must match the names of the players
#'     specified  by \code{players}.
#' @param quietly A logical value. If \code{TRUE}, the subgame perfect equilibrium will not be displayed
#'     on screen. Default is \code{FALSE}.
#' @param show_tree A logical value. If \code{TRUE}, the game tree will be displayed. Default is \code{TRUE}
#' @param mark_path A logical value. If \code{TRUE}, The paths played in the equilibrium will be
#'     marked (with color and bold lines). Default is \code{FALSE}.
#' @param direction The direction to which a game tree grows. The value must be one of
#'     \code{"right"}, \code{"up"}, \code{"down"}, and \code{"bidirectional"}. Default is \code{"right"}.
#' @param color_palette A color palette to be used. Default is "Set1".
#' @return An object of "extensive_form" class, which defines an extensive-form (or sequential) game.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' g1 <- extensive_form(
#'   players = list("Kamijo",
#'                  rep("Yanai", 2)),
#'   n_node = c(1, 2, 4),
#'   n_choice = list(2,
#'                   c(2, 2),
#'                   rep(0, 4)),
#'   strategy <- list(c("U", "D"),
#'                    c("U'", "D'"), c("U''", "D''")),
#'   payoff = list(Kamijo = c(0, 2, 1, 3),
#'                 Yanai  = c(0, 1, 2, 1)))
#'
#' g2 <- extensive_form(
#'   players = list("f", c("m", "m")),
#'  n_node = c(1, 2, 4),
#'  n_choice = list(2,
#'                  c(2, 2),
#'                  rep(0, 4)),
#'   strategy = list(c("ballet", "baseball"),
#'                  c("ballet", "baseball"), c("ballet", "baseball")),
#'  payoff = list(f = c(2, 0, 0, 1),
#'                m = c(1, 0, 0, 2)),
#'  mark_path = TRUE,
#'  quietly = TRUE
#' )
#'
#' g3 <- extensive_form(
#'   players = list("p1", rep("p2", 3)),
#'   n_node = c(1, 3, 6),
#'   n_choice = list(3,
#'                   rep(2, 3),
#'                   rep(0, 6)),
#'   strategy = list(c("C", "D", "E"),
#'                   c("F", "G"), c("H", "I"), c("J", "K")),
#'   payoff = list(p1 = c(3, 1, 1, 2, 2, 1),
#'                 p2 = c(0, 0, 1, 1, 2, 3)),
#'   mark_path = TRUE,
#'   direction = "down")
#'
#' g4 <- extensive_form(
#'   players = list("child",
#'                  "parent"),
#'    n_node = c(1, 2, 2),
#'    n_choice = list(2,
#'                    c(0, 2),
#'                    c(0, 0)),
#'    strategy = list(c("give up", "keep asking"),
#'                    c("leave",   "buy")),
#'    payoff   = list(child  = c(0, -10, 10),
#'                    parent = c(5, -10,  2)),
#'    mark_path = TRUE)
extensive_form <- function(
  players = NULL, # list, one vector for each sequence
  n_node,         # vector, one value for each sequence
  n_choice,       # list, one vector  for each sequence
  strategy,       # list, one vector for each node.
  payoff,         # named list, one vector for each player. Names must match the unique names of the players
  quietly = FALSE,
  show_tree = TRUE,
  mark_path = FALSE,
  direction = "right", # direction of the game tree
  color_palette = "Set1") {

   direction <- match.arg(direction, choices = c("right", "up", "down", "bidirectional"))

  u_players <- players %>% unlist() %>% unique()
  n_players <- length(u_players)
  n_seq <- length(players)

  n_node2 <- c(1, rep(NA, n_seq))
  keep <- 1
  for (i in 2:length(n_choice)) {
    n_node2[i] <- sum(n_choice[[i - 1]] * n_node[i - 1])
    vec <- rep(n_choice[[i-1]], each = n_node[i])
    keep <- c(keep, vec)
  }
  keep <- keep != 0

  ## number of choices at each sequence
  n_choice_seq <- rep(NA, length(players))
  for (i in seq_along(players)) {
    n_choice_seq[i] <- length(strategy[[i]])
  }

  p_length <- length(payoff[[1]])

  n_choice_vec <- n_choice %>% unlist()
  n_path <- sum(n_choice_vec)
  nonzero_choice <- n_choice_vec[n_choice_vec != 0]
  nonzero_index <- which(n_choice_vec != 0)

  players_vec <- players %>% unlist()
  df_path <- data.frame(
    id        = 1:n_path,
    player    = rep(players_vec, nonzero_choice),
    s         = unlist(strategy),
    node_from = rep(nonzero_index, nonzero_choice),
    node_to   = 2:(n_path + 1))

  df_node <- tree_position(players, n_node, n_choice)

  df_payoff <- as.data.frame(payoff)

  payoff_label <- rep(NA, nrow(df_payoff))
  for (i in 1:nrow(df_payoff)) {
    payoff_label[i] <- paste(df_payoff[i, ], collapse = ", ")
  }
  payoff_label <- paste0("(", payoff_label, ")")

  df_payoff <- df_node %>%
    dplyr::filter(type == "payoff") %>%
    dplyr::bind_cols(df_payoff) %>%
    dplyr::mutate(payoff = payoff_label)

  df_node <- df_node %>%
    dplyr::filter(type == "play")
  node_id_vec <- df_node %>%
    dplyr::arrange(id) %>%
    dplyr::pull(id)

  df_pos <- dplyr::bind_rows(df_node, df_payoff) %>%
    dplyr::arrange(id)

  df_played <- tibble::tibble(NULL)
  ## Backward induction
  for (i in nrow(df_node):1) {
    df_sub <- df_path %>%
      dplyr::filter(node_from == node_id_vec[i])

    check_player <- df_sub %>% dplyr::pull(player) %>% unique()
    check_payoff <- df_sub %>% dplyr::pull(node_to)
    df_subgame <- df_pos %>%
      dplyr::filter(id %in% check_payoff) %>%
      dplyr::select(dplyr::all_of(check_player), id)
    max_v <- max(df_subgame[, 1])
    choice <- df_subgame$id[df_subgame[, 1] == max_v]

    if (length(choice) > 1) {
      keep <- df_pos[choice, u_players] %>%
        as.matrix() %>%
        apply(1, mean) %>%
        which.max()
      df_keep <- df_pos[choice, u_players]
      df_keep <- df_keep[keep, ]
      df_pos[node_id_vec[i], u_players] <- df_keep
    } else {
      df_pos[node_id_vec[i], u_players] <- df_pos[choice, u_players]
    }

    df_played <- df_sub %>%
      dplyr::mutate(played = ifelse(node_to == choice, TRUE, FALSE)) %>%
      dplyr::bind_rows(df_played)
   }

  df_path <- df_path %>%
    dplyr::mutate(x_s = df_pos$x[df_path$node_from],
                  x_e = df_pos$x[df_path$node_to],
                  y_s = df_pos$y[df_path$node_from],
                  y_e = df_pos$y[df_path$node_to],
                  x_m = 3/4 * x_s + 1/4  * x_e,
                  y_m = 1/2 * y_s + 1/2 * y_e,
                  y_m = ifelse(y_m == y_e, y_m + 3, y_m),
                  played = df_played$played)

  ## Game Tree
  if (direction == "bidirectional") {
    if (n_choice_vec[1] != 2) stop(message("The first node must have two options for a 'bidrectional' tree"))
    df_path0 <- df_path
    df_path$left <- rep(NA, nrow(df_path))
    df_path$left[1:2] <- 0:1
    for (i in 3:nrow(df_path)) {
      node_origin2 <- df_path$node_from[i]
      while (node_origin2 > 2) {
        df_search <- filter(df_path, node_to == node_origin2)
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
    df_node <- df_path0 %>%
      dplyr::rename(match_id = id) %>%
      dplyr::select(match_id, x_s, y_s) %>%
      dplyr::right_join(df_node, by = c("x_s" = "x", "y_s" = "y")) %>%
      dplyr::select(-c(x_s, y_s))
    df_node <- df_path %>%
      dplyr::rename(x = x_s, y = y_s, match_id = id) %>%
      dplyr::select(x, y, match_id, left) %>%
      dplyr::right_join(df_node, by = "match_id") %>%
      dplyr::select(-match_id) %>%
      dplyr::distinct()
  }

  df_sol <- df_path %>%
    dplyr::filter(played)

  n_paths_played <- table(df_sol$node_from)
  n_sol <- prod(n_paths_played)


  choice <- vector(mode = "list", length = length(n_paths_played))
  node_id <- names(n_paths_played)
  node_player <- rep(NA, length.out = length(n_paths_played))
  for (i in seq_along(n_paths_played)) {
    node_player[i] <- unique(df_sol$player[df_sol$node_from == node_id[i]])
    choice[[i]] <- df_sol %>%
      dplyr::filter(node_from == names(n_paths_played)[i]) %>%
      dplyr::pull(s)
  }

  sol_sets <- expand.grid(choice) %>%
    t() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(
      cols = 1:n_sol,
      names_to = "sol_id",
      values_to = "s"
    ) %>%
    dplyr::arrange(sol_id)
  sol_sets$player <- rep(node_player, n_sol)

  df_sol2 <- dplyr::full_join(df_sol, sol_sets,
                              by = c("s", "player")) %>%
    dplyr::arrange(sol_id, node_from)

  sol_id_vec <- unique(df_sol2$sol_id)

  SGPE <- rep(NA, length.out = n_sol)
  for (j in 1:n_sol) {

    df_sol_sub <- df_sol2[df_sol2$sol_id == sol_id_vec[j],]


    SGPE_tmp <- rep(NA, n_players)
    for (i in seq_along(u_players)) {
      df_p <- df_sol_sub %>% dplyr::filter(player == u_players[i])
      s <- df_p %>% dplyr::pull(s)
      s_mid <- paste(s, collapse = ", ")
      SGPE_tmp[i] <- paste0("(", s_mid, ")")
    }
    SGPE_tmp <- SGPE_tmp %>%
      paste(collapse = ", ")
    SGPE_tmp <- paste0("[", SGPE_tmp, "]")
    SGPE[j] <- SGPE_tmp
  }

  if (mark_path) {
    tree <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = df_path,
                            ggplot2::aes(x = x_s,  xend = x_e,
                                         y = y_s,  yend = y_e)) +
      ggplot2::geom_segment(data = df_sol,
                            ggplot2::aes(x = x_s, xend = x_e,
                                         y = y_s, yend = y_e,
                                         color = player),
                            size = 2)
  } else {
    tree <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = df_path,
                            ggplot2::aes(x = x_s,  xend = x_e,
                            y = y_s,  yend = y_e))
  }

  if (direction == "up") {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x, y = y, label = payoff),
                         nudge_x = 5, size = 4) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_continuous(NULL, breaks = NULL) +
      ggplot2::scale_y_reverse(NULL, breaks = NULL)
  } else if (direction == "down") {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x, y = y, label = payoff),
                         nudge_x = -5, size = 4) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_reverse(NULL, breaks = NULL) +
      ggplot2::scale_y_reverse(NULL, breaks = NULL)
  } else {
    tree <- tree +
      ggplot2::geom_text(data = df_payoff,
                         ggplot2::aes(x = x, y = y, label = payoff),
                         nudge_x = 5 - 10 * df_payoff$left,
                         size = 4) +
      ggplot2::scale_x_continuous(NULL, breaks = NULL) +
      ggplot2::scale_y_continuous(NULL, breaks = NULL)
  }

  tree <- tree +
    ggplot2::geom_label(data = df_node,
                        ggplot2::aes(x = x, y = y,
                                     label = player,
                                     color = player),
                        size = 4) +
    ggplot2::geom_text(data = df_path,
                       ggplot2::aes(x = x_m, y = y_m, label = s),
                       nudge_x = 5 - 10 * df_path$left,
                       size = 4) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_color_brewer(palette = color_palette,
                                guide = FALSE)

   if (show_tree) {
    plot(tree)
  }

  if (!quietly) {
    if (length(SGPE) > 1) {
      message("Subgame perfect equilibria: ",
              paste(SGPE, collapse = ", "))
    } else {
      message("Subgame perfect equilibrium: ", SGPE)
    }
  }

  value <- list(player   = players,
                strategy = strategy,
                payoff   = payoff,
                sgpe     = SGPE,
                tree     = tree)

  structure(value, class = "extensive_form")
}
