#' @title Define an extensive-form (or strategic-form) game
#' @description \code{extensive_form} defines an extensive-form game and draws
#'     a game tree.
#' @details This function defines an extensive-form game and draws the game
#'     tree.
#' @param players A list of players. Each element of the list must be a
#'     character string  or a vector of character strings at a specific depth of
#'      the tree. Terminal nodes, where payoffs are displayed, must be specified
#'       as \code{NA}.
#' @param actions A list of actions. Each element of the list must be a vector
#'     of character strings that corresponds to a specific player node.
#' @param payoffs A named list of payoffs. Each element of the list must be a
#'     numeric vector of payoffs for a player. The names of the elements must
#'     match the names of the players specified  by \code{players}.
#' @param payoffs2 A list of payoffs. Each element of the list must be a numeric
#'     vector of payoffs for a terminal node.
#' @param show_tree A logical value. If \code{TRUE}, the game tree will be
#'     displayed. Default is \code{TRUE}.
#' @param show_node_id A logical value. If \code{TRUE}, the node numbers are
#'     displayed in the figure. Default is \code{TRUE}.
#' @param info_sets A list of information sets.
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
#' @return An object of "extensive_form" class, which defines an extensive-form
#'     (or sequential) game.
#' @importFrom magrittr %>%
#' @include set_nodes.R set_paths.R extensive_strategy.R
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' g1 <- extensive_form(
#'   players = list("Kamijo",
#'                  rep("Yanai", 2),
#'                  rep(NA, 4)),
#'   actions = list(c("U", "D"),
#'                   c("U'", "D'"), c("U''", "D''")),
#'   payoffs = list(Kamijo = c(0, 2, 1, 3),
#'                  Yanai  = c(0, 1, 2, 1)))
#'
#' g2 <- extensive_form(
#'   players = list("f",
#'                  c("m", "m"),
#'                  rep(NA, 4)),
#'   actions = list(c("ballet", "baseball"),
#'                  c("ballet", "baseball"), c("ballet", "baseball")),
#'   payoffs = list(f = c(2, 0, 0, 1),
#'                  m = c(1, 0, 0, 2)),
#'   show_node_id = FALSE)
#'
#' g3 <- extensive_form(
#'   players = list("p1",
#'                  rep("p2", 3),
#'                  rep(NA, 6)),
#'   actions = list(c("C", "D", "E"),
#'                  c("F", "G"), c("H", "I"), c("J", "K")),
#'   payoffs = list(p1 = c(3, 1, 1, 2, 2, 1),
#'                  p2 = c(0, 0, 1, 1, 2, 3)),
#'   direction = "down",
#'   show_node_id = TRUE)
#'
#' g4 <- extensive_form(
#'   players = list("child",
#'                  c(NA, "parent"),
#'                  c(NA, NA)),
#'    actions = list(c("give up", "keep asking"),
#'                   c("leave",   "buy")),
#'    payoffs   = list(child  = c(0, -10, 10),
#'                     parent = c(5, -10,  2)))
#'
#' g5 <- extensive_form(
#'   players = list("Kamijo",
#'                  c("Yanai", NA),
#'                  c("Kamijo", NA),
#'                  c(NA, NA)),
#'   actions = list(c("P", "T"),
#'                    c("P'", "T'"),
#'                    c("P''", "T''")),
#'   payoffs = list(Kamijo = c(0, 1, 5, 3),
#'                  Yanai = c(0, 2, 4, 1)),
#'   direction = "horizontal")
extensive_form <- function(
  players,        # list, one vector for each sequence
  actions,        # list, one vector for each node
  payoffs = NULL, # named list, one vector for each player. Names must match the unique names of the players
  payoffs2 = NULL,# list, one vector for each terminal node
  show_tree = TRUE,
  show_node_id = TRUE,
  info_sets = NULL,
  info_line = "solid",
  direction = "down",
  color_palette = "Set1",
  family = "sans",
  size_player = 4,
  size_payoff = 4,
  size_action = 4,
  size_node_id = 4,
  size_terminal = 2,
  scale = NULL) {

  direction <- match.arg(direction,
                         choices = c("right", "up", "down",
                                     "bidirectional",
                                     "horizontal", "vertical"))

  x_s <- x_m <- x_e <- y_s <- y_m <- y_e <- id <- player <- NULL
  node_from <- s <- NULL

  if (!is.null(payoffs2)) {
    u_players <- players %>%
      unlist() %>%
      stats::na.omit() %>%
      unique()
    n_players <- length(u_players)
    n_tmnl <- length(payoffs2)
    p_matrix <- matrix(NA, ncol = n_players, nrow = n_tmnl)
    for (i in 1:n_tmnl) {
      for (p in 1:n_players) {
        p_matrix[i, p] <- payoffs2[[i]][p]
      }
    }
    payoffs <- list()
    for (p in 1:n_players) {
      payoffs[[p]] <- p_matrix[, p]
    }
    names(payoffs) <- u_players
  }

  if (!is.null(scale)) {
    if (!is.numeric(scale) | scale <= 0)
      stop("scale must be a positive number")

    size_player <- size_player * scale
    size_payoff <- size_payoff * scale
    size_action <- size_action * scale
    size_node_id <- size_node_id * scale
    size_terminal <- size_terminal * scale
  }

  # count the number of choices at each node
  n_choice <- rep(NA, length(players)) %>%
    as.list()
  k <- 1
  for (i in 1:length(n_choice)) {
    v1 <- players[[i]]
    nc <- rep(NA, length(v1))
    for (j in seq_along(v1)) {
      if (is.na(v1[j])) {
        nc[j] <- 0
      } else {
        nc[j] <- length(actions[[k]])
        k <- k + 1
      }
    }
    n_choice[[i]] <- nc
  }

  ## set nodes
  df_node <- set_nodes(players, n_choice, payoffs, direction)

  ## set branches
  players_vec <- unlist(players)
  players_vec <- players_vec[!is.na(players_vec)]
  df_path <- set_paths(players_vec, n_choice, actions)

  ## add positions to the branches
  df_path <- df_path %>%
    dplyr::mutate(x_s = df_node$x[df_path$node_from],
                  x_e = df_node$x[df_path$node_to],
                  y_s = df_node$y[df_path$node_from],
                  y_e = df_node$y[df_path$node_to],
                  x_m = 2/3 * x_s + 1/3  * x_e,
                  y_m = 1/2 * y_s + 1/2 * y_e,
                  y_m = ifelse(y_m == y_e, y_m + 3, y_m))

  ## draw the game tree
  tree <- draw_tree(df_path = df_path,
                    df_node = df_node,
                    direction = direction,
                    show_node_id = show_node_id,
                    info_sets = info_sets,
                    info_line = info_line,
                    color_palette = color_palette,
                    family = family,
                    size_player = size_player,
                    size_payoff = size_payoff,
                    size_action = size_action,
                    size_node_id = size_node_id,
                    size_terminal = size_terminal,
                    scale = scale)

  if (show_tree) {
    plot(tree)
  }

  if (!is.null(info_sets)) {

    ## check if a node is included in only one information set
    info_sets_elements <- unlist(info_sets)
    info_sets_elements_u <- unique(info_sets_elements)
    if (length(info_sets_elements) != length(info_sets_elements_u))
      stop("A node can belong to only one information set.")

    ## check if actions are compatible with info sets
    for (i in 1:length(info_sets)) {
      n_nodes <- length(info_sets[[i]])
      if (n_nodes == 1) next
      action_list <- list()
      for (j in 1:n_nodes) {
        action_list[[j]] <- df_path %>%
          dplyr::filter(node_from == info_sets[[i]][j]) %>%
          dplyr::pull(s)
      }
      for (j in 2:n_nodes) {
        if (!setequal(action_list[[j]], action_list[[j - 1]]))
          stop("Different sets of actions are given at different nodes within an information set.")
      }
    }

    ## find players corresponding to info sets
    n_info_sets <- length(info_sets)
    info_sets_player <- rep(NA, n_info_sets)
    for (i in 1:n_info_sets) {
      info_node <- info_sets[[i]][1]
      info_sets_player[i] <- df_node %>%
        dplyr::filter(id == info_node) %>%
        dplyr::pull(player)
    }
  } else {
    info_sets_player <- NULL
  }

  #node_to_play <- list()
  #u_players <- unique(players_vec)
  #for (i in 1:length(u_players)) {
  #  node_to_play[[i]] <- df_node %>%
  #    dplyr::filter(player == u_players[i]) %>%
  #    dplyr::pull(id)
  #}
  #names(node_to_play) <- u_players

  #strategies <- extensive_strategy(player = players_vec,
  #                                 action_list = actions,
  #                                 info_sets = info_sets,
  #                                 info_sets_player = info_sets_player,
  #                                 node_to_play = node_to_play)



  value <- list(player = players_vec,
                action = actions,
#                strategy = strategies$strategy,
#                action_prof = strategies$action_profile,
                payoff = payoffs,
                info_sets = info_sets,
                info_sets_player = info_sets_player,
                tree = tree,
                data = list(node = df_node,
                            path = df_path),
                tree_params = list(info_line = info_line,
                                   direction = direction,
                                   show_node_id = show_node_id,
                                   color_palette = color_palette,
                                   family = family,
                                   size_player = size_player,
                                   size_payoff = size_payoff,
                                   size_action = size_action,
                                   size_node_id = size_node_id,
                                   size_terminal = size_terminal,
                                   scale = scale))

  structure(value, class = "extensive_form")
}
