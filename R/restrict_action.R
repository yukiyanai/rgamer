#' @title Restrict actions of an extensive-form game
#' @description \code{restrict_actions} restricts some actions of an extensive-
#'     form game defined by \code{extensive_form()}.
#' @param game An "extensive_form" class object created by
#'     \code{extensive_form()}.
#' @param action A named list of actions that can be chosen by players. The
#'     names must be node names: "n1", "n2", etc. A player can choose any action
#'     at the nodes not listed.
#' @return A "restricted_game" class object in which players have to choose the
#'    actions specified by the user.
#' @importFrom magrittr %>%
#' @include extensive_strategy.R
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
restrict_action <- function(game, action) {

  id <- node_from <- node_to <- info_sets <- NULL
  s <- player <- linetype <- NULL

  n_spec <- length(action)

  nodes <- names(action) %>%
    stringr::str_replace("n", "") %>%
    as.integer()

  action_vec <- unlist(action)

  ## check if specified actions are available
  for (i in 1:n_spec) {
    if (!(action_vec[i] %in% game$action[[nodes[i]]])) {
      stop(paste(action_vec[i], "is not an avilable action for", names(action)[i]))
    }

    type <- game$data$node %>%
      dplyr::filter(id == nodes[i]) %>%
      dplyr::pull(type)
    if (type != "play") stop(paste(names(action)[i], "is a terminal node"))
  }

  df_path <- game$data$path
  df_path$linetype <- "1"
  df_path$bold <- FALSE

  for (i in 1:n_spec) {
    for (j in 1:nrow(df_path)) {
      if (df_path$node_from[j] == nodes[i]) {
        if (df_path$s[j] == action_vec[i]) {
          df_path$bold[j] <- TRUE
        } else {
          df_path$linetype[j] <- "2"
        }
      }
    }
  }

  new_tree <- draw_tree(df_path = df_path,
                        df_node = game$data$node,
                        df_sol = NULL,
                        show_node_id = game$tree_params$show_node_id,
                        info_sets = game$info_sets,
                        info_line = game$tree_params$info_line,
                        direction = game$tree_params$direction,
                        color_palette = game$tree_params$color_palette,
                        family = game$tree_params$family,
                        size_player = game$tree_params$size_player,
                        size_payoff = game$tree_params$size_payoff,
                        size_action = game$tree_params$size_action,
                        size_node_id = game$tree_params$size_node_id,
                        size_terminal = game$tree_params$size_terminal,
                        scale = game$tree_params$scale,
                        restriction = TRUE)


  actions <- game$action
  for (i in 1:n_spec) {
    actions[[nodes[i]]] <- action_vec[i]
  }

  if (!is.null(game$info_sets)) {

    ## check if a node is included in only one information set
    info_sets_elements <- unlist(game$info_sets)
    info_sets_elements_u <- unique(info_sets_elements)
    if (length(info_sets_elements) != length(info_sets_elements_u))
      stop("A node can belong to only one information set.")

    ## check if actions are compatible with info sets
    for (i in 1:length(game$info_sets)) {
      n_nodes <- length(game$info_sets[[i]])
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

  node_to_play <- list()
  u_players <- unique(unlist(game$player))
  for (i in 1:length(u_players)) {
    node_to_play[[i]] <- game$data$node %>%
      dplyr::filter(player == u_players[i]) %>%
      dplyr::pull(id)
  }

  df_path <- df_path %>%
    dplyr::filter(linetype == "1") %>%
    dplyr::select(!linetype)

  reaches <- c(1, dplyr::pull(df_path, node_to))

  df_node <- game$data$node %>%
    dplyr::filter(id %in% reaches)

  payoffs <- game$payoff
  for (p in seq_along(u_players)) {
    payoffs[[p]] <- df_node %>%
      dplyr::pull(u_players[p]) %>%
      stats::na.omit() %>%
      as.vector()
  }

  strategies <- extensive_strategy(player = unlist(game$player),
                                   action_list = actions,
                                   info_sets = game$info_sets,
                                   info_sets_player = info_sets_player,
                                   node_to_play = node_to_play)

  value <- list(player = game$player,
                action = actions,
                strategy = strategies$strategy,
                action_prof = strategies$action_profile,
                payoff = payoffs,
                info_sets = game$info_sets,
                data = list(node = df_node,
                            path = df_path),
                tree = new_tree,
                tree_params = game$tree_params)


  structure(value, class = "restricted_game")
}
