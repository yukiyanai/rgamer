#' @title Find payoff for a specific set of actions.
#' @description \code{get_payoff_extensive} finds payoffs for a specified set of
#'     actions.
#' @param game An "extensive_form" class object created by
#'     \code{extensive_form()}.
#' @param actions A named list of actions to which the payoffs correspond. It
#'     must be a complete list of actions for each player node.
#' @return A vector of payoffs.
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
get_payoff_extensive <- function(game, actions) {

  id <- node_from <- node_to<- type <- s <- NULL

  if (is.null(names(actions))) {
    names(actions) <- unique(game$player)
  } else {
    for (i in 1:length(actions)) {
      if (!(names(actions)[i] %in% game$player))
        stop(paste(names(actions)[i], "is not a player of the game."))
    }
  }

  actions_avail <- game$action %>%
    unlist() %>%
    unique()

  for (i in 1:length(actions)) {
    actions_i <- actions[[i]]
    for (j in 1:length(actions_i)) {
      if (!(actions_i[j] %in% actions_avail))
        stop(paste(actions_i[j], "is not an available action of the game"))
    }
  }

  df_path <- game$data$path
  df_node <- game$data$node

  play_nodes <- df_path$node_from %>% unique()
  n_nodes <- length(play_nodes)

  df_list <- list()
  action_vec <- rep(NA, n_nodes)
  for (i in 1:n_nodes) {
    action_vec[i] <- actions[[game$player[i]]][1]
    actions[[game$player[i]]][1] <- NA
    actions[[game$player[i]]] <- stats::na.omit(actions[[game$player[i]]])
    df_list[[i]] <- df_path %>%
      dplyr::filter(node_from == play_nodes[i])
  }

  terminal_nodes <- df_node %>%
    dplyr::filter(type == "payoff") %>%
    dplyr::pull(id)

  ## play the game forward from the first node
  i <- 1
  while (i <= n_nodes) {
    next_node <- df_list[[i]] %>%
      dplyr::filter(s == action_vec[i]) %>%
      dplyr::pull(node_to)
    if (next_node %in% terminal_nodes) break
    else i <- next_node
  }

  df <- df_node %>% dplyr::filter(id == next_node)

  targets <- names(actions)
  payoffs <- df[1, targets] %>% unlist()

  return(payoffs)
}
