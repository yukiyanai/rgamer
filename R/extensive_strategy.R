#' @title Find strategies available in an extensive-form game
#' @description \code{extensive_strategy} finds the strategies for each player
#'     in an extensive-form game.
#' @param player A vector of players corresponding to play nodes in the game.
#' @param action_list A list of actions corresponding to play nodes in the game.
#' @param info_sets A list of information sets.
#' @param info_sets_player A vector of players corresponding to each element of
#'     info_sets.
#' @param node_to_play A list whose element shows which nodes each player plays.
#' @return A list of strategies and action_prof
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
extensive_strategy <- function(player,
                               action_list,
                               info_sets = NULL,
                               info_sets_player = NULL,
                               node_to_play = NULL) {

  u_player <- unique(player)
  n_seq <- length(action_list)

  action_num <- list()
  for (i in 1 : length(u_player)) {
    action_seq <- (1:n_seq)[player == u_player[i]]
    action_num[[i]] <- action_seq
  }
  names(action_num) <- u_player

 action_profiles <- strategy_list <- vector("list", length(u_player))
 for (p in 1 : length(u_player)) {
   action_num_p <- action_num[[p]]
   actions <- list()

   target_p <- u_player[p]
   if (target_p %in% info_sets_player) {
     node_to_play_kept <- NULL
     target_node <- node_to_play[[target_p]]
     target_info_sets <- info_sets[info_sets_player == target_p]
     for (j in 1 : length(target_info_sets)) {
       target_set <- target_info_sets[[j]]
       node_to_play_kept <- c(node_to_play_kept, target_set[1])
     }
     index_to_use <- which(
       target_node %in% node_to_play_kept | !(target_node %in% unlist(target_info_sets))
     )
     appear_on_action_prof <- rep(FALSE, length(target_node))
     appear_on_action_prof[index_to_use] <- TRUE
     action_num_p <- action_num_p[index_to_use]
   }

   for (s in action_num_p) {
     actions <- c(actions, list(action_list[[s]]))
   }

   actions_l <- sapply(actions, length)
   tot_rows <- prod(actions_l)

   action_p <- matrix(NA,
                      nrow = tot_rows,
                      ncol = length(actions))
   colnames(action_p) <- paste0("var", 1 : length(actions))
   denom <- 1
   for (l in 1 : length(actions)) {
     action_p[, l] <- rep(actions[[l]], each = tot_rows / actions_l[[l]])
     tot_rows <- tot_rows / actions_l[[l]]
   }

   a_list <- action_p |>
     t() |>
     as.data.frame() |>
     as.list()
   names(a_list) <- NULL

   b_list <- apply(action_p,
                 MARGIN = 1,
                 FUN = function(x) {
                   paste0("(", paste(x, collapse = ", "), ")")
                 }) |>
     as.list()

   action_profiles[[p]] <- a_list
   strategy_list[[p]] <- b_list

   if (target_p %in% info_sets_player) {
     action_p <- action_profiles[[p]]
     for (i in 1 : length(action_p)) {
       counter <- 1
       short_vec <- action_p[[i]]
       long_vec <- rep(NA, length(target_node))
       for (j in seq_along(long_vec)) {
         if (appear_on_action_prof[j]) {
           long_vec[j] <- short_vec[counter]
           counter <- counter + 1
         } else {
           long_vec[j] <- long_vec[j - 1]
         }
       }
       action_p[[i]] <- long_vec
      }
      action_profiles[[p]] <- action_p
    }
  }
  names(action_profiles) <- u_player

  for (i in 1 : length(u_player)) {
    strategy_list[[i]] <- unlist(strategy_list[[i]])
  }
  names(strategy_list) <- paste0("s", 1 : length(u_player))

  return(list(strategy = strategy_list,
              action_profile = action_profiles))
}
