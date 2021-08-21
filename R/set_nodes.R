#' @title Define the node positions of a game tree
#' @description \code{set_nodes} determines the position of nodes in a game tree.
#' @details Create a data frame necessary to draw a game tree
#' @param players A list of players.  Each element of the list must correspond to each game node, except for
#'   the terminal (payoff) nodes.
#' @param n_node A vector of the number of nodes at each sequence, including the terminal (payoff) nodes.
#' @param n_choice A list of the number of choices at each node. An element of the list must
#'   correspond to each sequence (including the terminal node as the final sequence). Each element must
#'   be a numeric vector whose length equals the number of nodes at the specific sequence.
#' @return A data frame containing the positions of each node on x and y axes.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
set_nodes <- function(players, n_node, n_choice) {

  n_seq <- length(n_node)

  ## position on the vertical axis
  height <- 100
  ypos <- NULL
  zero_count <- 0
  keep <- TRUE
  for (i in 1:n_seq) {
    height_sub <- height / (n_node[i] + zero_count)
    pos <- seq(from       = height_sub / 2,
               by         = height_sub,
               length.out = n_node[i] + zero_count)
    pos <- pos[keep]
    ypos <- c(ypos, pos)
    zero_count <- sum(n_choice[[i]] == 0)
    n_choice_vec <- n_choice[[i]]
    keep <- NULL
    for (i in seq_along(n_choice_vec)) {
      if (n_choice_vec[i] == 0) {
        keep <- c(keep, FALSE)
      } else {
        keep <- c(keep, rep(TRUE, n_choice_vec[i]))
      }
    }
  }

  ## position on the horizontal axis
  x <- seq(from = 0, to = 100, length.out = n_seq)
  xpos <- rep(x, n_node)

  n_choice_flat <- unlist(n_choice)
  player_vec <- unlist(players)
  player_node <- rep(NA, sum(n_node))
  j <- 1
  for (i in seq_along(n_choice_flat)) {
    if (n_choice_flat[i] != 0) {
      player_node[i] <- player_vec[j]
      j <- j + 1
    }
  }

  df <- data.frame(
    x = xpos,
    y = -1 * ypos,
    player = player_node
  )
  df$type = ifelse(is.na(df$player), "payoff", "play")
  df$id   = 1:nrow(df)

  return(df)
}
