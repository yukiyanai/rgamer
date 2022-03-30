#' @title Define the node positions of a game tree
#' @description \code{set_nodes} determines the position of nodes in a tree of
#'     an extensive-form game.
#' @param players A list of players.  Each element of the list must correspond
#'     to each game node, except for the terminal (payoff) nodes.
#' @param n_choice A list of the number of choices at each node. An element of
#'     the list must correspond to each sequence (including the terminal node as
#'      the final sequence). Each element must be a numeric vector whose length
#'      equals the number of nodes at the specific sequence.
#' @param payoffs A named list of payoffs. Each element of the list must be a
#'     numeric vector of payoffs for a player. The names of the elements must
#'     match the names of the players specified  by \code{players}.
#' @param direction The direction to which a game tree grows.
#'     The value must be one of:
#'     \code{"right"},
#'     \code{"up"},
#'     \code{"down"},
#'     \code{"bidirectional"},
#'     \code{"horizonal"}, and
#'     \code{"vertical"}.
#'     Default is \code{"down"}.
#' @return A data frame containing the position of each node on x-y plane.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
set_nodes <- function(players, n_choice, payoffs, direction = "down") {

  type <- id <- NULL

  n_node <- sapply(n_choice, length)
  n_seq <- length(n_node)

  ## position along the vertical axis
  height <- 100
  ypos <- NULL
  zero_count <- 0
  keep <- TRUE
  for (i in 1:n_seq) {
    height_sub <- height / (n_node[i] + zero_count)
    pos <- seq(from = height_sub / 2,
               by = height_sub,
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

  ## position along the horizontal axis
  x <- seq(from = 0, to = 100, length.out = n_seq)
  xpos <- rep(x, n_node)

  n_choice_flat <- unlist(n_choice)
  player_vec <- unlist(players)

  df <- data.frame(x = xpos,
                   y = -1 * ypos,
                   player = player_vec)
  df$type = ifelse(is.na(df$player), "payoff", "play")
  df$id   = 1:nrow(df)

  if (direction %in% c("horizontal", "vertical")) {
    x_types <- unique(df$x)
    df_tmp <- tibble::tibble(NULL)
    for (z in x_types) {
      df_sub <- df %>%
        dplyr::filter(x == z)
      n_y <- df_sub$y %>% length()
      df_sub$y <- seq(from = 0, to = -100, length.out = n_y)
      df_tmp <- dplyr::bind_rows(df_tmp, df_sub)
    }
    df <- df_tmp
  }

  df_payoff <- as.data.frame(payoffs)
  payoff_label <- rep(NA, nrow(df_payoff))
  for (i in 1:nrow(df_payoff)) {
    payoff_label[i] <- paste(df_payoff[i, ], collapse = ", ")
  }
  payoff_label <- paste0("(", payoff_label, ")")

  df_payoff <- df %>%
    dplyr::filter(type == "payoff") %>%
    dplyr::bind_cols(df_payoff) %>%
    dplyr::mutate(payoff = payoff_label)

  df_play <- df %>%
    dplyr::filter(type == "play")

  dplyr::bind_rows(df_play, df_payoff) %>%
    dplyr::arrange(id)

}
