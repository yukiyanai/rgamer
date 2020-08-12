#' @title Find best responses to the opponent's actions
#' @description \code{find_best_response()} finds the best responses to each of the opponent's strategy.
#' @return A data frame containing the pair of the best responses of two players.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @importFrom magrittr %>%
find_best_response <- function(game) {

  players <- game$player
  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  df <- game$df

  ## find the best responses of Player 1 (row player) against Player 2 (column player)
  df_1 <- data.frame(NULL)
  for (j in seq_along(s2)) {
    df_1 <- df %>%
      dplyr::filter(column == j) %>%
      dplyr::filter(p1 == max(p1)) %>%
      dplyr::bind_rows(df_1)
  }
  df_1 <- df_1 %>%
    dplyr::rename(best_response = s1,
                  against = s2,
                  payoff = p1) %>%
    dplyr::select(-p2) %>%
    dplyr::arrange(row, column) %>%
    dplyr::mutate(player = players[1],
                  pid = 1)

  ## find the best responses of Player 2 (column player) against Player 1 (row player)
  df_2 <- data.frame(NULL)
  for (i in seq_along(s1)) {
    df_2 <- df %>%
      dplyr::filter(row == i) %>%
      dplyr::filter(p2 == max(p2)) %>%
      dplyr::bind_rows(df_2)
  }
  df_2 <- df_2 %>%
    dplyr::rename(best_response = s2,
                  against = s1,
                  payoff = p2) %>%
    dplyr::select(-p1) %>%
    dplyr::arrange(row, column) %>%
    dplyr::mutate(player = players[2],
                  pid = 2)

  out <- dplyr::bind_rows(df_1, df_2) %>%
    dplyr::select(player, best_response, against, payoff, row, column, pid)

  return(out)
}
