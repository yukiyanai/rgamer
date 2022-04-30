#' @title Find best responses to the opponent's actions.
#' @description \code{find_best_response()} finds the best responses to each of
#'     the opponent's strategy in a normal-form game with discrete-choice
#'     strategies.
#' @return A data frame containing the pair of the best responses of two
#'     players.
#' @param game A "normal_form" class object created by \code{normal_form()} or
#'   a "sequential_form" class object created by \code{seq_form()}. The game's
#'   type must be "matrix".
#' @seealso \code{\link{normal_form}} and \code{\link{seq_form}}.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
find_best_response <- function(game) {

  column <- payoff1 <- payoff2 <- player <- best_response <- NULL
  against <- payoff <- pid <-NULL

  players <- game$player
  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  df <- game$df

  if (class(game) == "normal_form") {

    ## find the best responses of P1 (row player) v. P2 (column player)
    df_1 <- data.frame(NULL)
    for (j in seq_along(s2)) {
      df_1 <- df |>
        dplyr::filter(column == j) |>
        dplyr::filter(payoff1 == max(payoff1)) |>
        dplyr::bind_rows(df_1)
    }
    df_1 <- df_1 |>
      dplyr::rename(best_response = s1,
                    against = s2,
                    payoff = payoff1) |>
      dplyr::select(-payoff2) |>
      dplyr::arrange(row, column) |>
      dplyr::mutate(player = players[1],
                    pid = 1)

    ## find the best responses of P2 (column player) v. P1 (row player)
    df_2 <- data.frame(NULL)
    for (i in seq_along(s1)) {
      df_2 <- df |>
        dplyr::filter(row == i) |>
        dplyr::filter(payoff2 == max(payoff2)) |>
        dplyr::bind_rows(df_2)
    }
    df_2 <- df_2 |>
      dplyr::rename(best_response = s2,
                    against = s1,
                    payoff = payoff2) |>
      dplyr::select(-payoff1) |>
      dplyr::arrange(row, column) |>
      dplyr::mutate(player = players[2],
                    pid = 2)

    out <- dplyr::bind_rows(df_1, df_2) |>
      dplyr::select(player, best_response, against, payoff, row, column, pid)

  } else if (class(game) == "sequential_form") {

    df <- df |>
      dplyr::mutate(rc = paste(row, column, sep = ","))

    ## find the best responses of Player 2 (follower) against Player 1 (leader)
    df_2 <- data.frame(NULL)
    for (i in seq_along(s1)) {
      df_2 <- df |>
        dplyr::filter(row == i) |>
        dplyr::filter(payoff2 == max(payoff2)) |>
        dplyr::bind_rows(df_2)
    }
    df_2 <- df_2 |>
      dplyr::rename(best_response = s2,
                    against = s1,
                    payoff = payoff2) |>
      dplyr::select(-payoff1) |>
      dplyr::arrange(row, column) |>
      dplyr::mutate(player = players[2],
                    pid = 2)

    out <- df_2 |>
      dplyr::select(player, best_response, against, payoff, row, column, pid)
  }

  return(out)
}
