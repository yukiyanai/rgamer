#' @title Search a pair of the best responses of two players.
#' @description \code{gridsearch_br()} numerically finds a pair of best
#'     responses of a two-player normal-form game  by grid search.
#' @return A data frame containing the pair of the best responses of the
#'     players.
#' @param players A character vector specifying the two players of the game.
#' @param payoffs1 An R function describing Player 1's payoff.
#' @param payoffs2 An R function describing Player 2's payoff.
#' @param pars A character vector of length 2, which specifies the parameters
#'     each player chooses.
#' @param par1_lim A numerical vector of length 2, which specifies the range of
#'     the first parameter.
#' @param par2_lim A numerical vector of length 2, which specifies the range of
#'     the second parameter.
#' @param cons1 A named list of parameters contained in \code{payoffs1} that
#'     should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{payoffs2} that
#'     should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in \code{payoffs1}
#'     and \code{payoffs2} that should be treated as constants, if any. If
#'     \code{cons1} and \code{cons2} are exactly same, you can specify
#'     \code{cons_common} instead of \code{cons1} and \code{cons2}.
#' @param precision A natural number specifying the precision of numerical
#'     approximation. The value n approximately means that the approximation is
#'     correct up to the n-th decimal place. The default value is 1L.
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
gridsearch_br <- function(players,
                          payoffs1,
                          payoffs2,
                          pars,
                          par1_lim,
                          par2_lim,
                          cons1 = NULL,
                          cons2 = NULL,
                          cons_common = NULL,
                          precision = 1L) {

  range1 <- par1_lim[2] - par1_lim[1]
  range2 <- par2_lim[2] - par2_lim[1]
  grid1 <- 10^(ceiling(log10(range1)) - 3)
  grid2 <- 10^(ceiling(log10(range2)) - 3)

  df_list <- as_df_br(players = players,
                      payoffs1 = payoffs1,
                      payoffs2 = payoffs2,
                      pars = pars,
                      par1_lim = par1_lim,
                      par2_lim = par2_lim,
                      cons1 = cons1,
                      cons2 = cons2,
                      cons_common = cons_common)

  df1 <- df_list$df1
  df2 <- df_list$df2

  ## NE
  dif1 <- dif2 <- 1
  n_df1 <- nrow(df1)
  n_df2 <- nrow(df2)
  while (dif1 > 0 & dif2 > 0) {
    df1 <- df1 %>%
      dplyr::filter(x >= min(df2$x), x <= max(df2$x),
                    y >= min(df2$y), y <= max(df2$y))
    df2 <- df2 %>%
      dplyr::filter(x >= min(df1$x), x <= max(df1$x),
                    y >= min(df1$y), y <= max(df1$y))
    dif1 <- n_df1 - nrow(df1)
    dif2 <- n_df2 - nrow(df2)
    n_df1 <- nrow(df1)
    n_df2 <- nrow(df2)
  }

  df_sol <- dplyr::bind_rows(df1, df2) %>%
    dplyr::mutate(x = mean(x),
                  y = mean(y)) %>%
    dplyr::select(x, y) %>%
    dplyr::distinct()

  x <- df_sol %>% dplyr::pull(x)
  y <- df_sol %>% dplyr::pull(y)

  if (precision > 1) {
    df_sol <- gridsearch_br(
      players = players,
      payoffs1 = payoffs1,
      payoffs2 = payoffs2,
      pars = pars,
      par1_lim = c(max(x - 2 * grid1, par1_lim[1]),
                   min(x + 2 * grid1, par1_lim[2])),
      par2_lim = c(max(y - 2 * grid2, par2_lim[1]),
                   min(y + 2 * grid2, par2_lim[2])),
      cons1 = cons1,
      cons2 = cons2,
      cons_common = cons_common,
      precision = precision - 1)
  }

  return(df_sol)
}
