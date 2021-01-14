#' @title Plays a normal-form game by simulation
#' @description \code{simu_game()} simulates plays expected in a normal-form game.
#' @details Simulate plays expected in a normal-form game defined by \code{normal_form()}.
#' @param game An object of \code{normal_form} class defined by \code{normal_form()}.
#' @param n_samples A positive integer specifying the number of samples to be simulated.
#' @param n_periods A positive integer specifying how many times the game is played within each sample.
#' @param type A character string to tell what kind of simulation should be run.
#'   The available options are \code{"br"}, \code{"sbr"}, \code{"abr"}, and \code{"imitation"}.
#'   With \code{"br"}, each player chooses the best response to the opponent's choice in
#'   the previous period. With \code{"sbr"}, each player chooses the softly best response to
#'   the opponent's choice in the previous periods, With \code{"abr"}, each player
#'   alternately chooses the best response to the other player's previous action.
#'   With \code{"imitation"}, each player imitates the opponent's choice in the previous period.
#'   Players randomly choose their strategies or the first period in each of these options.
#' @param init1 Player 1's first strategy. If not specified, a strategy is randomly selected
#'   from the player's strategy set.
#' @param init2 Player 2's first strategy. If not specified, a strategy is randomly selected
#'   from the player's strategy set.
#' @param rho A numeric value in [0, 1] to control the degree of inertia in each player's behavior. If \code{rho = 1},
#'    each player does not change their choices over time. If \code{rho = 0}, which is the default value, each player does not stick to their
#'    previous choice at all.
#' @param lambda A positive value controlling the weight of the best response to the previous move of the opponent.
#' @param cons1 A named list of parameters contained in \code{game$payoff$p1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2} that should be treated as constants, if any.
#' @return A list of plays by player.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
simu_game <- function(game,
                      n_samples,
                      n_periods,
                      type = "br",
                      init1 = NULL,
                      init2 = NULL,
                      rho = 0,
                      lambda = 1,
                      cons1 = NULL,
                      cons2 = NULL) {

  if (class(game) != "normal_form") stop(message("Please provide a game defined by normal_form()"))

  if (rho < 0 | rho > 1) stop(message("The value for rho must be in [0, 1]."))

  type <- match.arg(type, choices = c("br", "sbr", "abr", "imitation"))


  if (type == "br") {
    df <- simu_game_br(game,
                       n_periods = n_periods,
                       rho = rho,
                       cons1 = cons1,
                       cons2 = cons2,
                       init1 = init1,
                       init2 = init2)
    df$sample <- 1
    if (n_samples > 1) {
      for (i in 2:n_samples) {
        df_i <- simu_game_br(game,
                             n_periods = n_periods,
                             rho = rho,
                             cons1 = cons1,
                             cons2 = cons2,
                             init1 = init1,
                             init2 = init2)
        df_i$sample <- i
        df <- dplyr::bind_rows(df, df_i)
      }
    }
  } else if (type == "sbr") {
    df <- simu_game_sbr(game,
                        n_periods = n_periods,
                        rho = rho,
                        lambda = lambda,
                        cons1 = cons1,
                        cons2 = cons2,
                        init1 = init1,
                        init2 = init2)
    df$sample <- 1
    if (n_samples > 1) {
      for (i in 2:n_samples) {
        df_i <- simu_game_sbr(game,
                              n_periods = n_periods,
                              rho = rho,
                              lambda = lambda,
                              cons1 = cons1,
                              cons2 = cons2,
                              init1 = init1,
                              init2 = init2)
        df_i$sample <- i
        df <- dplyr::bind_rows(df, df_i)
      }
    }

  } else if (type == "abr") {
    df <- simu_game_abr(game,
                        n_periods = n_periods,
                        rho = rho,
                        cons1 = cons1,
                        cons2 = cons2,
                        init1 = init1,
                        init2 = init2)
    df$sample <- 1
    if (n_samples > 1) {
      for (i in 2:n_samples) {
        df_i <- simu_game_abr(
          game,
          n_periods = n_periods,
          rho = rho,
          cons1 = cons1,
          cons2 = cons2,
          init1 = init1,
          init2 = init2)
        df_i$sample <- i
        df <- dplyr::bind_rows(df, df_i)
      }
    }
  } else if (type == "imitation") {
    df <- simu_game_imitation(game,
                              n_periods = n_periods,
                              rho = rho,
                              cons1 = cons1,
                              cons2 = cons2,
                              init1 = init1,
                              init2 = init2)
    df$sample <- 1
    if (n_samples > 1) {
      for (i in 2:n_samples) {
        df_i <- simu_game_imitation(
          game,
          n_periods = n_periods,
          rho = rho,
          cons1 = cons1,
          cons2 = cons2,
          init1 = init1,
          init2 = init2)
        df_i$sample <- i
        df <- dplyr::bind_rows(df, df_i)
      }
    }
  }

  return(df)
}
