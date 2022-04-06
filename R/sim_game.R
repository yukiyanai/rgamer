#' @title Plays a normal-form game by simulation
#' @description \code{sim_game()} simulates plays expected in a normal-form
#'     game.
#' @details Simulate plays expected in a normal-form game defined by
#'     \code{normal_form()}.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param n_samples A positive integer specifying the number of samples to be
#'     simulated.
#' @param n_periods A positive integer specifying how many times the game is
#'     played within each sample.
#' @param type A character string to tell what kind of simulation should be run.
#'     The available options are \code{"br"}, \code{"sbr"}, \code{"abr"}, and
#'     \code{"imitation"}. With \code{"br"}, each player chooses the best
#'     response to the opponent's choice in the previous period. With
#'     \code{"sbr"}, each player chooses the softly best response to the
#'     opponent's choice in the previous periods, With \code{"abr"}, each
#'     player alternately chooses the best response to the other player's
#'     previous action. With \code{"imitation"}, each player imitates the
#'     opponent's choice in the previous period. Players randomly choose their
#'     strategies or the first period in each of these options.
#' @param init1 Player 1's first strategy. If not specified, a strategy is
#'     randomly selected from the player's strategy set.
#' @param init2 Player 2's first strategy. If not specified, a strategy is
#'     randomly selected from the player's strategy set.
#' @param omega A numeric value between 0 and 1 to control the degree of inertia
#'     in each player's behavior. If \code{omega = 1}, each player does not
#'     change their choices over time. If \code{omega = 0}, which is the default
#'      value, each player does not stick to their previous choice at all.
#' @param eta A numeric value between 0 and 1 to control the degree of
#'     randomness in each player's behavior. If \code{eta = 1}, each player
#'     chooses their strategy completely at random. If \code{eta = 0}, each
#'     player chooses the best strategy based on the opponent's behavior in the
#'     previous period.
#' @param lambda A positive value controlling the weight of the best response to
#'      the previous move of the opponent.
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @param plot_range_y Choose the range of vertical axis for plots. Available
#'     choices are \code{"fixed"}, \code{"full"} and \code{"free"}.
#'     If \code{plot_range_y = "free"}, the range of y-axis depends on
#'     simulation results.  If \code{plot_range_y = "full"}, The range
#'     defined in \code{game} is used for each player, which can be different
#'     between players. With \code{"fixed"}, the same y-axis is used for both
#'     players.
#' @return A data frame of simulation results.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
sim_game <- function(game,
                     n_samples,
                     n_periods,
                     type = "br",
                     init1 = NULL,
                     init2 = NULL,
                     omega = 0,
                     eta = 0.1,
                     lambda = 1,
                     cons1 = NULL,
                     cons2 = NULL,
                     plot_range_y = NULL) {

  play1 <- play2 <- period <- player <- strategy <- d1 <- NULL

  if (class(game) != "normal_form") stop("Please provide a game defined by normal_form().")

  if (omega < 0 | omega > 1) stop(message("The value for omega must be in [0, 1]."))

  type <- match.arg(type, choices = c("br", "sbr", "abr", "imitation"))

  if (is.null(cons1)) cons1 <- game$constants[[1]]
  if (is.null(cons2)) cons2 <- game$constants[[2]]


  if (type == "br") {
    df_list <- list()
    for (i in 1:n_samples) {
      d1 <-  sim_game_br(game,
                         n_periods = n_periods,
                         omega = omega,
                         cons1 = cons1,
                         cons2 = cons2,
                         init1 = init1,
                         init2 = init2)
      d1$sample <- i
      df_list[[i]] <- d1
    }
    df <- dplyr::bind_rows(df_list)

  } else if (type == "sbr") {
    df_list <- list()
    for (i in 1:n_samples) {
      d1 <- sim_game_sbr(game,
                         n_periods = n_periods,
                         omega = omega,
                         lambda = lambda,
                         cons1 = cons1,
                         cons2 = cons2,
                         init1 = init1,
                         init2 = init2)
      d1$sample <- i
      df_list[[i]] <- d1
    }
    df <- dplyr::bind_rows(df_list)

  } else if (type == "abr") {
    df_list <- list()
    for (i in 1:n_samples) {
      d1 <- sim_game_abr(game,
                         n_periods = n_periods,
                         omega = omega,
                         cons1 = cons1,
                         cons2 = cons2,
                         init1 = init1,
                         init2 = init2)
      d1$sample <- i
      df_list[[i]] <- d1
    }
    df <- dplyr::bind_rows(df_list)

  } else if (type == "imitation") {
    df_list <- list()
    for (i in 1:n_samples) {
      d1 <- sim_game_imitation(game,
                               n_periods = n_periods,
                               omega = omega,
                               eta = eta,
                               cons1 = cons1,
                               cons2 = cons2,
                               init1 = init1,
                               init2 = init2)
      d1$sample <- i
      df_list[[i]] <- d1

    }
    df <- dplyr::bind_rows(df_list)

  }

  df_longer <- df %>%
    tidyr::pivot_longer(play1:play2,
                        names_to = "player",
                        values_to = "strategy") %>%
    dplyr::select(sample, period, player, strategy) %>%
    dplyr::mutate(player = ifelse(player == "play1",
                                  game$player[1],
                                  game$player[2]))

  p <- plot_sim(df_longer,
                game = game,
                plot_range_y = plot_range_y)

  return(list(data = df_longer,
              plot_mean = p$plot_mean,
              plot_samples = p$plot_samples))
}
