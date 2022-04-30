#' @title Simulates fictitious plays of a game
#' @description \code{sim_fict()} simulates a fictitious play of a game
#' @details Simulate fictitious plays of a normal-form game defined by
#'     \code{normal_form()}.
#' @param n_samples A positive integer specifying the number of samples to be
#'     simulated.
#' @param plot_range_y Choose the range of vertical axis for plots. Available
#'     choices are \code{"fixed"}, \code{"full"} and \code{"free"}.
#'     If \code{plot_range_y = "free"}, the range of y-axis depends on
#'     simulation results.  If \code{plot_range_y = "full"}, The range
#'     defined in \code{game} is used for each player, which can be different
#'     between players. With \code{"fixed"}, the same y-axis is used for both
#'     players.
#' @param plot_id An integer between 1 and n_samples to specify for which
#'     sample, playing history should be displayed. If \code{plot_id = NULL},
#'     plot_B and plot_P will be NULL.
#' @inheritParams sim_fict_one
#' @return A list containing (1) a data frames of strategies chosen by each
#'     player,  (2) a single long data frame of (1)'s data frames combined,
#'     (3) a list of each player's belief about the opponent's behavior
#'     (data frames), (4) a list of probability of each strategy being chosen
#'     (data frames), and (5) three plots of simulation result.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
sim_fict <- function(game,
                     n_samples = 1,
                     n_periods = 50,
                     lambda = 1,
                     init = NULL,
                     sigma = 0,
                     plot_range_y = NULL,
                     plot_id = 1) {

  player <- player1 <- player2 <- period <- strategy <- NULL
  belief <- probability <- NULL

  data_list <- B1_list <- B2_list <- P1_list <- P2_list <- list()

  for (i in 1:n_samples) {
    res <- sim_fict_one(game = game,
                        n_periods = n_periods,
                        lambda = lambda,
                        init = init,
                        sigma = sigma)
    df <- res$data
    df$sample <- i
    data_list[[i]] <- df
    B1_list[[i]] <- res$belief$B1
    B2_list[[i]] <- res$belief$B2
    P1_list[[i]] <- res$choice_prob$P1
    P2_list[[i]] <- res$choice_prob$P1
  }

  data_long <- dplyr::bind_rows(data_list) |>
    tidyr::pivot_longer(player1:player2,
                        names_to = "player",
                        values_to = "strategy") |>
    dplyr::select(sample, period, player, strategy) |>
    dplyr::mutate(player = ifelse(player == "player1",
                                  game$player[1],
                                  game$player[2]))

  p <- plot_sim(data_long,
                game = game,
                plot_range_y = plot_range_y)

  if (is.null(plot_id)) {
    plt_B <- plt_P <- NULL
  } else {
    # Plot beliefs
    p_B1 <- B1_list[[plot_id]] |>
      tidyr::pivot_longer(cols = -period,
                          values_to = "belief",
                          names_to = "strategy") |>
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = belief,
                                   color = strategy,
                                   linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_brewer(name = paste("strategy of\n", game$player[2]),
                                  palette = "Dark2") +
      ggplot2::scale_linetype_discrete(name = paste("strategy of\n", game$player[2])) +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(subtitle = paste("Belief of", game$player[1]))

    p_B2 <- B2_list[[plot_id]] |>
      tidyr::pivot_longer(cols = -period,
                          values_to = "belief",
                          names_to = "strategy") |>
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = belief,
                                   color = strategy,
                                   linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_brewer(name = paste("strategy of\n", game$player[1]),
                                  palette = "Dark2") +
      ggplot2::scale_linetype_discrete(name = paste("strategy of\n", game$player[1])) +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(subtitle = paste("Belief of", game$player[2]))

    plt_B <- patchwork::wrap_plots(p_B1, p_B2)

    # Plot choice probabilities
    p_P1 <- P1_list[[plot_id]] |>
      tidyr::pivot_longer(cols = -period,
                          values_to = "probability",
                          names_to = "strategy") |>
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = probability,
                                   color = strategy,
                                   linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(subtitle = game$player[1])

    p_P2 <- P2_list[[plot_id]] |>
      tidyr::pivot_longer(cols = -period,
                          values_to = "probability",
                          names_to = "strategy") |>
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = probability,
                                   color = strategy,
                                   linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(subtitle = game$player[2])

    plt_P <- patchwork::wrap_plots(p_P1, p_P2)
  }

  return(list(data = data_list,
              data_long = data_long,
              belief = list(B1 = B1_list,
                            B2 = B2_list),
              choice_prob = list(P1 = P1_list,
                                 P2 = P2_list),
              plot_mean = p,
              plot_B = plt_B,
              plot_P = plt_P))
}
