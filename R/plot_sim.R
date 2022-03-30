#' @title Plot the simulation results
#' @description \code{plot_sim()} creates ggplot graphs for simulations.
#' @details Creates ggplot graphs for simulation run by \code{sim_game()}.
#'     For "matrix" type game, the ratio of each strategy is displayed.
#'     For other types, the graph showing the mean across sample and one showing
#'     the strategy played in each sample are displayed.
#' @param x A data frame containing the simulation results.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param plot_range_y Choose the range of vertical axis for plots. Available
#'     choices are \code{"fixed"}, \code{"full"} and \code{"free"}.
#'     If \code{plot_range_y = "free"}, the range of y-axis depends on
#'     simulation results.  If \code{plot_range_y = "full"}, The range
#'     defined in \code{game} is used for each player, which can be different
#'     between players. With \code{"fixed"}, the same y-axis is used for both
#'     players.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @import ggplot2
#' @importFrom magrittr %>%
#' @noRd
plot_sim <- function(x,
                     game,
                     plot_range_y = "free") {

  player <- period <- strategy <- play <- ratio <- m <- NULL

  plot_range_y <- match.arg(plot_range_y,
                            choices = c("fixed", "free", "full"))

  n_samples <- length(unique(x$sample))

  if (game$type == "matrix") {

    # Player 1: ratio of each strategy
    df1 <- NULL
    for (s in game$strategy$s1) {
      df_tmp <- x %>%
        dplyr::filter(player == game$player[1]) %>%
        dplyr::group_by(period) %>%
        dplyr::summarize(play = s,
                         ratio = sum(strategy == s) / n_samples) %>%
        dplyr::rename(strategy = play)

      df1 <- dplyr::bind_rows(df1, df_tmp)
    }

    p1_1 <- ggplot2::ggplot(df1,
                            ggplot2::aes(x = period,
                                         y = ratio,
                                         color = strategy,
                                         linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "period",
                    y = "ratio",
                    subtitle = game$player[1]) +
      ggplot2::scale_color_brewer(palette = "Dark2")

    # Player 2: ratio of each strategy
    df2 <- NULL
    for (s in game$strategy$s2) {
      df_tmp <- x %>%
        dplyr::filter(player == game$player[2]) %>%
        dplyr::group_by(period) %>%
        dplyr::summarize(play = s,
                         ratio = sum(strategy == s) / n_samples) %>%
        dplyr::rename(strategy = play)
      df2 <- dplyr::bind_rows(df2, df_tmp)
    }

    p1_2 <- ggplot2::ggplot(df2,
                            ggplot2::aes(x = period,
                                         y = ratio,
                                         color = strategy,
                                         linetype = strategy)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "period",
                    y = "ratio",
                    subtitle = game$player[2]) +
      ggplot2::scale_color_brewer(palette = "Dark2")

    # Wrap two plots by patchwork
    p1 <- patchwork::wrap_plots(p1_1, p1_2)

    # plot_sample is NULL for discrete-choice (matrix) games
    p2 <- NULL

  } else {

    # Calculate the mean play across all samples
    df <- x %>%
      dplyr::group_by(period, player) %>%
      dplyr::summarize(m = mean(strategy),
                       .groups = "drop")

    # Player 1: mean play
    p1_1 <- df %>%
      dplyr::filter(player == game$player[1]) %>%
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = m)) +
      ggplot2::geom_path() +
      ggplot2::labs(x = "period",
                    y = "strategy\n (mean across samples)",
                    subtitle = game$player[1])

    # Player 2: mean play
    p1_2 <- df %>%
      dplyr::filter(player == game$player[2]) %>%
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = m)) +
      ggplot2::geom_path() +
      ggplot2::labs(x = "period",
                    y = "strategy\n (mean across samples)",
                    subtitle = game$player[2])

    # Player 1: each sample as a line
    p2_1 <- x %>%
      dplyr::filter(player == game$player[1]) %>%
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = strategy,
                                   group = sample)) +
      ggplot2::geom_path(alpha = 5 / n_samples) +
      ggplot2::labs(x = "period",
                    y = "strategy",
                    subtitle = game$player[1])

    # Player 2: each smample as a line
    p2_2 <- x %>%
      dplyr::filter(player == game$player[2]) %>%
      ggplot2::ggplot(ggplot2::aes(x = period,
                                   y = strategy,
                                   group = sample)) +
      ggplot2::geom_path(alpha = 5 / n_samples) +
      ggplot2::labs(x = "period",
                    y = "strategy",
                    subtitle = game$player[2])


    # Adjust y range
    if (plot_range_y == "fixed") {
      yl <- min(game$strategy$s1[1], game$strategy$s2[1])
      yu <- max(game$strategy$s1[2], game$strategy$s2[2])
      p1_1 <- p1_1 + ggplot2::ylim(yl, yu)
      p1_2 <- p1_2 + ggplot2::ylim(yl, yu)
      p2_1 <- p2_1 + ggplot2::ylim(yl, yu)
      p2_2 <- p2_2 + ggplot2::ylim(yl, yu)
    } else if (plot_range_y == "full") {
      p1_1 <- p1_1 +
        ggplot2::ylim(game$strategy$s1[1],
                      game$strategy$s1[2])
      p1_2 <- p1_2 +
        ggplot2::ylim(game$strategy$s2[1],
                      game$strategy$s2[2])
      p2_1 <- p2_1 +
        ggplot2::ylim(game$strategy$s1[1],
                      game$strategy$s1[2])
      p2_2 <- p2_2 +
        ggplot2::ylim(game$strategy$s2[1],
                      game$strategy$s2[2])
    }

    # Wrap two plots by patchwork
    p1 <- patchwork::wrap_plots(p1_1, p1_2)
    p2 <- patchwork::wrap_plots(p2_1, p2_2)

  }

  return(list(plot_mean = p1,
              plot_samples = p2))

}
