#' @title Simulates a fictitious play
#' @description \code{sim_fict()} simulates a fictitous play of a game
#' @details Simulate fictitious plays of a normal-form game defined by
#'     \code{normal_form()}.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param n_periods A positive integer specifying how many times the game is
#'     played.
#' @param lambda A positive real value representing the sensitivity to
#'     expected utilities. As \code{lambda} gets larger, a small difference in
#'     expected utility makes a big difference in choice probability.
#' @param init A list of initial levels of beliefs. The length of the list must
#'     be two. Each element must be a vector of values between 0 and 1
#'     representing a player's belief. If \code{init = NULL}, which is default,
#'     initial beliefs will be randomly assigned.
#' @param sigma A non-negative value determining the level of noise adherent to
#'     evaluation of payoffs.
#' @return A list containing (1) a list of data frames of strategies chosen by
#'     each player, (2) a single long data frame of (1), (3) a list of each
#'     player's attraction values for each strategy (data frames), (4) a list of
#'      probability of each strategy being chosen (data frames), and (5) a plot
#'      of the simulation result (ggplot object).
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
sim_fict <- function(game,
                     n_periods = 50,
                     lambda = 1,
                     init = NULL,
                     sigma = 0) {

  period <- belief <- strategy <- probability <- NULL

  pi1 <- game$mat$matrix1
  pi2 <- game$mat$matrix2

  n1 <- length(game$strategy$s1)
  n2 <- length(game$strategy$s2)

  ## B (belief), P (Probability) and choice
  B1 <- matrix(NA, nrow = n_periods + 1, ncol = n2) # Player 1's belief
  B2 <- matrix(NA, nrow = n_periods + 1, ncol = n1) # Player 2's belief
  P1 <- matrix(NA, nrow = n_periods + 1, ncol = n1) # Player 1's choice prob
  P2 <- matrix(NA, nrow = n_periods + 1, ncol = n2) # Player 2's choice prob
  choice1 <- rep(NA, n_periods + 1)
  choice2 <- rep(NA, n_periods + 1)


  if (is.null(init)) {
    tmp1 <- stats::runif(n2)
    tmp2 <- stats::runif(n1)
    B1[1, ] <- tmp1 / sum(tmp1)
    B2[1, ] <- tmp1 / sum(tmp2)
  } else {
    B1[1, ] <- init[[1]]
    B2[1, ] <- init[[2]]
  }

  for (t in (1:n_periods) + 1) {

    # Weight
    b1 <- B1[t - 1, ]
    b2 <- B2[t - 1, ]

    pi1m <- pi1 + matrix(stats::rnorm(n1 * n2, mean = 0, sd = sigma),
                         nrow = n1,
                         ncol = n2)
    pi2m <- pi2 + matrix(stats::rnorm(n1 * n2, mean = 0, sd = sigma),
                         nrow = n1,
                         ncol = n2)

    if (is.infinite(lambda)) {
      w1 <- pi1m %*% b1
      w2 <- t(b2) %*% pi2m

      w1 <- as.numeric(w1 == max(w1))
      w2 <- as.numeric(w2 == max(w2))
    } else {
      w1 <- exp(lambda * pi1m %*% b1)
      w2 <- exp(lambda * t(b2) %*% pi2m)
    }


    # Prob and choice
    P1[t, ] <- w1 / sum(w1)
    P2[t, ] <- w2 / sum(w2)

    c1 <- sample(1:n1, size = 1, prob = as.vector(w1))
    c2 <- sample(1:n2, size = 1, prob = as.vector(w2))

    choice1[t] <- c1
    choice2[t] <- c2

    # Update beliefs
    e1 <- numeric(n2)
    e1[c2] <- 1
    e2 <- numeric(n1)
    e2[c1] <- 1

    B1[t, ] <- ((t - 1) * b1 + e1) / t
    B2[t, ] <- ((t - 1) * b2 + e2) / t

  }

  # Make tibbles of the result
  df <- data.frame(player1 = game$strategy$s1[choice1[-1]],
                   player2 = game$strategy$s2[choice2[-1]],
                   period = 1:n_periods)

  colnames(B1) <- game$strategy$s2
  colnames(B2) <- game$strategy$s1
  colnames(P1) <- game$strategy$s1
  colnames(P2) <- game$strategy$s2

  B1 <- as.data.frame(B1[-1, ])
  B2 <- as.data.frame(B2[-1, ])
  P1 <- as.data.frame(P1[-1, ])
  P2 <- as.data.frame(P2[-1, ])

  B1$period <- B2$period <- P1$period <- P2$period <- 1:n_periods

  # Plot beliefs
  p_B1 <- B1 %>%
    tidyr::pivot_longer(cols = -period,
                        values_to = "belief",
                        names_to = "strategy") %>%
    ggplot2::ggplot(ggplot2::aes(x = period,
                                 y = belief,
                                 color = strategy,
                                 linetype = strategy)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(name = paste0(game$player[2], "'s\nstrategy"),
                                palette = "Dark2") +
    ggplot2::scale_linetype_discrete(name = paste0(game$player[2], "'s\nstrategy")) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(subtitle = paste0(game$player[1], "'s belief"))

    p_B2 <- B2 %>%
    tidyr::pivot_longer(cols = -period,
                        values_to = "belief",
                        names_to = "strategy") %>%
    ggplot2::ggplot(ggplot2::aes(x = period,
                                 y = belief,
                                 color = strategy,
                                 linetype = strategy)) +
    ggplot2::geom_line() +
      ggplot2::scale_color_brewer(name = paste0(game$player[1], "'s\nstrategy"),
                                  palette = "Dark2") +
      ggplot2::scale_linetype_discrete(name = paste0(game$player[1], "'s\nstrategy")) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(subtitle = paste0(game$player[2], "'s belief"))

  plt_B <- patchwork::wrap_plots(p_B1, p_B2)


  # Plot choice probabilities
  p_P1 <- P1 %>%
    tidyr::pivot_longer(cols = -period,
                        values_to = "probability",
                        names_to = "strategy") %>%
    ggplot2::ggplot(ggplot2::aes(x = period,
                                 y = probability,
                                 color = strategy,
                                 linetype = strategy)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(subtitle = game$player[1])

  p_P2 <- P2 %>%
    tidyr::pivot_longer(cols = -period,
                        values_to = "probability",
                        names_to = "strategy") %>%
    ggplot2::ggplot(ggplot2::aes(x = period,
                                 y = probability,
                                 color = strategy,
                                 linetype = strategy)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(subtitle = game$player[2])

  plt_P <- patchwork::wrap_plots(p_P1, p_P2)


  return(list(data = df,
              belief = list(B1 = B1,
                            B2 = B2,
              choice_prob = list(P1 = P1,
                                 P2 = P2)),
              plot_B = plt_B,
              plot_P = plt_P))

}
