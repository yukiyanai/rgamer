#' @title Simulates an Experienced Weighted Attraction model
#' @description \code{sim_EWA()} simulates plays of a normal-form
#'     game expected by an experienced weighted attraction (EWA) model.
#' @details Simulate plays of a normal-form game defined by
#'     \code{normal_form()} in a way expected by an EWA model.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param n_periods A positive integer specifying how many times the game is
#'     played within each sample.
#' @param lambda A positive real value representing the players' sensitivity to
#'     attraction values of strategies. As \code{lambda} gets larger, the choice
#'     will be dependent on attraction values more heavily.  As \code{lambda}
#'     gets close to 0, a strategy will tend to be chosen randomly.
#' @param delta A real number between 0 and 1. This parameter controls how fast
#'     attraction values of strategies that are not chosen are updated.  If
#'     \code{delta = 0}, attraction is updated only for the strategy that is
#'     selected at the given period (i.e., reinforcement learning is
#'     implemented). If \code{delta = 1}, attraction is updated equally for all
#'     strategies (i.e., belief-based learning model is applied).
#' @param rho A real value between 0 and 1. This parameter controls the learning
#'     speed. \code{rho = 0} for "reinforcement" leaning and "belief" based
#'     learning.
#' @param phi A real value between 0 and 1. This parameter controls how much
#'     attraction values at the current period are constrained by the past
#'     attraction values. If \code{phi = 0}, the past attraction values are
#'     ignored. \code{phi = 1} for "reinforcement" leaning and "belief" based
#'     learning.
#' @param A1_init An initial value of Player 1's attraction for each strategy.
#' @param A2_init An initial value of Player 2's attraction for each strategy.
#' @param N_init An initial value of N.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
sim_EWA <- function(game,
                    n_periods,
                    lambda = 1,
                    delta = 0.5,
                    rho = 0.5,
                    phi = 0.5,
                    A1_init = 0,
                    A2_init = 0,
                    N_init = 0) {


  pi1 <- game$mat$matrix1
  pi2 <- game$mat$matrix2

  n1 <- length(game$strategy$s1)
  n2 <- length(game$strategy$s2)

  ## A (attraction) and P (Probability)
  A1 <- P1 <- matrix(NA, nrow = n_periods + 1, ncol = n1) # Player 1
  A2 <- P2 <- matrix(NA, nrow = n_periods + 1, ncol = n2) # Player 2

  A1[1, ] <- A1_init
  A2[1, ] <- A2_init

  N <- rep(NA, n_periods)
  N[1] <- N_init

  choice1 <- choice2 <- rep(NA, n_periods + 1)

  for (t in (1:n_periods) + 1) {

    w1 <- A1[t - 1, ]
    w2 <- A2[t - 1, ]

    N[t] <- rho * N[t - 1] + 1

    lambda1 <- lambda * w1
    lambda2 <- lambda * w2
    max1 <- max(lambda1)
    max2 <- max(lambda2)
    dif1 <- lambda1 - max1
    dif2 <- lambda2 - max2
    ln_P1 <- lambda1 - (max1 + log(sum(exp(dif1))))
    ln_P2 <- lambda2 - (max2 + log(sum(exp(dif2))))

    P1[t, ] <- exp(ln_P1)
    P2[t, ] <- exp(ln_P2)

    choice1[t] <- sample(1:n1, size = 1, prob = P1[t, ])
    choice2[t] <- sample(1:n2, size = 1, prob = P2[t, ])

    ## vector indicating which strategy was selected
    e1 <- numeric(n1)
    e1[choice1[t]] <- 1
    e2 <- numeric(n2)
    e2[choice2[t]] <- 1

    ## Player 1's payoff given Player 2's action
    payoff1_vec <- as.vector(pi1 %*% e2)

    # Player2 's payoff given Player 1's action
    payoff2_vec <- as.vector(e1 %*% pi2)

    ## update attraction
    A1[t, ] <- (phi * N[t - 1] * A1[t - 1, ] + delta * payoff1_vec) / N[t]
    A2[t, ] <- (phi * N[t - 1] * A2[t - 1, ] + delta * payoff2_vec) / N[t]

    ## update attraction for the strategy actually chosen
    payoff1 <- as.numeric(e1 %*% pi1 %*% e2)
    payoff2 <- as.numeric(e1 %*% pi2 %*% e2)

    A1[t, choice1[t]] <- (phi * N[t - 1] * A1[t - 1, choice1[t]] + payoff1) / N[t]
    A2[t, choice2[t]] <- (phi * N[t - 1] * A2[t - 1, choice2[t]] + payoff2) / N[t]
  }

  df <- data.frame(player1 = game$strategy$s1[choice1[-1]],
                   player2 = game$strategy$s2[choice2[-1]],
                   period = 1:n_periods)

  colnames(A1) <- colnames(P1) <- game$strategy$s1
  colnames(A2) <- colnames(P2) <- game$strategy$s2

  A1 <- as.data.frame(A1[-1, ])
  A2 <- as.data.frame(A2[-1, ])
  P1 <- as.data.frame(P1[-1, ])
  P2 <- as.data.frame(P2[-1, ])

  A1$period <- A2$period <- P1$period <- P2$period <- 1:n_periods

  return(list(data = df,
              attraction = list(A1 = A1,
                                A2 = A2),
              choice_prob = list(P1 = P1,
                                 P2 = P2)))
}
