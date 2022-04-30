#' @title Simulates a fictitious play once
#' @description \code{sim_fict()} simulates a fictitious play of a game
#' @details Simulate fictitious plays of a normal-form game defined by
#'     \code{normal_form()}.
#' @param game An object of \code{normal_form} class defined by
#'     \code{normal_form()}.
#' @param n_periods A positive integer specifying how many times the game is
#'     played.
#' @param lambda A positive real value representing the players' sensitivity to
#'     expected utilities. As \code{lambda} gets larger, a small difference in
#'     expected utility makes a big difference in choice probability.
#' @param init A list of initial levels of beliefs. The length of the list must
#'     be two. Each element must be a vector of values between 0 and 1
#'     representing a player's belief. If \code{init = NULL}, which is default,
#'     initial beliefs will be randomly assigned.
#' @param sigma A non-negative value determining the level of noise adherent to
#'     evaluation of payoffs.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
sim_fict_one <- function(game,
                         n_periods = 50,
                         lambda = 1,
                         init = NULL,
                         sigma = 0) {

  ## check input
  if (game$type != "matrix") {
    stop("This function works with 'matrix' type games only.")
  }

  if (n_periods < 1) {
    stop("n_periods must be a positive integer.")
  }

  if (lambda <= 0) {
    stop("lambda must be a positive number.")
  }

  if (sigma < 0) {
    stop("sigma must be a non-negative number.")
  }
  1

  period <- belief <- strategy <- probability <- ratio <- NULL

  pi1 <- game$mat$matrix1
  pi2 <- game$mat$matrix2

  s1 <- game$strategy$s1
  s2 <- game$strategy$s2

  n1 <- length(s1)
  n2 <- length(s2)

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
    B2[1, ] <- tmp2 / sum(tmp2)
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

    c1 <- sample(1:n1, size = 1, prob = P1[t, ])
    c2 <- sample(1:n2, size = 1, prob = P2[t, ])

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
  df <- data.frame(player1 = s1[choice1[-1]],
                   player2 = s2[choice2[-1]],
                   period = 1:n_periods)

  colnames(B1) <- s2
  colnames(B2) <- s1
  colnames(P1) <- s1
  colnames(P2) <- s2

  B1 <- as.data.frame(B1[-1, ])
  B2 <- as.data.frame(B2[-1, ])
  P1 <- as.data.frame(P1[-1, ])
  P2 <- as.data.frame(P2[-1, ])

  B1$period <- B2$period <- P1$period <- P2$period <- 1:n_periods

  return(list(data = df,
              belief = list(B1 = B1,
                            B2 = B2),
              choice_prob = list(P1 = P1,
                                 P2 = P2)))
}
