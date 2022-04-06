#' @title Simulates a learning model
#' @description \code{sim_learning()} simulates learning dynamics in a
#'     normal-form game expected by an experienced weighted attraction (EWA)
#'     model.
#' @details Simulate plays of a normal-form game defined by
#'     \code{normal_form()} in a way expected by an EWA model.
#' @inheritParams sim_EWA
#' @param n_samples A positive integer specifying the number of samples to be
#'     simulated.
#' @param type A character string to tell which learning models should be
#'     simulated. The available options are \code{"EWA"},
#'     \code{"reinforcement"} (choice reinforcement), and \code{"belief"}
#'     (belief based model).  \code{"reinforcement"} and \code{"belief"} are
#'     special cases of \code{"EWA"}.
#' @param plot_range_y Choose the range of vertical axis for plots. Available
#'     choices are \code{"fixed"}, \code{"full"} and \code{"free"}.
#'     If \code{plot_range_y = "free"}, the range of y-axis depends on
#'     simulation results.  If \code{plot_range_y = "full"}, The range
#'     defined in \code{game} is used for each player, which can be different
#'     between players. With \code{"fixed"}, the same y-axis is used for both
#'     players.
#' @return A list containing (1) a list of data frames of strategies chosen by
#'     each player, (2) a single long data frame of (1), (3) a list of each
#'     player's attraction values for each strategy (data frames), (4) a list of
#'      probability of each strategy being chosen (data frames), and (5) a plot
#'      of the simulation result (ggplot object).
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
sim_learning <- function(game,
                         n_samples,
                         n_periods,
                         type = "EWA",
                         lambda = 1,
                         delta = 0.5,
                         rho = 0.5,
                         phi = 0.5,
                         A1_init = 0,
                         A2_init = 0,
                         N_init = 0,
                         plot_range_y = NULL) {

  player1 <- player2 <- period <- player <- strategy <- NULL

  ## Checks input values
  type <- match.arg(type,
                    choices = c("EWA", "reinforcement", "belief"))

  if (n_samples < 1) {
    stop("n_samples must be a positive integer.")
  }

  if (n_periods < 1) {
    stop("n_periods must be a positive integer.")
  }

  if (lambda <= 0) {
    stop("lambda must be a positive number.")
  }

  if (delta < 0 | delta > 1) {
    stop("delta must be a value between 0 and 1.")
  }

  if (rho < 0 | rho > 1) {
    stop("rho must be a value between 0 and 1.")
  }

  if (phi < 0 | phi > 1) {
    stop("phi must be a value between 0 and 1.")
  }

  if (N_init < 0) {
    stop("N_init must be a non-negative number.")
  }

  data_list <- A1_list <- A2_list <- P1_list <- P2_list <- list()

  if (game$type != "matrix") {
    stop("This function works with 'matrix' type games only.")
  }

  if (type == "EWA") {
    for (i in 1:n_samples) {
      res <- sim_EWA(game = game,
                     n_periods = n_periods,
                     A1_init = A1_init,
                     A2_init = A2_init,
                     N_init = N_init,
                     delta = delta,
                     phi = phi,
                     rho = rho,
                     lambda = lambda)

      data_list[[i]] <- res$data %>%
        dplyr::mutate(sample = i)
      A1_list[[i]] <- res$attraction$A1
      A2_list[[i]] <- res$attraction$A2
      P1_list[[i]] <- res$choice_prob$P1
      P2_list[[i]] <- res$choice_prob$P2
    }

  } else if (type == "reinforcement") {
    for (i in 1:n_samples) {
      res <- sim_EWA(game = game,
                     n_periods = n_periods,
                     A1_init = A1_init,
                     A2_init = A2_init,
                     N_init = N_init,
                     delta = 0,
                     phi = 1,
                     rho = 1,
                     lambda = lambda)

      data_list[[i]] <- res$data %>%
        dplyr::mutate(sample = i)
      A1_list[[i]] <- res$attraction$A1
      A2_list[[i]] <- res$attraction$A2
      P1_list[[i]] <- res$choice_prob$P1
      P2_list[[i]] <- res$choice_prob$P2
    }

  } else { # belief based model
    for (i in 1:n_samples) {
      res <- sim_EWA(game = game,
                     n_periods = n_periods,
                     A1_init = A1_init,
                     A2_init = A2_init,
                     N_init = N_init,
                     delta = 1,
                     phi = 1,
                     rho = 1,
                     lambda = lambda)

      data_list[[i]] <- res$data %>%
        dplyr::mutate(sample = i)
      A1_list[[i]] <- res$attraction$A1
      A2_list[[i]] <- res$attraction$A2
      P1_list[[i]] <- res$choice_prob$P1
      P2_list[[i]] <- res$choice_prob$P2
    }
  }

  data_long <- dplyr::bind_rows(data_list) %>%
    tidyr::pivot_longer(player1:player2,
                        names_to = "player",
                        values_to = "strategy") %>%
    dplyr::select(sample, period, player, strategy) %>%
    dplyr::mutate(player = ifelse(player == "player1",
                                  game$player[1],
                                  game$player[2]))

  p <- plot_sim(data_long,
                game = game,
                plot_range_y = plot_range_y)


  return(list(data = data_list,
              data_long = data_long,
              attraction = list(A1 = A1_list,
                                A2 = A2_list),
              choice_prob = list(P1 = P1_list,
                                 P2 = P2_list),
              plot_mean = p$plot_mean))
}
