#' @title Find Nash equilibria of a normal-form game.
#' @description \code{solve_nfg_fcn()} finds the pair of best responses when
#'     payoff functions are provided as R functions.
#' @return A list containing the pair of the best response correspondence (NE)
#'      and the plot of best response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param par_label A vector of parameter labels if the user define the game
#'     with functions as characters.
#' @param cons1 A named list of parameters contained in
#'     \code{game$payoff$payoffs1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in
#'     \code{game$payoff$payoffs2} that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in
#'     \code{game$payoff$payoffs1} and \code{game$payoff$payoffs2} that should
#'     be treated as constants, if any. If \code{cons1} and \code{cons2} are
#'     exactly same, you can specify \code{cons_common} instead of specifying
#'     both \code{cons1} and \code{cons2}.
#' @param precision A natural number specifying the precision of numerical
#'     approximation. The value n approximately means that the approximation is
#'      correct up to the Nth decimal place. The default value is 1.
#' @param plot A logical value to determine whether the figure of the best
#'     response correspondences will be displayed. Default is \code{TRUE}.
#' @param mark_NE A logical value to control if the NE (if any) will be marked
#'     in the best response plot, which will be displayed (only displayed when
#'     \code{plot = TRUE}). Default is \code{FALSE}.
#' @param quietly A logical value to determine if the equilibrium will be kept
#'     in the returned list  without being printed on screen. Default is
#'     \code{FALSE}.
#' @param color_palette A color palette to be used. Default is \code{"Set1"}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_fcn <- function(game,
                          par_label = NULL,
                          cons1 = NULL,
                          cons2 = NULL,
                          cons_common = NULL,
                          precision = 1L,
                          plot = TRUE,
                          mark_NE = FALSE,
                          quietly = FALSE,
                          color_palette = "Set1") {

  x <- y <- player <- text <- pars <- NULL

  players <- game$player

  if (is.null(par_label)) par_label <- game$pars

  par1_lim <- game$strategy[[1]]
  par2_lim <- game$strategy[[2]]

  if (is.null(cons1) & is.null(cons2) & is.null(cons_common)) {
    if (length(game$constants) == 2) {
      cons1 <- game$constants[[1]]
      cons2 <- game$constants[[2]]
    } else {
      cons_common <- game$constants[[1]]
    }
  }

  # find best responses by grid search
  df_sol <- gridsearch_br(players = players,
                          payoffs1 = game$payoff[[1]],
                          payoffs2 = game$payoff[[2]],
                          pars = game$pars,
                          par1_lim = par1_lim,
                          par2_lim = par2_lim,
                          cons1 = cons1,
                          cons2 = cons2,
                          cons_common = cons_common,
                          precision = precision)

  NE <- c(df_sol$x, df_sol$y)

  # data frame of NE(s)
  df_sol <- df_sol |>
    dplyr::mutate(text = paste0("(",
                                round(NE[1], digits = precision),
                                ", ",
                                round(NE[2], digits = precision),
                                ")"))

  # data frame of best responses
  df <- as_df_br(players = game$player,
                 payoffs1 = game$payoff[[1]],
                 payoffs2 = game$payoff[[2]],
                 pars = game$pars,
                 par1_lim = par1_lim,
                 par2_lim = par2_lim,
                 cons1 = cons1,
                 cons2 = cons2,
                 cons_common = cons_common)
  df1 <- df$df1 |>
    dplyr::arrange(y)
  df2 <- df$df2 |>
    dplyr::arrange(x)

  # check if best responses are within the domain
  difference1 <- diff(df1$y[1:2])
  check_range_lb <- NE - difference1
  check_range_ub <- NE + difference1
  check_df1 <- df1 |>
    dplyr::filter(x > check_range_lb[1],
                  x < check_range_ub[1],
                  y > check_range_lb[2],
                  y < check_range_ub[2])
  difference2 <- diff(df2$x[1:2])
  check_range_lb <- NE - difference2
  check_range_ub <- NE + difference2
  check_df2 <- df2 |>
    dplyr::filter(x > check_range_lb[1],
                  x < check_range_ub[1],
                  y > check_range_lb[2],
                  y < check_range_ub[2])

  if (nrow(check_df1) == 0 | nrow(check_df2) == 0) {
    NE <- NULL
  }

  # best responses inside the domain
  df <- dplyr::bind_rows(df1, df2)

  # best response correspondence
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_hline(yintercept = par1_lim[1], color = "gray") +
    ggplot2::geom_vline(xintercept = par2_lim[1], color = "gray") +
    ggplot2::geom_point(data = df,
                        ggplot2::aes(x = x, y = y,
                                     color = player,
                                     group = player,
                                     alpha = player,
                                     size  = player)) +
    ggplot2::scale_color_brewer(palette = color_palette,
                                breaks = players,
                                labels = players) +
    ggplot2::scale_alpha_manual(values = c(1, 1),
                                breaks = players,
                                labels = players) +
    ggplot2::scale_size_manual(values = c(3, 1),
                               breaks = players,
                               labels = players) +
    ggplot2::labs(x = par_label[1], y = par_label[2]) +
    ggplot2::coord_fixed()


   if (is.null(NE)) {
     payoffs2 <- p
   } else {
     # add text showing NE
     payoffs2 <- p +
      ggplot2::geom_point(data = df_sol,
                          ggplot2::aes(x = x, y = y),
                          size = 4,
                          color = "black") +
      ggplot2::geom_text(data = df_sol,
                         ggplot2::aes(x = x, y = y, label = text),
                         nudge_x = par1_lim[2] / 10,
                         nudge_y = par2_lim[2] / 10)
   }

  # display the plot
  if (plot) {
    if (mark_NE) plot(payoffs2)
    else plot(p)
  }


  if (!quietly) {
    if (is.null(NE)) {
      message("NE was not found.")
    } else {
      message("approximated NE: ", df_sol$text, "\n")
    }
  }

  message("The obtained NE might be only a part of the solutions.\n",
          "Please examine br_plot (best response plot) carefully.")

  return(list(NE = NE, br_plot = p, br_plot_NE = payoffs2))
}
