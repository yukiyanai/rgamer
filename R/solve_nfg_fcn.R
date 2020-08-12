#' @title Find Nash equilibria of a normal-form game with payoffs defined by R functions
#' @description \code{solve_nfg_fcn()} finds the pair of best responses when payoff
#'     functions are provided as R functions.
#' @return A list containing the pair of the best response correspondence (NE)  and the plot of best
#'     response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso [normal_form()]
#' @param cons1 A named list of parameters contained in \code{game$payoff$p1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{game$payoff$p2} that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in \code{game$payoff$p1} and \code{game$payoff$p2} that should be treated as constants, if any.
#'     If \code{cons1} and \code{cons2} are exactly same, you can specify \code{cons_common} instead of specifying
#'     both \code{cons1} and \code{cons2}.
#' @param precision A natural number specifying the precision of numerical approximation.
#'     The value n approximately means that the approximation is correct up to the Nth decimal place.
#'     The default value is 1.
#' @param plot A logical value to determine whether the figure of the best response correspondences
#'     will be displayed. Default is \code{TRUE}.
#' @param quietly A logical value to determine if the equilibrium will be kept in the returned list
#'     without being printed on screen. Default is \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_fcn <- function(game,
                          cons1 = NULL,
                          cons2 = NULL,
                          cons_common = NULL,
                          precision = 1L,
                          plot = TRUE,
                          quietly = FALSE) {

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

  df_sol <- gridsearch_br(
    players = game$player,
    p1 = game$payoff[[1]],
    p2 = game$payoff[[2]],
    pars = game$pars,
    par1_lim = par1_lim,
    par2_lim = par2_lim,
    cons1 = cons1,
    cons2 = cons2,
    cons_common = cons_common,
    precision = precision)

  NE <- c(df_sol$x, df_sol$y)

  df_sol <- df_sol %>%
    dplyr::mutate(text = paste0("(",
                                round(NE[1], digits = precision),
                                ", ",
                                round(NE[2], digits = precision),
                                ")"))

  df <- as_df_br(players = game$player,
                 p1 = game$payoff[[1]],
                 p2 = game$payoff[[2]],
                 pars = game$pars,
                 par1_lim = par1_lim,
                 par2_lim = par2_lim,
                 cons1 = cons1,
                 cons2 = cons2,
                 cons_common = cons_common)
  df <- dplyr::bind_rows(df)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_hline(yintercept = par1_lim[1], color = "gray") +
    ggplot2::geom_vline(xintercept = par2_lim[1], color = "gray") +
    ggplot2::geom_line(data = df,
                       ggplot2::aes(x = x, y = y, color = player, group = player)) +
    ggplot2::geom_point(data = df_sol, ggplot2::aes(x = x, y = y), size = 2, color = "black") +
    ggplot2::geom_text(data = df_sol, ggplot2::aes(x = x, y = y, label = text),
                       nudge_x = par1_lim[2] / 10, nudge_y = par2_lim[2] / 10) +
    ggplot2::labs(x = game$pars[1], y = game$pars[2]) +
    ggplot2::coord_fixed()

  if (plot) plot(p)
  if (!quietly) cat("approximated NE:", df_sol$text, "\n")

  return(list(NE = NE, br_plot = p))
}