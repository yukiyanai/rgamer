#' @title Find Nash equilibria of a normal-form game with  payoffs defined by character strings
#' @description \code{solve_nfg_char()} finds the pair of the best responses when payoff
#'     functions are provided as character strings.
#' @return A list containing the pair of the best response correspondence (NE)  and the plot of best
#'     response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param delta A numerical value specifying the grid size to draw the figure of best response correspondences.
#'     The default value is 0.1. The smaller the value is, the smoother the correspondence curves are.
#' @param plot A logical value to determine whether the figure of the best response correspondences
#'     will be displayed. Default is \code{TRUE}.
#' @param quietly A logical value to determine if the equilibrium will be kept in the returned list
#'     without being printed on screen. Default is \code{FALSE}.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_char <- function(game, delta = 0.1, plot = TRUE, quietly = FALSE) {

  p1 <- game$payoff[[1]]
  p2 <- game$payoff[[2]]
  pars = game$pars
  par1_lim = game$strategy[[1]]
  par2_lim = game$strategy[[2]]
  players = game$player

  f1 <- p1 %>%
    stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x", "SECOND_PARAM" = "y")) %>%
    str2expression()
  f2 <- p2 %>%
    stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x", "SECOND_PARAM" = "y")) %>%
    str2expression()

  ## first-order derivatives
  g1 <- function(x, y) {
    g <- stats::D(f1, "x")
    eval(g, envir = list(x = x, y = y))
  }
  g2 <- function(x, y) {
    g <- stats::D(f2, "y")
    eval(g, envir = list(x = x, y = y))
  }

  ## Find the intercepts of Player 1's response curve
  g1_y0 <- function(x) {
    g1(x = x, y = par2_lim[1])
  }
  xintercept1 <- stats::uniroot(g1_y0, c(par1_lim[1], par1_lim[2] * 2))$root
  g1_x0 <- function(y) {
    g1(x = par1_lim[1], y = y)
  }
  yintercept1 <- stats::uniroot(g1_x0, c(par2_lim[1], par2_lim[2] * 2))$root

  ## Find the intercepts of Player 2's response curve
  g2_y0 <- function(x) {
    g2(x = x, y = par2_lim[1])
  }
  xintercept2 <- stats::uniroot(g2_y0, c(par1_lim[1], par1_lim[2] * 2))$root
  g2_x0 <- function(y) {
    g2(x = par1_lim[1], y = y)
  }
  yintercept2 <- stats::uniroot(g2_x0, c(par2_lim[1], par2_lim[2]) * 2)$root

  xmax <- ceiling(max(c(xintercept1, xintercept2)) * 1.1)
  ymax <- ceiling(max(c(yintercept1, yintercept2)) * 1.1)

  df_intercept <- data.frame(
    xs = c(par1_lim[1], xintercept2),
    xe = c(par1_lim[1], xmax),
    ys = c(yintercept1, par2_lim[1]),
    ye = c(ymax, par2_lim[1]),
    player = players
  )

  ## NE
  fn <- function(X) {
    x <- X[1]
    y <- X[2]
    c(g1(x, y), g2(x, y))
  }
  NE <- nleqslv::nleqslv(c(xmax / 2, ymax / 2), fn)$x
  df_sol <- data.frame(
    x = NE[1],
    y = NE[2]) %>%
    dplyr::mutate(text = paste0("(", MASS::fractions(NE[1]),
                                ", ", MASS::fractions(NE[2]), ")"))

  df <- expand.grid(x = seq(from = par1_lim[1], to = xmax, by = delta),
                    y = seq(from = par2_lim[1], to = ymax, by = delta)) %>%
    dplyr::mutate(br_a = g1(x, y),
                  br_b = g2(x, y))
  df1 <- df %>%
    dplyr::filter(br_a == 0) %>%
    dplyr::select(!br_b) %>%
    dplyr::mutate(player = players[1])
  df2 <- df %>%
    dplyr::filter(br_b == 0) %>%
    dplyr::select(!br_a) %>%
    dplyr::mutate(player = players[2])
  df <- dplyr::bind_rows(df1, df2)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_hline(yintercept = par1_lim[1], color = "gray") +
    ggplot2::geom_vline(xintercept = par2_lim[1], color = "gray") +
    ggplot2::geom_line(data = df,
                       ggplot2::aes(x = x, y = y, color = player, group = player)) +
    ggplot2::geom_point(data = df_sol, ggplot2::aes(x = x, y = y), size = 2, color = "black") +
    ggplot2::geom_text(data = df_sol, ggplot2::aes(x = x, y = y, label = text),
              nudge_x = xmax / 12, nudge_y = ymax / 12) +
    ggplot2::xlim(par1_lim[1], xmax) +
    ggplot2::ylim(par2_lim[1], ymax) +
    ggplot2::geom_segment(data = df_intercept,
                          ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye,
                                       group = player, color = player)) +
    ggplot2::labs(x = pars[1], y = pars[2]) +
    ggplot2::coord_fixed()

  if (plot) plot(p)
  if (!quietly) cat("NE:", df_sol$text, "\n")

  return(list(NE = NE, br_plot = p))
}
