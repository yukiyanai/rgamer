#' @title Find Nash equilibria of a normal-form game with  payoffs defined
#'     by character strings
#' @description \code{solve_nfg_char()} finds the pair of the best responses when payoff
#'     functions are provided as character strings.
#' @return A list containing the pair of the best response correspondence (NE)
#'     and the plot of best response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param delta A numerical value specifying the grid size to draw the figure of
#'     best response correspondences.  The default value is 0.1. The smaller the
#'     value is, the smoother the correspondence curves are.
#' @param plot A logical value to determine whether the figure of the best response
#'     correspondences will be displayed. Default is \code{TRUE}.
#' @param mark_NE A logical value to control if the NE (if any) will be marked in
#'     the best response plot, which will be displayed (only displayed when
#'     \code{plot = TRUE}). Default is \code{FALSE}.
#' @param quietly A logical value to determine if the equilibrium will be kept
#'     in the returned list without being printed on screen. Default is \code{FALSE}.
#' @param color_palette A color palette to be used. Default is \code{"Set1"}.
#' @import ggplot2
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_char <- function(game,
                           delta = 0.1,
                           plot = TRUE,
                           mark_NE = FALSE,
                           quietly = FALSE,
                           color_palette = "Set1") {

x <- y <- br_a <- br_b <- player <- NULL
xs <- ys <- xe <- ye <- text <- NULL

  p1 <- game$payoff[[1]]
  p2 <- game$payoff[[2]]
  pars <- game$pars
  par1_lim <- game$strategy[[1]]
  par2_lim <- game$strategy[[2]]
  players <-game$player

  rg1 <- par1_lim[2] - par1_lim[1]
  rg2 <- par2_lim[2] - par2_lim[1]

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
  p1min <- par1_lim[1]
  p1max <- par1_lim[2]
  p2min <- par2_lim[1]
  p2max <- par2_lim[2]
  g1_y0 <- function(x) {
    g1(x = x, y = par2_lim[1])
  }
  expand_counter <- 0
  while (g1_y0(p1min) * g1_y0(p1max) >= 0) {
    p1min <- p1min - 0.5 * rg1
    p1max <- p1max + 0.5 * rg1
    expand_counter <- expand_counter + 1
    if (expand_counter == 500) break
  }
  if (expand_counter < 500) {
    xintercept1 <- stats::uniroot(g1_y0, c(p1min, p1max))$root
  } else {
    xintercept1 <- NULL
  }
  g1_x0 <- function(y) {
    g1(x = par1_lim[1], y = y)
  }
  expand_counter <- 0
  while (g1_x0(p2min) * g1_x0(p2max) >= 0) {
    p2min <- p2min - 0.5 * rg2
    p2max <- p2max + 0.5 * rg2
    if (expand_counter == 500) break
  }
  if (expand_counter < 500) {
    yintercept1 <- stats::uniroot(g1_x0, c(p2min, p2max))$root
  } else {
    yintercept1 <- NULL
  }

  ## Find the intercepts of Player 2's response curve
  p1min <- par1_lim[1]
  p1max <- par1_lim[2]
  p2min <- par2_lim[1]
  p2max <- par2_lim[2]
  g2_y0 <- function(x) {
    g2(x = x, y = par2_lim[1])
  }
  expand_counter <- 0
  while (g2_y0(p1min) * g2_y0(p1max) >= 0) {
    p1min <- p1min - 0.5 * rg1
    p1max <- p1max + 0.5 * rg1
    expand_counter <- expand_counter + 1
    if (expand_counter == 500) break
  }
  if (expand_counter < 500) {
    xintercept2 <- stats::uniroot(g2_y0, c(p1min, p1max))$root
  } else {
    xintercetp2 <- NULL
  }
  g2_x0 <- function(y) {
    g2(x = par1_lim[1], y = y)
  }
  expand_counter <- 0
  while (g2_x0(p2min) * g2_x0(p2max) >= 0) {
    p2min <- p2min - 0.5 * rg2
    p2max <- p2max + 0.5 * rg2
    expand_counter <- expand_counter + 1
    if (expand_counter == 500) break
  }
  if (expand_counter < 500) {
    yintercept2 <- stats::uniroot(g2_x0, c(p2min, p2max))$root
  } else {
    yintercept2 <- NULL
  }

  ## determine plot range
  if (is.null(xintercept1) & is.null(xintercept2)) {
    xmax <- ceiling(max(par1_lim) * 1.1)
    xmax <- max(c(xmax, par1_lim[2]))
  } else {
    xmax <- ceiling(max(c(xintercept1, xintercept2)) * 1.1)
    xmax <- max(c(xmax, par1_lim[2]))
  }
  if (is.null(yintercept1) & is.null(yintercept2)) {
    ymax <- ceiling(max(par2_lim) * 1.1)
    ymax <- max(c(ymax, par2_lim[2]))
  } else {
    ymax <- ceiling(max(c(yintercept1, yintercept2)) * 1.1)
    ymax <- max(c(ymax, par2_lim[2]))
  }

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

  ## Solution with constraints
  sol_type <- ifelse (NE[1] < par1_lim[1] | NE[1] > par1_lim[2] |
                      NE[2] < par2_lim[1] | NE[2] > par2_lim[2], 2, 1)

  if (sol_type == 2) {

    f1 <- p1 %>%
      stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
      stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
      stringr::str_replace_all(c("FIRST_PARAM" = "x", "SECOND_PARAM" = "y")) %>%
      str2expression()
    f2 <- p2 %>%
      stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
      stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
      stringr::str_replace_all(c("FIRST_PARAM" = "y", "SECOND_PARAM" = "x")) %>%
      str2expression()



    par1_seq <- seq(from = par1_lim[1],
                    to   = par1_lim[2],
                    by   = delta)
    par2_seq <- seq(from = par2_lim[1],
                    to   = par2_lim[2],
                    by    = delta)


    # P1's best response
    df_p1_br <- data.frame(x = rep(NA, length(par2_seq)),
                           y = rep(NA, length(par2_seq)))
    for (i in seq_along(par2_seq)) {
      fn1 <- function(x) {
        -1 * eval(f1[[1]], envir = list(y = par2_seq[i]))
      }
      df_p1_br$y[i] <- par2_seq[i]
      df_p1_br$x[i] <- stats::optim(sum(par1_lim) / 2,
                                    fn = fn1,
                                    method = "L-BFGS-B",
                                    lower = c(par1_lim[1]),
                                    upper = c(par1_lim[2]))$par

    }

    # P2's best response
    df_p2_br <- data.frame(x = rep(NA, length(par1_seq)),
                           y = rep(NA, length(par1_seq)))
    for (i in seq_along(par1_seq)) {
      fn2 <- function(x) {
        -1 * eval(f2[[1]], envir = list(y = par1_seq[i]))
      }
      df_p2_br$x[i] <- par1_seq[i]
      df_p2_br$y[i] <- stats::optim(sum(par2_lim) / 2,
                                    fn = fn2,
                                    method = "L-BFGS-B",
                                    lower = c(par2_lim[1]),
                                    upper = c(par2_lim[2]))$par
    }


    ## NE
    NE <- NULL
    for (i in 1:nrow(df_p1_br)) {
      for (j in 1:nrow(df_p2_br))
        if (abs(df_p1_br$x[i] - df_p2_br$x[j]) < abs(delta / 10) &
            abs(df_p1_br$y[i] - df_p2_br$y[j]) < abs(delta / 10)) {
          NE <- c(df_p1_br$x[i], df_p2_br$y[j])
          break
        }
      if (!is.null(NE)) break
    }
  }

  df_sol <- data.frame(
    x = NE[1],
    y = NE[2]) %>%
    dplyr::mutate(text = paste0("(", MASS::fractions(NE[1]),
                                ", ", MASS::fractions(NE[2]), ")"))

  if (sol_type == 1) {
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

    plt <- ggplot2::ggplot(df) +
      ggplot2::geom_hline(yintercept = par1_lim[1], color = "gray") +
      ggplot2::geom_vline(xintercept = par2_lim[1], color = "gray") +
      ggplot2::geom_line(data = df,
                         ggplot2::aes(x = x, y = y,
                                      group = player,
                                      color = player,
                                      alpha = player,
                                      size  = player)) +
      ggplot2::xlim(par1_lim[1], xmax) +
      ggplot2::ylim(par2_lim[1], ymax) +
      ggplot2::geom_segment(data = df_intercept,
                            ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye,
                                         group = player,
                                         color = player,
                                         alpha = player,
                                         size  = player),
                            lineend = "round",
                            linejoin = "round") +
      ggplot2::scale_color_brewer(palette = color_palette,
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_alpha_manual(values = c(0.7, 0.8),
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_size_manual(values = c(3, 1),
                                 breaks = players,
                                 labels = players) +
      ggplot2::labs(x = pars[1], y = pars[2]) +
      ggplot2::coord_fixed()
  } else {
    df1 <- df_p1_br %>%
      dplyr::mutate(player = players[1])
    df2 <- df_p2_br %>%
       dplyr::mutate(player = players[2])
    df <- dplyr::bind_rows(df1, df2)

    plt <- ggplot2::ggplot(df) +
      ggplot2::geom_hline(yintercept = par1_lim[1], color = "gray") +
      ggplot2::geom_vline(xintercept = par2_lim[1], color = "gray") +
      ggplot2::geom_path(data = df,
                         ggplot2::aes(x = x, y = y,
                                      group = player,
                                      color = player,
                                      alpha = player,
                                      size  = player)) +
      ggplot2::scale_color_brewer(palette = 'Set1',
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_alpha_manual(values = c(0.7, 0.8),
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_size_manual(values = c(3, 1),
                                 breaks = players,
                                 labels = players) +
      ggplot2::labs(x = pars[1], y = pars[2]) +
      ggplot2::coord_fixed() +
      ggplot2::xlim(par1_lim[1], par1_lim[2] * 1.1) +
      ggplot2::ylim(par2_lim[1], par2_lim[2] * 1.1)

    xmax <- par1_lim[2]
    ymax <- par2_lim[2]
  }

  plt2 <- plt +
    ggplot2::geom_point(data = df_sol, ggplot2::aes(x = x, y = y),
                        size = 4, color = "black") +
    ggplot2::geom_text(data = df_sol, ggplot2::aes(x = x, y = y, label = text),
              nudge_x = xmax / 12, nudge_y = ymax / 12)


  if (plot) {
    if (mark_NE) plot(plt2)
    else plot(plt)
  }

  if (!quietly) message("NE: ", df_sol$text)

  message("#  The obtained NE might be only a part of the solutions.\n",
          "#  Please examine br_plot (best response plot) carefully.")

  return(list(NE = NE, br_plot = plt, br_plot_NE = plt2))
}
