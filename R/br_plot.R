#' @title Plot of best response correspondences
#' @description \code{br_plot()} creates a plot of the best response correspondences for a 2-by-2 game.
#' @return A ggplot figure of the best response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}, \code{\link[ggplot2]{ggplot}}
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
br_plot <- function(game) {

  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  p1 <- game$payoff[[1]]
  p2 <- game$payoff[[2]]
  if (length(s1) != 2 | length(s2) != 2) stop("This function works only for a 2-by-2 game.")

  players <- game$player
  mat1 <- game$mat$matrix1
  mat2 <- game$mat$matrix2
  msNE <- find_mixed_NE(game)

  if (is.null(msNE)) {
    ## BR of A v B
    if (p1[1] == p1[2] & p1[3] == p1[4]) {
      p <- "ANY"
      cat(paste0("Any p is ", players[1], "'s best response regardless of ", players[2], "'s action.\n"))
    } else {
      p <- ifelse(p1[3] > p1[4], 1, 0)
    }

    ## BR of B v A
    if (p2[1] == p2[3] & p2[2] == p1[4]) {
      q <- "ANY"
      cat(paste0("Any q is ", players[2], "'s best response regardless of ", players[1], "'s action.\n"))
    } else {
      q <- ifelse(p2[2] > p2[4], 1, 0)
    }
    if (p == "ANY" | q == "ANY") {
      cat("The best response correspondens is not uniquely determined; no plot has been created.\n")
      return(NULL)
    }
    df <- data.frame(
      xs = c(p, 0),
      xe = c(p, 1),
      ys = c(0, q),
      ye = c(1, q),
      player = players)
    brp <- ggplot2::ggplot(df) +
      ggplot2::geom_segment(ggplot2::aes(x = xs, y = ys,
                                         xend = xe, yend = ye,
                            color = player)) +
      ggplot2::labs(x = "p", y = "q", title = "best response correspondence") +
      ggplot2::coord_fixed() +
      ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
  } else {
    p <- q <- 0:1
    ep1_1 <- sapply(q, function(q) sum(mat1[1, ] * c(q, 1 - q)))
    ep1_2 <- sapply(q, function(q) sum(mat1[2, ] * c(q, 1 - q)))
    ep2_1 <- sapply(p, function(p) sum(mat2[, 1] * c(p, 1 - p)))
    ep2_2 <- sapply(p, function(p) sum(mat2[, 2] * c(p, 1 - p)))

    ## Player 1's best response
    q_cut <- msNE$s2[1]  ## msNE
    coord_q1_s <- c(0, rep(q_cut, 2))
    coord_q1_e <- c(rep(q_cut, 2), 1)
    coord_p1_s <- rep(NA, 3)
    coord_p1_e <- rep(NA, 3)
    if (ep1_1[1] > ep1_2[1]) {
      coord_p1_s[1:2] <- 1
      coord_p1_e[1] <- 1
    } else {
      coord_p1_s[1:2] <- 0
      coord_p1_e[1] <- 0
    }
    if (ep1_1[2] > ep1_2[2]) {
      coord_p1_s[3] <- 1
      coord_p1_e[2:3] <- 1
    } else {
      coord_p1_s[3] <- 0
      coord_p1_e[2:3] <- 0
    }

    ## Player 2's best response
    p_cut <- msNE$s1[1] ## msNE
    coord_p2_s <- c(0, rep(p_cut, 2))
    coord_p2_e <- c(rep(p_cut, 2), 1)
    coord_q2_s <- rep(NA, 3)
    coord_q2_e <- rep(NA, 3)
    if (ep2_1[1] > ep2_2[1]) {
      coord_q2_s[1:2] <- 1
      coord_q2_e[1] <- 1
    } else {
      coord_q2_s[1:2] <- 0
      coord_q2_e[1] <- 0
    }
    if (ep2_1[2] > ep2_2[2]) {
      coord_q2_s[3] <- 1
      coord_q2_e[2:3] <- 1
    } else {
      coord_q2_s[3] <- 0
      coord_q2_e[2:3] <- 0
    }

    df <- data.frame(
      player = rep(players, each = 3),
      xs = c(coord_p1_s, coord_p2_s),
      xe = c(coord_p1_e, coord_p2_e),
      ys = c(coord_q1_s, coord_q2_s),
      ye = c(coord_q1_e, coord_q2_e))
    brp <- ggplot2::ggplot(df) +
      ggplot2::geom_vline(xintercept = c(0, 1), color = "gray") +
      ggplot2::geom_hline(yintercept = c(0, 1), color = "gray") +
      ggplot2::geom_segment(ggplot2::aes(x = xs, y = ys,
                                         xend = xe, yend = ye,
                                         color = player,
                                         alpha = player,
                                         size  = player),
                            lineend = "round",
                            linejoin = "mitre") +
      ggplot2::labs(x = "p", y = "q", title = "best response correspondence") +
      ggplot2::coord_fixed() +
      ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      ggplot2::scale_color_brewer(palette = 'Set1',
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_alpha_manual(values = c(0.7, 0.8),
                                  breaks = players,
                                  labels = players) +
      ggplot2::scale_size_manual(values = c(3, 1),
                                 breaks = players,
                                 labels = players) +
      ggplot2::scale_x_continuous(breaks = c(0, p_cut, 1),
                                  labels = c("0", as.character(MASS::fractions(p_cut)), "1")) +
      ggplot2::scale_y_continuous(breaks = c(0, q_cut, 1),
                                  labels = c("0", as.character(MASS::fractions(q_cut)), "1"))
  }
  return(brp)
}
