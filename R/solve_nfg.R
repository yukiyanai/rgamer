#' @title Find equilibria of a normal-form game
#' @description \code{solve_nfg()} finds Nash equilibria of a normal-form game.
#' @details This function finds Nash equilibria of a normal-form game by
#'     \code{\link{solve_nfg_matrix}}, \code{\link{solve_nfg_char}}, or
#'     \code{\link{solve_nfg_fcn}} depending on \code{type} of a normal_form
#'     object defined by \code{normal_form}.
#' @return A list containing Nash equilibria (if any), the payoff matrix of the
#'     game  (if available), and the plot of best response correspondence
#'     (if available).
#' @inheritParams solve_nfg_matrix
#' @inheritParams solve_nfg_char
#' @inheritParams solve_nfg_fcn
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' game1 <- normal_form(
#'   s1 = c("T", "B"), s2 = c("L", "R"),
#'   p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1))
#' s1 <- solve_nfg(game1, show_table = FALSE)
#'
#' game2 <- normal_form(
#'   p1 = "-x1^2 + (28 - x2) * x1",
#'   p2 = "-x2^2 + (28 - x1) * x2",
#'   par1_lim = c(0, 30),
#'   par2_lim = c(0, 30),
#'   pars = c("x1", "x2"))
#' s2 <- solve_nfg(game2)
#'
#' fx <- function(x, y) -x^2 + (28 - y) * x
#' fy <- function(x, y) -y^2 + (28 - x) * y
#' game3 <- normal_form(
#'     p1 = fx,
#'     p2 = fy,
#'     pars = c("x", "y"),
#'     par1_lim = c(0, 40),
#'     par2_lim = c(0, 40))
#' s3 <- solve_nfg(game3)
solve_nfg <- function(game,
                      mixed = FALSE,
                      show_table = TRUE,
                      mark_br = TRUE,
                      cons1 = NULL,
                      cons2 = NULL,
                      cons_common = NULL,
                      precision = 1,
                      plot = TRUE,
                      mark_NE = FALSE,
                      quietly = FALSE,
                      color_palette = "Set1") {

  if (class(game) != "normal_form")
    stop("game must be an object of 'normal_form' class created by normal_form() function.")

  if (game$type == "matrix") {
    solve_nfg_matrix(game = game,
                     mixed = mixed,
                     show_table = show_table,
                     mark_br = mark_br,
                     quietly = quietly,
                     color_palette = color_palette)

  } else if (game$type == "char_function") {
    solve_nfg_char(game = game,
                   plot = plot,
                   mark_NE = mark_NE,
                   quietly = quietly,
                   color_palette = color_palette)

  } else if (game$type == "function") {
    solve_nfg_fcn(game = game,
                  cons1 = cons1,
                  cons2 = cons2,
                  cons_common = cons_common,
                  precision = precision,
                  plot = plot,
                  mark_NE = mark_NE,
                  quietly = quietly,
                  color_palette = color_palette)
  } else {
    stop("This game has not been defined properly.")
  }
}
