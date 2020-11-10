#' @title Find Nash equilibria of a normal-form game with discrete-choice strategies
#' @description \code{solve_nfg_matrix()} finds Nash equilibria of a normal-form (strategic-form) game.
#' @return A list containing pure strategy Nash equilibrium (NE), mixed strategy NE, the gt table of the game,
#'   and the plot of best response correspondences. Each element will be NULL if not available.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param mixed A logical value. If \code{TRUE}, mixed-strategy NE will be calculated. Default is \code{FALSE}.
#' @param show_table A logical value. If \code{TRUE}, the table of the game will be displayed. Default is \code{TRUE}.
#' @param mark_br A logical value. If \code{TRUE}, the best response to each of the opponent's strategy is marked.
#'   Default is \code{TRUE}.
#' @param cell_width A number specifying the cell width of the game matrix. The unit is pixel.
#'   The default value is 80.
#' @param quietly A logical value that determines whether the equilibrium will be kept in the returned list
#'     without being printed on screen. Default is \code{FALSE}
#' @param color_palette A color palette to be used. Default is \code{"Set1"}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_matrix <- function(
  game,
  mixed = FALSE,
  show_table = TRUE,
  cell_width = NULL,
  mark_br = TRUE,
  quietly = FALSE,
  color_palette = "Set1") {

  ## Pure-strategy NE
  psNE <- find_pure_NE(game)
  if (is.null(psNE)) {
    message("Pure strategy NE does not exist.\n")
  } else if (!quietly) {
    if (!quietly) message("Pure-strategy NE: ", psNE)
  }

  if (mixed) {
    msNE <- find_mixed_NE(game)
    if (is.null(msNE)) message("Mixed-strategy NE does not exist (or infinitely many exist).\n")
    else {
      out1 <- stringi::stri_join(MASS::fractions(msNE[[1]]), collapse = ", ")
      out2 <- stringi::stri_join(MASS::fractions(msNE[[2]]), collapse = ", ")
      out <- paste0("[(", out1, "), (", out2, ")]")

      if (!quietly) message("Mixed-strategy NE: ", out)

      message("#  The obtained mixed-strategy NE might be only a part of the solutions.\n",
              "#  Please examine br_plot (best response plot) carefully.")
    }
  } else {
    msNE <- NULL
  }
  mat_tbl <- game_table(game, cell_width = cell_width, mark_br = mark_br)
  if (show_table) print(mat_tbl)

  if (length(game$strategy[[1]]) == 2 & length(game$strategy[[2]])) {
    p <- br_plot(game, color_palette = color_palette)
  } else {
    p <- NULL
  }

  return(list(psNE = psNE, msNE = msNE, table = mat_tbl, br_plot = p))
}
