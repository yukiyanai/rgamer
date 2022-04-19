#' @title Find Nash equilibria of a normal-form game.
#' @description \code{solve_nfg_matrix()} finds Nash equilibria of a normal-form
#'      (strategic-form) game with discrete-choice strategies.
#' @return A list containing pure strategy Nash equilibria (NE), mixed strategy
#'      NE, a table of the payoff matrix, the plot of best response
#'      correspondences, and the list of mixed strategy NE of the subsets of
#'      strategies. Each element will be \code{NULL} if not available.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param mixed A logical value. If \code{TRUE}, mixed-strategy NE will be
#'      searched. Default is \code{FALSE}.
#' @param show_table A logical value. If \code{TRUE}, the payoff matrix  of the
#'     game will be displayed. Default is \code{TRUE}.
#' @param mark_br A logical value. If \code{TRUE}, the best response to each of
#'     the opponent's strategy is marked. Default is \code{TRUE}.
#' @param quietly A logical value that determines whether the equilibrium will
#'     be kept in the returned list without being printed on screen. Default is
#'      \code{FALSE}.
#' @param color_palette A color palette to be used. Default is \code{"Set1"}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_matrix <- function(
  game,
  mixed = FALSE,
  show_table = TRUE,
  mark_br = TRUE,
  quietly = FALSE,
  color_palette = "Set1") {

  ## Pure-strategy NE
  psNE <- find_pure_NE(game)
  if (is.null(psNE)) {
    if (!quietly) message("Pure strategy NE does not exist.\n")
  } else if (!quietly) {
    psNE_str <- paste(psNE, collapse = ", ")
    message("Pure-strategy NE: ", psNE_str, "\n")
  }

  if (mixed) {
    msNE <- find_mixed_NE(game)
    msNE_list <- msNE$msNE_list
    msNE <- msNE$msNE
    if (!is.null(msNE)) {
      out1 <- stringi::stri_join(MASS::fractions(msNE[[1]]), collapse = ", ")
      out2 <- stringi::stri_join(MASS::fractions(msNE[[2]]), collapse = ", ")
      out <- paste0("[(", out1, "), (", out2, ")]")

      if (!quietly) {
        message("Mixed-strategy NE: ", out, "\n")
        message("The obtained mixed-strategy NE might be only a part of the solutions.")

        if (length(game$strategy$s1) == 2 & length(game$strategy$s2) == 2) {
          message("Please examine br_plot (best response plot) carefully.")
        }
      }
    } else {
      if (!quietly) {
        message("The payoff matrix is degenerate; No full support mixed-strategy NE exist (or infinitely many exist).")
        if (length(game$strategy$s1) == 2 & length(game$strategy$s2) == 2) {
          message("Please examine br_plot (best response plot).")
        } else {
          message("It might be useful to check 'msNE_list'.")
        }
      }
    }
  } else {
    msNE <- NULL
    msNE_list <- NULL
  }
  mat_tbl <- game_table(game, mark_br = mark_br)
  if (show_table) print(mat_tbl)

  if (length(game$strategy[[1]]) == 2 & length(game$strategy[[2]]) == 2) {
    p <- br_plot(game, color_palette = color_palette, msNE = msNE)
  } else {
    p <- NULL
  }

  return(list(psNE = psNE,
              msNE = msNE,
              table = mat_tbl,
              br_plot = p,
              msNE_list = msNE_list))
}
