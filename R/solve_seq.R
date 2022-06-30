#' @title Find Nash equilibria of a sequential-form game
#' @description \code{solve_seq()} finds Nash equilibrium outcomes of a
#'     sequential-form game. This is a wrapper function of
#'     \code{\link{solve_seq_matrix}}, \code{\link{solve_seq_char}}, and
#'     \code{\link{solve_seq_fcn}}.
#' @return A list containing Nash equilibrium outcomes (if any), and
#'     the game table (if available).
#' @inheritParams solve_seq_matrix
#' @inheritParams solve_seq_char
#' @inheritParams solve_seq_fcn
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
solve_seq <- function(game,
                      show_table = TRUE,
                      mark_br = FALSE,
                      cons1 = NULL,
                      cons2 = NULL,
                      cons_common = NULL,
                      precision = 1L,
                      quietly = FALSE) {

  if (!methods::is(game, "sequential_form"))
    stop("game must be an object of 'sequential_form' class created by sequential_form() function.")

  if (game$type == "matrix") {
    solve_seq_matrix(game = game,
                     show_table = show_table,
                     mark_br = mark_br,
                     quietly = quietly)

  } else if (game$type == "char_function") {
    solve_seq_char(game = game,
                   precision = precision,
                   quietly = quietly)

  } else {
    solve_seq_fcn(game = game,
                  cons1 = cons1,
                  cons2 = cons2,
                  cons_common = cons_common,
                  precision = precision,
                  quietly = quietly)
  }
}
