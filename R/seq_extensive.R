#' @title Transform a sequential_form into extensive_form
#' @description \code{seq_extensive} transforms a seq_form game defined by
#'     \code{seq_form} into an extensive_form game.
#' @param game A sequential_form object created by \code{seq_form()}. The game's
#'     type must be "matrix".
#' @param ... Other parameter used to define an extensive-form game.
#' @seealso \code{\link{extensive_form}}
#' @return An object of "extensive_form" class.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' g1 <- seq_form(
#'   players = c("Leader", "Follower"),
#'   s1 = c("R", "S", "P"),
#'   s2 = c("R", "S", "P"),
#'   payoffs1 = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
#'   payoffs2 = c(0, 1, -1, -1, 0, 1, 1, -1, 0))
#' g1e <- seq_extensive(g1)
#' g1e2 <- seq_extensive(g1, direction = "right", color_palette = "Accent")
seq_extensive <- function(game, ...) {

  if (!methods::is(game, "sequential_form"))
    stop("game must be a 'sequential_form' object created by seq_form().")

  if (game$type != "matrix")
    stop("This function works only with 'matrix' type games.")

  player1 <- game$player[1]
  player2 <- game$player[2]

  s1 <- game$strategy$s1
  s2 <- game$strategy$s2

  players <- list(player1,
                  rep(player2, length(s1)),
                  rep(NA, length(s1) * length(s2)))

  actions <- c(list(s1), rep(list(s2), length(s1)))

  payoff1 <- as.vector(t(game$mat$matrix1))
  payoff2 <- as.vector(t(game$mat$matrix2))
  payoffs <- list(payoff1, payoff2)
  names(payoffs) <- c(player1, player2)

  extensive_form(players = players,
                 actions = actions,
                 payoffs = payoffs,
                 ...)
}
