#' @title Find solutions of an extensive-form game.
#' @description \code{solve_efg} finds solutions of an extensive-form game
#'     defined by \code{extensive_form()}.
#' @param game An "extensive_form" class object created by
#' @param concept Solution concept to be used. It must be one of
#'     \code{"backward"} (backward induction) or \code{"spe"} (subgame perfect
#'     equilibrium).
#' @param quietly A logical value. If \code{TRUE}, the solution(s) will not be
#'     displayed on screen. Default is \code{FALSE}.
#' @return A list containing (1) \code{sols}: a list of solutions, if any, (2)
#'     \code{n_sols}: the number of solutions found, and (3) \code{trees}: a
#'     list of game trees each of which shows which paths were played under
#'     each solution.
#' @include backward_induction.R
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' g1 <- extensive_form(
#'   players = list("Kamijo",
#'                  rep("Yanai", 2),
#'                  rep(NA, 4)),
#'   actions <- list(c("U", "D"),
#'                   c("U'", "D'"), c("U''", "D''")),
#'   payoffs = list(Kamijo = c(0, 2, 1, 3),
#'                  Yanai  = c(0, 1, 2, 1)))
#' s1 <- solve_efg(g1)
#'
#' g2 <- extensive_form(
#'   players = list("f",
#'                  c("m", "m"),
#'                  rep(NA, 4)),
#'   actions = list(c("ballet", "baseball"),
#'                  c("ballet", "baseball"), c("ballet", "baseball")),
#'   payoffs = list(f = c(2, 0, 0, 1),
#'                  m = c(1, 0, 0, 2)),
#'   show_node_id = FALSE)
#' s2 <- solve_efg(g2, quietly = TRUE)
solve_efg <- function(game,
                      concept = "backward",
                      quietly = FALSE) {

  if (class(game) != "extensive_form")
    stop("game must be an object of 'extensive_form' class defined by extensive_form() function.")

  concept <- match.arg(concept,
                       choices = c("backward", "spe"))

  if (concept == "backward") {
    out <- backward_induction(game)
  } else if (concept == "spe") {
    stop("funciton to find SPE is under construction.")
  } else {
    sol <- NULL
  }

  if (!quietly) {
    if (length(out$sol) > 1) {
      message("backward induction: ",
              paste(out$sol, collapse = ", "))
    } else {
      message("backward induction: ", out$sol)
    }
  }

  value <- list(sols = out$sol,
                n_sols = length(out$sol),
                trees = out$sol_tree)

 structure(value, class = "extensive_sol")
}
