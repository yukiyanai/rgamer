#' @title Find dominated or dominant strategies in a normal-form game
#' @description \code{dom()} finds each player's (weakly) dominated
#'     or (weakly) dominant strategies.
#' @return A list of dominated and weakly dominated strategies or a list of
#'     dominant and weakly dominant strategies.
#' @param game An object of "normal_form" class created by \code{normal_form()}.
#'     The game's type must be "matrix".
#' @param type Which to be found, "dominated" or "dominant".
#'     Default is \code{"dominated"}.
#' @param quietly If \code{TRUE}, the message telling dominated (or dominant)
#'     strategies will not be shown. Default is \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
dom <- function(game,
                type = "dominated",
                quietly = FALSE) {

  type <- match.arg(type, choices = c("dominated", "dominant"))

  if (type == "dominated") {
    find_dominated(game, quietly = quietly)
  } else {
    find_dominant(game, quietly = quietly)
  }
}
