#' @title Find dominated and dominant strategies in a normal-form game
#' @description \code{dom()} finds each player's (weakly) dominated
#'     or (weakly) dominant strategies
#' @param game A normal-form game object created by \code{normal_form()}.
#' @param type Specify which to be found, "dominated" or "dominant."
#'   Default is "dominated."
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
dom <- function(game,
                type = "dominated") {

  match.arg(type, choices = c("dominated", "dominant"))

  if (type == "dominated") {
    find_dominated(game)
  } else {
    find_dominant(game)
  }
}
