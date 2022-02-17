#' @title Custom print method for "matching" class
#' @description Custom print method for "matching" class created by
#'     \code{matching()}.
#' @param x The output of \code{matching()}.
#' @param type What to print. The available options are "results", "history",
#'     and "both" (both results and history). Default to \code{"results"}.
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
print.matching <- function(x, type = "results") {
  if (type == "results") {
    cat(x$results)
    cat("\n")
  } else if (type == "history") {
    cat(x$history)
  } else if (type == "both") {
    cat(x$history)
    cat(x$results)
    cat("\n")
  }
}
