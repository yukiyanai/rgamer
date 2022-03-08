#' @title Custom print method for "matching" class
#' @description Custom print method for "matching" class created by
#'     \code{matching()} or \code{matching_df()}.
#' @inheritParams print
#' @param x The output of \code{matching()} or \code{matching_df()}.
#' @param digits Not used.
#' @param quote Not used.
#' @param na.print Not used.
#' @param zero.print Not used.
#' @param right Not used.
#' @param justify Not used.
#' @param ... Not used.
#' @param type What to print. The available options are "results", "history",
#'     and "both" (both results and history). Default to \code{"results"}.
#' @export
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
print.matching <- function(x,
                           digits = getOption("digits"),
                           quote = FALSE,
                           na.print = "",
                           zero.print = "0",
                           right = is.numeric(x) || is.complex(x),
                           justify = "none",
                           ...,
                           type = "results") {

  if (type == "results") {
    #print("Matching Result")
    cat(x$results)
    cat("\n")
  } else if (type == "history") {
    #print("Matching Hisotry")
    cat(x$history)
  } else if (type == "both") {
    #print("Matching History and Result")
    cat(x$history)
    cat(x$results)
    cat("\n")
  }
  invisible(x)
}
