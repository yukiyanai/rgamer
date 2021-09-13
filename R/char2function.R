#' @title Transform a pair of character function into R functions.
#' @description \code{char2function()} transform a pair of functions given by
#'    chracter strings into a pair of R functions.
#' @param f1 A character string of the payoff function of Player 1.
#' @param f2 A character string of the payoff function of Player 2.
#' @param pars A character vector specifying the parameters of payoff functions.
#' @return A list of two R functions, each of which is a player's  payoff
#'     function.
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
char2function <- function(f1, f2, pars) {

  f1 <- f1 %>%
    stringr::str_replace_all(pars[1],
                             replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all(pars[2],
                             replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x",
                               "SECOND_PARAM" = "y")) %>%
    str2expression()

  f2 <- f2 %>%
    stringr::str_replace_all(pars[1],
                             replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all(pars[2],
                             replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x",
                               "SECOND_PARAM" = "y")) %>%
    str2expression()

  ff1 <- function(x, y) {
    eval(f1, envir = list(x = x, y = y))
  }

  ff2 <- function(x, y) {
    eval(f2, envir = list(x = x, y = y))
  }

  return(list(ff1, ff2))
}
