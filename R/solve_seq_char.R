#' @title Find subgame perfect equilibria outcome of a sequential-form game
#'     (extensive form game) with  payoffs defined by character strings
#' @description \code{solve_seq_char()} finds the subgame perfect equilibrium
#'     outcomes when payoff functions are provided as character strings.
#' @return A list containing the subgame perfect equilibrium (SPE) outcomes
#'     and the plot of best response correspondences.
#' @param game A "sequential_form" class object created by \code{seq_form()}.
#' @seealso \code{\link{seq_form}}
#' @param precision A natural number specifying the precision of numerical approximation.
#'     The value n approximately means that the approximation is correct up to the Nth decimal place.
#'     The default value is 1.
#' @param quietly A logical value to determine if the equilibrium will be kept in the returned list
#'     without being printed on screen. Default is \code{FALSE}.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_seq_char <- function(game,
                           precision = 1L,
                           quietly = FALSE) {

  p1 <- game$payoff[[1]]
  p2 <- game$payoff[[2]]
  pars <- game$pars
  par1_lim <- game$strategy[[1]]
  par2_lim <- game$strategy[[2]]

  delta <- 10^(-(precision + 2))

  f1 <- p1 %>%
    stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x", "SECOND_PARAM" = "y")) %>%
    str2expression()
  ff1 <- function(x, y) {
    eval(f1, envir = list(x = x, y = y))
  }

  f2 <- p2 %>%
    stringr::str_replace_all(pattern = pars[1], replacement = "FIRST_PARAM") %>%
    stringr::str_replace_all( pattern = pars[2], replacement = "SECOND_PARAM") %>%
    stringr::str_replace_all(c("FIRST_PARAM" = "x", "SECOND_PARAM" = "y")) %>%
    str2expression()
  ff2 <- function(x, y) {
    eval(f2, envir = list(x = x, y = y))
  }


  par1_seq <- seq(from = par1_lim[1],
                  to = par1_lim[2],
                  length.out = 50)
  par2_seq <- seq(from = par2_lim[1],
                  to = par2_lim[2],
                  length.out = 50)


  while (TRUE) {
    dif <- max(par1_seq[2] - par1_seq[1],
               par2_seq[2] - par2_seq[1])
    out <- gridsearch_backward(f1 = ff1,
                               f2 = ff2,
                               x_vec = par1_seq,
                               y_vec = par2_seq)


    if (dif < delta) {
      break
    } else {
      x_bottom <- mean(c(par1_seq[1], out$x))
      x_top <- mean(c(par1_seq[50], out$x))
      par1_seq <- seq(from = x_bottom,
                      to = x_top,
                      length.out = 50)

      y_bottom <- mean(c(par2_seq[1], out$y))
      y_top <- mean(c(par2_seq[50], out$y))
      par2_seq <- seq(from = y_bottom,
                      to = y_top,
                      length.out = 50)
    }
  }

  x <- round(out$x, precision)
  y <- round(out$y, precision)

  SPE <- paste0("(",
                format(x, nsmall = precision), ", ",
                format(y, nsmall = precision), ")")

  payoff = paste0("(",
                  ff1(x, y), ", ",
                  ff2(x, y), ")")




  if (!quietly) message("SPE outcome: ", SPE)

  message("#  The SPE shown here was numerically obtained and \n",
          "#  can be slightly different from the analytical solution.")

  return(list(spe = SPE, payoff = payoff))
}
