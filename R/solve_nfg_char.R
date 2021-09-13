#' @title Find Nash equilibria of a normal-form game.
#' @description \code{solve_nfg_char()} finds the pair of the best responses
#'     of a normal-form game defined with payoff functions as character
#'     strings by \code{normal_form()}.
#' @return A list containing the pair of the best responses of two players (NE)
#'     and the plot of best response correspondences.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}
#' @param plot A logical value to determine whether the figure of the best
#'     response correspondences will be displayed. Default is \code{TRUE}.
#' @param mark_NE A logical value to control if the NE (if any) will be marked
#'     in the best response plot, which will be displayed (only displayed when
#'     \code{plot = TRUE}). Default is \code{FALSE}.
#' @param quietly A logical value to determine if the equilibrium will be kept
#'     in the returned list without being printed on screen. Default is
#'     \code{FALSE}.
#' @param color_palette A color palette to be used. Default is \code{"Set1"}.
#' @importFrom magrittr %>%
#' @include char2function.R
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_nfg_char <- function(game,
                           plot = TRUE,
                           mark_NE = FALSE,
                           quietly = FALSE,
                           color_palette = "Set1") {

  ff_list <- char2function(game$payoff[[1]],
                           game$payoff[[2]],
                           game$pars)

  g <- normal_form(players = game$player,
                   p1 = ff_list[[1]],
                   p2 = ff_list[[2]],
                   pars = c("x", "y"),
                   par1_lim = game$strategy[[1]],
                   par2_lim = game$strategy[[2]])

  solve_nfg_fcn(g,
                par_label = game$pars,
                plot = plot,
                mark_NE = mark_NE,
                quietly = quietly,
                color_palette = color_palette)
}
