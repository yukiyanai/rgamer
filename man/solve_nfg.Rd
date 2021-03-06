% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solve_nfg.R
\name{solve_nfg}
\alias{solve_nfg}
\title{Find equilibria of a normal-form game}
\usage{
solve_nfg(
  game,
  mixed = FALSE,
  show_table = TRUE,
  cell_width = NULL,
  mark_br = TRUE,
  delta = 0.1,
  cons1 = NULL,
  cons2 = NULL,
  cons_common = NULL,
  precision = 1,
  plot = TRUE,
  mark_NE = FALSE,
  quietly = FALSE,
  color_palette = "Set1"
)
}
\arguments{
\item{game}{A "normal_form" class object created by \code{normal_form()}.}

\item{mixed}{A logical value. If \code{TRUE}, mixed-strategy NE will be calculated. Default is \code{FALSE}.}

\item{show_table}{A logical value. If \code{TRUE}, the table of the game will be displayed. Default is \code{TRUE}.}

\item{cell_width}{A number specifying the cell width of the game matrix. The unit is pixel.
The default value is 80.}

\item{mark_br}{A logical value. If \code{TRUE}, the best response to each of the opponent's strategy is marked.
Default is \code{TRUE}.}

\item{delta}{A numerical value specifying the grid size to draw the figure of best response correspondences.
The default value is 0.1. The smaller the value is, the smoother the correspondence curves are.}

\item{cons1}{A named list of parameters contained in \code{game$payoff$p1} that should be treated as constants, if any.}

\item{cons2}{A named list of parameters contained in \code{game$payoff$p2} that should be treated as constants, if any.}

\item{cons_common}{A named list of parameters contained in \code{game$payoff$p1} and \code{game$payoff$p2} that should be treated as constants, if any.
If \code{cons1} and \code{cons2} are exactly same, you can specify \code{cons_common} instead of specifying
both \code{cons1} and \code{cons2}.}

\item{precision}{A natural number specifying the precision of numerical approximation.
The value n approximately means that the approximation is correct up to the Nth decimal place.
The default value is 1.}

\item{plot}{A logical value to determine whether the figure of the best response correspondences
will be displayed. Default is \code{TRUE}.}

\item{mark_NE}{A logical value to control if the NE (if any) will be marked in the best response
plot, which will be displayed (only dislayed when \code{plot = TRUE}). Default is \code{FALSE}.}

\item{quietly}{A logical value that determines whether the equilibrium will be kept in the returned list
without being printed on screen. Default is \code{FALSE}}

\item{color_palette}{A color palette to be used. Default is \code{"Set1"}.}
}
\value{
A list containing Nash equilibria (if any), the game table (if available),
    and the plot of best response correspondence (if available).
}
\description{
\code{solve_nfg()} finds Nash equilibria of a normal-form game. This is a wrapper
    function of \code{\link{solve_nfg_matrix}}, \code{\link{solve_nfg_char}}, and
    \code{\link{solve_nfg_fcn}}.
}
\examples{
game1 <- normal_form(
  s1 = c("T", "B"), s2 = c("L", "R"),
  p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1))
s1 <- solve_nfg(game1, show_table = FALSE)

game2 <- normal_form(
  p1 = "-x1^2 + (28 - x2) * x1",
  p2 = "-x2^2 + (28 - x1) * x2",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x1", "x2"))
s2 <- solve_nfg(game2)

fx <- function(x, y) -x^2 + (28 - y) * x
fy <- function(x, y) -y^2 + (28 - x) * y
game3 <- normal_form(
    p1 = fx,
    p2 = fy,
    pars = c("x", "y"),
    par1_lim = c(0, 40),
    par2_lim = c(0, 40))
s3 <- solve_nfg(game3)
}
\author{
Yoshio Kamijo and Yuki Yanai <yanai.yuki@kochi-tech.ac.jp>
}
