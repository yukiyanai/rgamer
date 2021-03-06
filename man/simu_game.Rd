% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simu_game.R
\name{simu_game}
\alias{simu_game}
\title{Plays a normal-form game by simulation}
\usage{
simu_game(
  game,
  n_samples,
  n_periods,
  type = "br",
  init1 = NULL,
  init2 = NULL,
  rho = 0,
  lambda = 1,
  cons1 = NULL,
  cons2 = NULL
)
}
\arguments{
\item{game}{An object of \code{normal_form} class defined by \code{normal_form()}.}

\item{n_samples}{A positive integer specifying the number of samples to be simulated.}

\item{n_periods}{A positive integer specifying how many times the game is played within each sample.}

\item{type}{A character string to tell what kind of simulation should be run.
The available options are \code{"br"}, \code{"sbr"}, \code{"abr"}, and \code{"imitation"}.
With \code{"br"}, each player chooses the best response to the opponent's choice in
the previous period. With \code{"sbr"}, each player chooses the softly best response to
the opponent's choice in the previous periods, With \code{"abr"}, each player
alternately chooses the best response to the other player's previous action.
With \code{"imitation"}, each player imitates the opponent's choice in the previous period.
Players randomly choose their strategies or the first period in each of these options.}

\item{init1}{Player 1's first strategy. If not specified, a strategy is randomly selected
from the player's strategy set.}

\item{init2}{Player 2's first strategy. If not specified, a strategy is randomly selected
from the player's strategy set.}

\item{rho}{A numeric value in [0, 1] to control the degree of inertia in each player's behavior. If \code{rho = 1},
each player does not change their choices over time. If \code{rho = 0}, which is the default value, each player does not stick to their
previous choice at all.}

\item{lambda}{A positive value controlling the weight of the best response to the previous move of the opponent.}

\item{cons1}{A named list of parameters contained in \code{game$payoff$p1} that should be treated as constants, if any.}

\item{cons2}{A named list of parameters contained in \code{game$payoff$p2} that should be treated as constants, if any.}
}
\value{
A list of plays by player.
}
\description{
\code{simu_game()} simulates plays expected in a normal-form game.
}
\details{
Simulate plays expected in a normal-form game defined by \code{normal_form()}.
}
\author{
Yoshio Kamijo and Yuki Yanai <yanai.yuki@kochi-tech.ac.jp>
}
