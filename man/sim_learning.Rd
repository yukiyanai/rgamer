% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_learning.R
\name{sim_learning}
\alias{sim_learning}
\title{Simulates a learning model}
\usage{
sim_learning(
  game,
  n_samples,
  n_periods,
  type = "EWA",
  lambda = 1,
  delta = 0.5,
  rho = 0.5,
  phi = 0.5,
  A1_init = 0,
  A2_init = 0,
  N_init = 0,
  plot_range_y = NULL
)
}
\arguments{
\item{game}{An object of \code{normal_form} class defined by
\code{normal_form()}.}

\item{n_samples}{A positive integer specifying the number of samples to be
simulated.}

\item{n_periods}{A positive integer specifying how many times the game is
played within each sample.}

\item{type}{A character string to tell which learning models should be
simulated. The available options are \code{"EWA"},
\code{"reinforcement"} (choice reinforcement), and \code{"belief"}
(belief based model).  \code{"reinforcement"} and \code{"belief"} are
special cases of \code{"EWA"}.}

\item{lambda}{A positive real value representing the players' sensitivity to
attraction values of strategies. As \code{lambda} gets larger, the choice
will be dependent on attraction values more heavily.  As \code{lambda}
gets close to 0, a strategy will tend to be chosen randomly.}

\item{delta}{A real number between 0 and 1. This parameter controls how fast
attraction values of strategies that are not chosen are updated.  If
\code{delta = 0}, attraction is updated only for the strategy that is
selected at the given period (i.e., reinforcement learning is
implemented). If \code{delta = 1}, attraction is updated equally for all
strategies (i.e., belief-based learning model is applied).}

\item{rho}{A real value between 0 and 1. This parameter controls the learning
speed. \code{rho = 0} for "reinforcement" leaning and "belief" based
learning.}

\item{phi}{A real value between 0 and 1. This parameter controls how much
attraction values at the current period are constrained by the past
attraction values. If \code{phi = 0}, the past attraction values are
ignored. \code{phi = 1} for "reinforcement" leaning and "belief" based
learning.}

\item{A1_init}{An initial value of Player 1's attraction for each strategy.}

\item{A2_init}{An initial value of Player 2's attraction for each strategy.}

\item{N_init}{An initial value of N.}

\item{plot_range_y}{Choose the range of vertical axis for plots. Available
choices are \code{"fixed"}, \code{"full"} and \code{"free"}.
If \code{plot_range_y = "free"}, the range of y-axis depends on
simulation results.  If \code{plot_range_y = "full"}, The range
defined in \code{game} is used for each player, which can be different
between players. With \code{"fixed"}, the same y-axis is used for both
players.}
}
\value{
A list containing (1) a list of data frames of strategies chosen by
each player, (2) a single long data frame of (1)'s data frames combined,
(3) a list of each  player's attraction values for each strategy
(data frames), (4) a list of probability of each strategy being chosen
(data frames), and (5) a plot of the simulation result (ggplot object).
}
\description{
\code{sim_learning()} simulates learning dynamics in a
normal-form game expected by an experienced weighted attraction (EWA)
model.
}
\details{
Simulate plays of a normal-form game defined by
\code{normal_form()} in a way expected by an EWA model.
}
\author{
Yoshio Kamijo and Yuki Yanai \href{mailto:yanai.yuki@kochi-tech.ac.jp}{yanai.yuki@kochi-tech.ac.jp}
}
