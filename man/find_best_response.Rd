% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_best_response.R
\name{find_best_response}
\alias{find_best_response}
\title{Find best responses to the opponent's actions}
\usage{
find_best_response(game)
}
\arguments{
\item{game}{A "normal_form" class object created by \code{normal_form()}.}
}
\value{
A data frame containing the pair of the best responses of two players.
}
\description{
\code{find_best_response()} finds the best responses to each of the opponent's strategy.
}
\seealso{
\code{\link{normal_form}}
}
\author{
Yoshio Kamijo and Yuki Yanai <yanai.yuki@kochi-tech.ac.jp>
}
