#' @title Evaluate how desirable a matching result is
#' @description \code{eval_match()} evaluates how desirable a mathcing result is
#'    for a specified player.
#' @return An integer value indicating the rank of the match given their true
#'     preference. NA will be returned if there is no match.
#' @param x An object of the "matching" class obtained by \code{matching} or
#'     \code{matching_df()}.
#' @param name A character string of the name of a player to be evaluated.
#' @param group This argument determines whether the specified player belongs to
#'     the proposers or the proposed. If a proposing player and a proposed
#'     player have the same name, this argument must be either "proposer" or
#'     "proposed" in order to uniquely identify the player.  If all players have
#'     a unique name, this can be set to \code{NULL}. Default is \code{NULL}.
#' @param preference A vector representing the preference of the player.
#'    The first, second, third, ... element of the vector is the players' first,
#'    second, third, ... choice. For example, If the player preferes A to B to
#'    C, this argument should be \code{c("A", "B", "C")}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
eval_match <- function(x,
                       name,
                       group = NULL,
                       preference) {

  if (!is.null(group)) {
    group <- match.arg(group,
                       choices = c("proposer", "proposed"))
  }
  df <- x$data
  if (!is.null(group)) df <- df[df$group == group, ]
  matched <- df[df$name == name, 2]
  ifelse(is.null(matched), NA_integer_, which(preference == matched))
}
