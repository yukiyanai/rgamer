#' @title Eliminate a strategy from a normal-form game.
#' @description \code{eliminate_strategy()} eliminates a strategy from a normal-form game defined by \code{norma_form()}.
#' @details
#' @param game A normal-form game object created by \code{normal_form()}.
#' @param player A player one of whose strategies will be eliminated.
#' @param eliminated A strategy to be eliminated. Case sensitive.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
eliminate_strategy <- function(game,
                               player,
                               eliminated) {

  elim_pos <- which(game$player == player)

  for (i in 1:length(game$strategy)) {
    if (i == elim_pos) {
      game$strategy[[i]] <- game$strategy[[i]][game$strategy[[i]] != eliminated]
    }
  }

  new_df <- game$df %>%
    filter(s1 %in% game$strategy$s1,
           s2 %in% game$strategy$s2)

  new_game <- normal_form(players = game$player,
                          s1 = game$strategy$s1,
                          s2 = game$strategy$s2,
                          p1 = new_df$p1,
                          p2 = new_df$p2)
  return(new_game)
}
