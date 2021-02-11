#' @title Find dominated strategies in a normal-form game
#' @description \code{find_dominated()} finds each player's dominated and weakly dominated strategies if they exist.
#' @details
#' @param game A normal-form game object created by \code{normal_form()}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
find_dominated <- function(game) {

  if (game$type == "matrix") {
    n_s1 <- length(game$strategy$s1)
    n_s2 <- length(game$strategy$s2)

    ## Player 1
    dom_res_1 <- rep(FALSE, length.out = n_s1)
    for (i in 1:n_s1) {
      not_i <- c(1:n_s1)[-i]
      cond_w <- cond <- rep(FALSE, length.out = length(not_i))
      for (j in seq_along(not_i)) {
        if (all(game$mat$matrix1[i, ] < game$mat$matrix1[not_i[j], ])) {
          cond[j] <- TRUE
        } else if (all(game$mat$matrix1[i, ] <= game$mat$matrix1[not_i[j], ])) {
          cond_w[j] <- TRUE
        }
      }
      if (all(cond)) {
        dom_res_1[i] <- "dominated"
        break
      }
      else if (all(cond_w)) dom_res_1[i] <- "weakly"
    }

    ## Player 2
    dom_res_2 <- rep(FALSE, length.out = n_s2)
    for (i in 1:n_s2) {
      not_i <- c(1:n_s2)[-i]
      cond_w <- cond <- rep(FALSE, length.out = length(not_i))
      for (j in seq_along(not_i)) {
        if (all(game$mat$matrix2[, i] < game$mat$matrix2[, not_i[j]])) {
          cond[j] <- TRUE
        } else if (all(game$mat$matrix2[, i] <= game$mat$matrix2[, not_i[j]])) {
          cond_w[j] <- TRUE
        }
      }
      if (all(cond)) {
        dom_res_2[i] <- "dominated"
        break
      }
      else if (all(cond_w)) dom_res_2[i] <- "weakly"
    }
    dom_1 <- ifelse(
      "dominated" %in% dom_res_1,
      game$strategy$s1[dom_res_1 == "dominated"],
      NA)
    wdom_1 <- ifelse(
      "weakly" %in% dom_res_1,
      game$strategy$s1[dom_res_1 == "weakly"],
      NA)
    dom_2 <- ifelse(
      "dominated" %in% dom_res_2,
      game$strategy$s2[dom_res_2 == "dominated"],
      NA)
    wdom_2 <- ifelse(
      "weakly" %in% dom_res_2,
      game$strategy$s2[dom_res_2 == "weakly"],
      NA)

    message(paste0(game$player[1], "'s dominated strategy: ", dom_1))
    message(paste0(game$player[1], "'s weakly dominated strategy: ", wdom_1))
    message(paste0(game$player[2], "'s dominated strategy: ", dom_2))
    message(paste0(game$player[2], "'s weakly dominated strategy: ", wdom_2))

    return(list(dominated = list(player1 = dom_1, player2 = dom_2),
                w_dominated = list(player1 = wdom_1, player2 = wdom_2)))
  }
}
