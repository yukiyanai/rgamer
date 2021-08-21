#' @title Find dominated strategies in a normal-form game
#' @description \code{find_dominated()} finds each player's dominated
#'     and weakly dominated strategies if they exist.
#' @param game A normal-form game object created by \code{normal_form()}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @noRd
find_dominated <- function(game) {

  if (game$type != "matrix") {
    stop("game is not given by a payoff matrix")
  } else {
    n_s1 <- length(game$strategy$s1)
    n_s2 <- length(game$strategy$s2)

    ## Player 1
    dom_res_1 <- rep(FALSE, length.out = n_s1)
    if (n_s1 > 1) {
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
        if (sum(cond) > 0) {
          dom_res_1[i] <- "dominated"
          break
        } else if (sum(cond_w) > 0) dom_res_1[i] <- "weakly"
      }
    }

    ## Player 2
    dom_res_2 <- rep(FALSE, length.out = n_s2)
    if (n_s2 > 1) {
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
        if (sum(cond) > 0) {
          dom_res_2[i] <- "dominated"
          break
        } else if (sum(cond_w) > 0) dom_res_2[i] <- "weakly"
      }
    }

    ## to sum up
    dom_1 <- game$strategy$s1[dom_res_1 == "dominated"]
    if (length(dom_1) == 0) dom_1 <- NA

    wdom_1 <- game$strategy$s1[dom_res_1 == "weakly"]
    if (!is.na(dom_1)) {
      wdom_1 <- dom_1
    } else if (length(wdom_1) == 0) {
      wdom_1 <- NA
    }

    dom_2 <- game$strategy$s2[dom_res_2 == "dominated"]
    if (length(dom_2) == 0) dom_2 <- NA

    wdom_2 <- game$strategy$s2[dom_res_2 == "weakly"]
    if (!is.na(dom_2)) {
      wdom_2 <- dom_2
    } else if (length(wdom_2) == 0) {
      wdom_2 <- NA
    }

    message(paste0(game$player[1],
                   "'s dominated strategy: ",
                   paste(dom_1, collapse = ", ")))
    message(paste0(game$player[1],
                   "'s weakly dominated strategy: ",
                   paste(wdom_1, collapse = ", ")))
    message(paste0(game$player[2],
                   "'s dominated strategy: ",
                   paste(dom_2, collapse = ", ")))
    message(paste0(game$player[2],
                   "'s weakly dominated strategy: ",
                   paste(wdom_2, collapse = ", ")))

    dom_list <- list(dom_1, dom_2)
    names(dom_list) <- game$player
    wdom_list <- list(wdom_1, wdom_2)
    names(wdom_list) <- game$player

    return(list(dominated = dom_list,
                w_dominated = wdom_list))
  }
}
