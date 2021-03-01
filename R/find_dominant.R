#' @title Find dominant strategies in a normal-form game
#' @description \code{find_dominant()} finds each player's dominant and weakly dominant strategies if they exist.
#' @details
#' @param game A normal-form game object created by \code{normal_form()}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
find_dominant <- function(game) {

  if (game$type == "matrix") {
    n_s1 <- length(game$strategy$s1)
    n_s2 <- length(game$strategy$s2)

    ## Player 1
    dom_res_1 <- rep(FALSE, length.out = n_s1)
    if (n_s1 > 1) {
      for (i in 1:n_s1) {
        not_i <- c(1:n_s1)[-i]
        cond_w <- cond <- rep(FALSE, length.out = length(not_i))
        for (j in seq_along(not_i)) {
          if (all(game$mat$matrix1[i, ] > game$mat$matrix1[not_i[j], ])) {
            cond[j] <- TRUE
          } else if (all(game$mat$matrix1[i, ] >= game$mat$matrix1[not_i[j], ])) {
            cond_w[j] <- TRUE
          }
        }
        if (all(cond)) {
          dom_res_1[i] <- "dominant"
          break
        } else if (all(cond_w)) dom_res_1[i] <- "weakly"
      }
    }

    ## Player 2
    dom_res_2 <- rep(FALSE, length.out = n_s2)
    if (n_s2 > 1) {
      for (i in 1:n_s2) {
        not_i <- c(1:n_s2)[-i]
        cond_w <- cond <- rep(FALSE, length.out = length(not_i))
        for (j in seq_along(not_i)) {
          if (all(game$mat$matrix2[, i] > game$mat$matrix2[, not_i[j]])) {
            cond[j] <- TRUE
          } else if (all(game$mat$matrix2[, i] >= game$mat$matrix2[, not_i[j]])) {
            cond_w[j] <- TRUE
          }
        }
        if (all(cond)) {
          dom_res_2[i] <- "dominant"
          break
        } else if (all(cond_w)) dom_res_2[i] <- "weakly"
      }
    }

    # to sum up
    dom_1 <- game$strategy$s1[dom_res_1 == "dominant"]
    if (length(dom_1) != 1) dom_1 <- NA

    wdom_1 <- game$strategy$s1[dom_res_1 == "weakly"]
    if (length(wdom_1) != 1) wdom_1 <- NA

    dom_2 <- game$strategy$s2[dom_res_2 == "dominant"]
    if (length(dom_2) != 1) dom_2 <- NA

    wdom_2 <- game$strategy$s2[dom_res_2 == "weakly"]
    if (length(wdom_2) != 1) wdom_2 <- NA

    message(paste0(game$player[1],
                   "'s dominant strategy: ",
                   dom_1))
    message(paste0(game$player[1],
                   "'s weakly dominant strategy: ",
                   wdom_1))
    message(paste0(game$player[2],
                   "'s dominant strategy: ",
                   dom_2))
    message(paste0(game$player[2],
                   "'s weakly dominant strategy: ",
                   wdom_2))

    dom_list <- list(dom_1, dom_2)
    names(dom_list) <- game$player
    wdom_list <- list(wdom_1, wdom_2)
    names(wdom_list) <- game$player

    return(list(dominant = dom_list,
                w_dominant = wdom_list))
  }
}
