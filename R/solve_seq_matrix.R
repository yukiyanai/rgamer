#' @title Find Nash equilibria of a sequential-form game.
#' @description \code{solve_seq_matrix()} finds subgame perfect equilibria of
#'   a sequential-form (an extensive-form) game with discrete-choice strategies.
#' @return A list containing Nash equilibrium (NE) outcome and
#'   the gt table of the game.
#' @param game A "sequential_form" class object created by \code{seq_form()}.
#' @seealso \code{\link{seq_form}}
#' @param show_table A logical value. If \code{TRUE}, the table of the game will
#'   be displayed. Default is \code{TRUE}.
#' @param mark_br A logical value. If \code{TRUE}, the follower's best response
#'   to each of the leader's strategy is marked. Default is \code{FALSE}.
#' @param quietly A logical value that determines whether the equilibrium will
#'   be kept in the returned list without being printed on screen. Default is
#'   \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
solve_seq_matrix <- function(
  game,
  show_table = TRUE,
  mark_br = FALSE,
  quietly = FALSE) {

  s1 <- s2 <- NULL

  ## Follower's choice given leader's action
  sp1 <- game$strategy$s1
  p1_get <- rep(NA, length(sp1))
  sp2 <- rep(NA, length(sp1)) %>% as.list()
  for (i in seq_along(sp1)) {
    dd <- game$df %>%
      dplyr::filter(s1 == sp1[i])
    s2_chosen <- dd$s2[which.max(dd$p2)]
    dd <- dd %>%
      dplyr::filter(s2 %in% s2_chosen)
    p1_get[i] <- mean(dd$p1)
    sp2[[i]] <- s2_chosen
  }
  sp1_chosen <- sp1[tuple::matchAll(max(p1_get), p1_get)]

  if (length(sp1_chosen > 1)) {
    NE <- rep(NA, length(sp1_chosen))
    for (i in seq_along(NE)) {
      sp1_ci <- sp1_chosen[i]
      NE[i] <- paste0("(", sp1_ci, ", ",
                         sp2[[match(sp1_ci, sp1_chosen)]], ")")
    }
  } else {
    NE <- paste0("(", sp1_chosen, ", ",
                  sp2[[which.max(p1_get)]], ")")
  }

  if (!quietly) message("NE outcome: ", NE)

  mat_tbl <- game_table(game, mark_br = mark_br)
  if (show_table) print(mat_tbl)

  return(list(NE = NE, table = mat_tbl))
}
