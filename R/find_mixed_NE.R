#' @title Find a mixed-strategy Nash equilibrium.
#' @description \code{find_mixed_NE} finds mixed-strategy Nash equilibrium of
#'     a normal-form game with discrete-choice strategies.
#' @return A list of the probabilities given to each strategy that specifies
#'     the mixed-strategy Nash equilibrium and a list of mixed-strategy Nash
#'     equilibria of the subsets of strategies
#' @param game A "normal_form" class object created by \code{normal_form()}.
#'     The game's type must be "matrix".
#' @seealso \code{\link{normal_form}}
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
find_mixed_NE <- function(game) {

  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]

  if (length(s1) < 2 | length(s2) < 2) {
    stop("find_mixed_NE() doesn't work for a game with a single-strategy player.")
  }

  find_sets <- function(s) {
    s <- as.list(s)
    2:length(s) |>
      lapply(function(x) apply(utils::combn(s, x), 2, function(y) y)) |>
      unlist(recursive = FALSE) |>
      lapply(unlist)
  }
  s1_sets <- find_sets(s1)
  s2_sets <- find_sets(s2)

  pair <- expand.grid(row = 1:length(s1_sets),
                      col = 1:length(s2_sets)) |>
    dplyr::arrange(row, col)

  msNE_list <- NULL
  msNE_df <- data.frame(NULL)
  n_msNE <- 0
  for (k in 1:nrow(pair)) {

    s1_sub <- s1_sets[[pair$row[k]]]
    s2_sub <- s2_sets[[pair$col[k]]]

    mat1 <- game$mat$matrix1[s1 %in% s1_sub, s2 %in% s2_sub]
    mat2 <- game$mat$matrix2[s1 %in% s1_sub, s2 %in% s2_sub]

    n_rows <- length(s1_sub)
    n_cols <- length(s2_sub)

    a1 <- matrix(NA, nrow = n_cols, ncol = n_rows)
    b1 <- matrix(NA, nrow = n_cols, ncol = 1)
    a1[1, ] <- 1
    b1[1, 1] <- 1
    for (i in 2:n_cols) {
      a1[i, ] <- mat2[, (i - 1)] - mat2[, i]
      b1[i, 1] <- 0
    }
    prob1 <- tryCatch({
      solve(a1, b1)
    }, error = function(e) {
      NULL
    })

    a2 <- matrix(NA, nrow = n_rows, ncol = n_cols)
    b2 <- matrix(NA, nrow = n_rows, ncol = 1)
    a2[1, ] <- 1
    b2[1, 1] <- 1
    for (i in 2:n_rows) {
      a2[i, ] <- mat1[(i - 1), ] - mat1[i,]
      b2[i, 1] <- 0
    }
    prob2 <- tryCatch({
      solve(a2, b2)
    }, error = function(e) {
      NULL
    })

    if (is.null(prob1) | is.null(prob2)) {
      msNE <- NULL
      p_msNE <- NA_character_
      q_msNE <- NA_character_
    } else {
      prob1 <- as.vector(prob1)
      prob2 <- as.vector(prob2)

      if (!all(prob1 > 0) | !all(prob1 < 1) |
          !all(prob2 > 0) | !all(prob2 < 1)) {
        msNE <- NULL
        p_msNE <- NA_character_
        q_msNE <- NA_character_
      } else {
        n_msNE <- n_msNE + 1
        msNE <- list(s1 = prob1, s2 = prob2)

        p_msNE <- paste0("(", paste(prob1, collapse = ", "), ")")
        q_msNE <- paste0("(", paste(prob2, collapse = ", "), ")")

      }
    }

    sub_df <- data.frame(
      player1 = paste0("(", paste(s1_sub, collapse = ", "), ")"),
      player2 = paste0("(", paste(s2_sub, collapse = ", "), ")"),
      p_msNE = p_msNE,
      q_msNE = q_msNE)

    msNE_df <- dplyr::bind_rows(msNE_df, sub_df)

  }

  return(list(msNE = msNE,
              msNE_df = msNE_df,
              probs = list(p = prob1,
                           q = prob2)))
}
