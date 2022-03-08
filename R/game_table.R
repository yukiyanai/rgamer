#' @title Create a table of the game
#' @description \code{game_table()} creates and displays the payoff matrix of
#'     the game.
#' @return A table of the payoff matrix of a normal-form game.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#'     The game's type must be "matrix".
#' @seealso \code{\link{normal_form}}
#' @param mark_br A logical value. If \code{TRUE}, the best response to each of
#'     the opponent's strategy is marked. Default is \code{TRUE}.
#' @importFrom magrittr %>%
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
game_table <- function(game, mark_br = TRUE) {

  pid <- column <- NULL

  players <- game$player
  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  mat1 <- game$mat$matrix1
  mat2 <- game$mat$matrix2

  if (mark_br) {
    BR <- find_best_response(game)
    BR1 <- BR %>% dplyr::filter(pid == 1)
    if (nrow(BR1) > 0) {
      rows1 <- BR1 %>% dplyr::pull(row)
      cols1 <- BR1 %>% dplyr::pull(column)
      for (s in seq_along(rows1)) {
        mat1[rows1[s], cols1[s]] <- paste0(mat1[rows1[s], cols1[s]], '^')
      }
    }
    BR2 <- BR %>% dplyr::filter(pid == 2)
    if (nrow(BR2) > 0) {
      rows2 <- BR2 %>% dplyr::pull(row)
      cols2 <- BR2 %>% dplyr::pull(column)
      for (s in seq_along(rows2)) {
        mat2[rows2[s], cols2[s]] <- paste0(mat2[rows2[s], cols2[s]], '^')
      }
    }
  }

  n_rows <- length(s1)
  n_cols <- length(s2)

  mat <- matrix(paste(mat1, mat2, sep = ", "),
                ncol = n_cols)

  row.names(mat) <- s1
  colnames(mat) <- s2

  ## Create the payoff matrix in HTML with kableExtra::kbl()
  mat0 <- mat %>%
    tibble::as_tibble(rownames = "strategy") %>%
    as.matrix()
  row.names(mat0) <- rep("", nrow(mat0))
  row.names(mat0)[1] <- players[1]

  mat_tbl <- mat0 %>%
    kableExtra::kbl(booktabs = TRUE,
                    align = "c") %>%
    kableExtra::kable_classic(html_font = "Arial") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              latex_options = "scale_down") %>%
    kableExtra::add_header_above(data.frame(c("", players[2]),
                                            c(2, length(s2))),
                                 bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE)

  return(mat_tbl)
}
