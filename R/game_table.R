#' @title Create a table of the game
#' @description \code{game_table()} creates and displays a gt table of the game.
#' @return A gt table of the normal form game.
#' @param game A "normal_form" class object created by \code{normal_form()}.
#' @seealso \code{\link{normal_form}}, \code{\link[gt]{gt}}
#' @param mark_br A logical value. If \code{TRUE}, the best response to each of the opponent's strategy is marked.
#'   Default is \code{TRUE}.
#' @param cell_width A number specifying the cell width of the game matrix. The unit is pixel. If not specified,
#'     the function tries to find the appropriate size.
#' @importFrom magrittr %>%
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
game_table <- function(game, mark_br = TRUE, cell_width = NULL) {

  players <- game$player
  s1 <- game$strategy[[1]]
  s2 <- game$strategy[[2]]
  mat1 <- game$mat$matrix1
  mat2 <- game$mat$matrix2

  if (mark_br) {
    BR <- find_best_response(game)
    BR1 <- BR %>%  dplyr::filter(pid == 1)
    if (nrow(BR1) > 0) {
      rows1 <- BR1 %>% dplyr::pull(row)
      cols1 <- BR1 %>% dplyr::pull(column)
      for (s in seq_along(rows1)) {
        mat1[rows1[s], cols1[s]] <- paste0(mat1[rows1[s], cols1[s]], '^')
      }
    }
    BR2 <- BR %>%  dplyr::filter(pid == 2)
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

  ## determine the width of cells
  if (is.null(cell_width)) {
    s1_length <- stringr::str_length(s1)
    s2_length <- stringr::str_length(s2)
    pname_length <- stringr::str_length(players)
    pname_length[2] <- ceiling(pname_length[2] / 2)
    cell_size <- mat %>% as.vector() %>% stringr::str_length()
    max_length <- c(s1_length, s2_length, pname_length, cell_size) %>% max()
    cell_width <- 12 * max_length
    if (cell_width > 200) cell_width = 200
  } else if (cell_width > 200) {
    warning("If the table doesn't fit to the screen, please make cell_width smaller.")
  }
  if (exists("cellw")) cellw_rgamer_temp <- cellw
  cellw <<- paste0(cell_width, "px")

  ## Create the game matrix in HTML format with gt::gt()
  mat_tbl <- mat %>%
    tibble::as_tibble(rownames = "strategy") %>%
    gt::gt() %>%
    gt::tab_spanner(
      label = players[2],
      columns = (1:n_cols) + 1
    ) %>%
    gt::tab_row_group(
      label = players[1],
      rows = 1:n_rows
    ) %>%
    gt::cols_align(
      align = "center",
      columns = tidyselect::everything()
    ) %>%
    gt::cols_width(
      tidyselect::everything() ~ cellw
    )

  if (exists("cellw_rgamer_temp")) cellw <<- cellw_rgamer_temp

  return(mat_tbl)
}
