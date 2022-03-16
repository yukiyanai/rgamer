#' @title Assign rank to each match
#' @return A data frame
#' @param data A data frame of matching results.
#' @param g1_prefs A named list of preferences of individuals who make
#'     proposals.
#' @param g2_prefs A named list of preferences of individuals who receives
#'     proposals.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
assign_rank <- function(data, g1_prefs, g2_prefs) {

  g1names <- names(g1_prefs)
  g2names <- names(g2_prefs)

  df1 <- data[data$group == "proposer", ]
  df2 <- data[data$group == "proposed", ]

  # Ranking for the proposers
  df_proposer <- data.frame(name = NULL,
                            rank = NULL)

  for (i in g1names) {
    row1 <- df1[df1$name == i, ]
    match_i <- row1$match
    if (is.na(match_i)) {
      rank <- NA_integer_
    } else {
      match_pos <- which(g2names == match_i)
      rank <- which(g1_prefs[[i]] == match_pos)
    }
    df_g1 <- data.frame(name = i,
                        rank = rank,
                        group = "proposer")
    df_proposer <- dplyr::bind_rows(df_proposer, df_g1)
  }

  # Ranking for the proposed
  df_proposed <- data.frame(name = NULL,
                            rank = NULL)
  for (j in g2names) {
    row2 <- df2[df2$name == j, ]
    match_j <- row2$match
    if (is.na(match_j)) {
      rank <- NA_integer_
    } else {
      match_pos <- which(g1names == match_j)
      rank <- which(g2_prefs[[j]] == match_pos)
    }
    df_g2 <- data.frame(name = j,
                        rank = rank,
                        group = "proposed")
    df_proposed <- dplyr::bind_rows(df_proposed, df_g2)
  }

  df <- dplyr::bind_rows(df_proposer, df_proposed)

  dplyr::left_join(data, df, by = c("name", "group"))
}
