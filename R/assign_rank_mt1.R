#' @title Assign rank to each match for many-to-one matching
#' @return A data frame
#' @param data A data frame of matching results.
#' @param g1_prefs A named list of preferences of individuals who make
#'     proposals.
#' @param g2_prefs A named list of preferences of individuals who receives
#'     proposals.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
assign_rank_mt1 <- function(data, g1_prefs, g2_prefs) {

  g1names <- names(g1_prefs)
  g2names <- names(g2_prefs) |>
    stringr::str_replace(pattern = "_\\d*", "") |>
    unique()

  data$match <- data$match |>
    stringr::str_replace(pattern = "_\\d*", "")
  data$name <- data$name |>
    stringr::str_replace(pattern = "_\\d*", "")

  df1 <- data[data$group == "proposer", ] |>
    dplyr::filter(!is.na(match))

  df2 <- data[data$group == "proposed", ]

  # Ranking for the proposers
  df_proposer <- data.frame(name = NULL,
                            rank = NULL)

  for (i in 1:length(g1names)) {
    PREF <- g1_prefs[[i]]
    g1_prefs[[i]] <- PREF |>
      stringr::str_replace(pattern = "_\\d*",
                           replacement = "") |>
      unique()
  }

  for (i in g1names) {
    row1 <- df1[df1$name == i, ]
    match_i <- row1$match[1]
    if (is.na(match_i)) {
      rank <- NA_integer_
    } else {
      rank <- which(g1_prefs[[i]] == match_i)
    }
    df_g1 <- data.frame(name = i,
                        match = match_i,
                        rank = rank,
                        group = "proposer")
    df_proposer <- dplyr::bind_rows(df_proposer, df_g1)
  }

  # Ranking for the proposed
  df_proposed <- data.frame(name = NULL,
                            rank = NULL)

  g2_prefs <- g2_prefs[paste0(g2names, "_1")]
  names(g2_prefs) <- g2names

  for (j in g2names) {
    row2 <- df2[df2$name == j, ]
    K <- nrow(row2)
    rank <- rep(NA, K)
    for (k in seq_along(rank)) {
      match_j <- row2$match[k]
      if (is.na(match_j)) {
        rank[k] <- NA_integer_
      } else {
        rank[k] <- which(g2_prefs[[j]] == match_j)
      }
    }
    df_g2 <- data.frame(name = rep(j, K),
                        match = row2$match,
                        rank = rank,
                        group = rep("proposed", K))
    df_proposed <- dplyr::bind_rows(df_proposed, df_g2)
  }

  dplyr::bind_rows(df_proposer, df_proposed) |>
    dplyr::distinct()
}
