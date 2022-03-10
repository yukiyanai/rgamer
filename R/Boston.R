#' @title Implement Boston matching algorithm
#' @description \code{Boston()} implements Boston mechanism matching
#' @return A list containing (1) a data frame of the matching results,
#'     (2) a character string showing which algorithm was used,
#'     (3) a character string of the matching results, and
#'     (4) a character string of the history of matching steps.
#' @param g1_prefs A named list of preferences of individuals who make
#'     proposals.
#' @param g2_prefs A named list of preferences of individuals who receives
#'     proposals.
#' @param verbose If \code{TRUE}, matching steps will be printed on screen.
#'     Default to \code{TRUE}.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
Boston <- function(g1_prefs,
                   g2_prefs,
                   verbose = TRUE) {

  g1p <- g1_prefs
  g2p <- g2_prefs

  g1_names <- names(g1p)
  g2_names <- names(g2p)

  ## the number of individuals in each group
  n_g1 <- length(g1p)
  n_g2 <- length(g2p)

  ## number of matches among proposers
  num_match <- 0

  ## record the matching process
  history <- ""

  ## players not yet matched
  g1_notyet <- 1:n_g1

  ## data frame to record matches
  df_match <- data.frame(NULL)

  ## steps
  t <- 1
  while (num_match < n_g1) {
    step_print <- TRUE
    df_propose <- data.frame(NULL)
    for (i in g1_notyet) {
      if (length(g1p[[i]]) < 1) {
        df0 <- data.frame(g1 = i,
                          g2 = NA_integer_)
        df_match <- dplyr::bind_rows(df_match, df0)
        break
      }
      df1 <- data.frame(from = i,
                        to = g1p[[i]][1])
      df_propose <- dplyr::bind_rows(df_propose, df1)
    }
    df_accept <- data.frame(NULL)
    for (j in 1:n_g2) {
      offers <- df_propose$from[df_propose$to == j]
      if (length(offers) < 1) next
      if (step_print) {
        history <- paste(history, "Step", t, "\n")
        step_print <- FALSE
      }
      if (length(offers) == 1) {
        history <- paste(history, " ", g1_names[offers],
                         "proposes", g2_names[j], "\n")
      } else {
        offers_str <- paste(g1_names[offers], collapse = ", ")
        history <- paste(history, " ", offers_str,
                         "propose", g2_names[j], "\n")
      }
      for (k in 1:length(g2p[[j]])) {
        if (g2p[[j]][k] %in% offers) {
          accept <- g2p[[j]][k]
          history <- paste(history, " ", g2_names[j],
                           "accepts", g1_names[accept], "\n\n")
          df2 <- data.frame(g1 = accept,
                        g2 = j)
          df_accept <- dplyr::bind_rows(df_accept, df2)
          break
        }
      }
    }
    df_match <- dplyr::bind_rows(df_match, df_accept)

    num_match <- nrow(df_match)

    g1_done <- 1:n_g1 %in% df_match$g1
    g1_notyet <- (1:n_g1)[!g1_done]

    for (i in g1_notyet) {
      prefs0 <- g1p[[i]]
      prefs_left <- prefs0[!(prefs0 %in% df_match$g2)]
      g1p[[i]] <- prefs_left
    }
    t <- t + 1
  }

  if (verbose) cat(history)

  res_char <- ""
  res_char <- paste(res_char, "Results\n")
  for (i in 1:n_g1) {
    res_char <- paste(res_char,
                      g1_names[df_match$g1[i]], ":",
                      g2_names[df_match$g2[i]], "\n")
  }
  g2_matched <- g2_names[stats::na.omit(df_match$g2)]
  g2_left <- (1:n_g2)[!(g2_names %in% g2_matched)]
  for (j in g2_left) {
     res_char <- paste(res_char, "NA :", g2_names[j], "\n")
     df_left <- data.frame(g1 = NA_integer_,
                           g2 = j)
     df_match <- dplyr::bind_rows(df_match, df_left)
  }

  g1_partner <- NULL
  df_g1partner <- df_match[!is.na(df_match$g1),]
  for (i in 1:n_g1) {
    g1_partner <- c(g1_partner, df_g1partner$g2[df_g1partner$g1 == i])
  }
  g1_partner <- g2_names[g1_partner]

  g2_partner <- NULL
  df_g2partner <- df_match[!is.na(df_match$g2),]
  for (j in 1:n_g2) {
    g2_partner <- c(g2_partner, df_g2partner$g1[df_g2partner$g2 == j])
  }
  g2_partner <- g1_names[g2_partner]

  df <- data.frame(
    name = c(g1_names, g2_names),
    match = c(g1_partner, g2_partner),
    group = c(rep("proposer", n_g1), rep("proposed", n_g2))
  )

  if (verbose) cat(res_char)

  return(list(data = df,
              algorithm = "Boston",
              results = res_char,
              history = history))
}
