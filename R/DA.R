#' @title Implement DA matching algorithm
#' @description \code{DA()} implements DA matching
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
DA <- function(g1_prefs,
               g2_prefs,
               verbose = TRUE) {

  g1_names <- names(g1_prefs)
  g2_names <- names(g2_prefs)

  ## the number of individuals in each group
  n_g1 <- length(g1_prefs)
  n_g2 <- length(g2_prefs)

  ## ordered preferences
  reorder_prefs <- function(x) {
    x2 <- rep(NA, n_g1)
    for (i in 1:n_g1) {
     x2[i] <- match(i, x)
    }
    x2
  }
  g2_order <- lapply(g2_prefs, reorder_prefs)

  ## placeholders for matched partners
  g1_matched <- rep(0, n_g1)
  g2_matched <- rep(0, n_g2)

  ## number of matches among proposers
  num_match <- 0

  ## check if matched
  g1_filled <- rep(FALSE, n_g1)
  g2_filled <- rep(FALSE, n_g2)

  ## how many proposals have been offered?
  position <- rep(1, n_g1)

  ## record the matching process
  history <- ""

  ## steps
  t <- 1
  while (num_match < n_g1) {
    step_print <- TRUE
    #history <- paste(history, "Step", t, "\n")
    for (i in 1:n_g1) {
      if (!g1_filled[i]) {
        ## To whom i proposes (j)
        j <- g1_prefs[[i]][position[i]]

        if (is.na(j)) { # if no cadidate is left
          g1_matched[i] <- NA
          g1_filled[i] <- TRUE
          num_match <- num_match + 1
          next
        }

        if (step_print) {
          history <- paste(history, "Step", t, "\n")
          step_print <- FALSE
        }

        history <- paste(history, "  ",
                         g1_names[i], "proposes", g2_names[j], "\n")

        ## j's current match
        k <- g2_matched[j]

        if (is.na(g2_order[[j]][i])) {
          history <- paste(history, "  ",
                           g2_names[j], "rejects", g1_names[i], "\n")
          position[i] <- position[i] + 1

          if (position[i] > n_g2) {
            g1_matched[i] <- NA
            g1_filled[i] <- TRUE
            num_match <- num_match + 1
          }
        } else if (!g2_filled[j]) {
          history <- paste(history, "  ",
                          g1_names[i], "and", g2_names[j],
                          "temporarily match\n")
          g1_matched[i] <- j
          g2_matched[j] <- i
          g1_filled[i] <- TRUE
          g2_filled[j] <- TRUE

          num_match <- num_match + 1

        } else if (g2_order[[j]][i] < g2_order[[j]][k]) {
          history <- paste(history, "  ",
                           g2_names[j], "rejects", g1_names[k], "\n")
          position[k] <- position[k] + 1
          g1_matched[i] <- j
          g2_matched[j] <- i
          history <- paste(history, "  ",
                           g1_names[i], "and", g2_names[j],
                          "temporarily match\n")
          g1_filled[i] <- TRUE
          g1_filled[k] <- FALSE
          g2_filled[j] <- TRUE
        } else {
          history <- paste(history, "  ",
                           g2_names[j], "rejects", g1_names[i], "\n")
          position[i] <- position[i] + 1

          if (position[i] > n_g2) {
            g1_matched[i] <- NA
            g1_filled[i] <- TRUE
            num_match <- num_match + 1
          }
        }
        history <- paste(history, "\n")
      }
    }
    t <- t + 1
  }

  if (verbose) cat(history)

  g1_partner <- ifelse(is.na(g1_matched),
                       NA_character_,
                       g2_names[g1_matched])
  g2_partner <- ifelse(is.na(g2_matched),
                       NA_character_, g1_names[g2_matched])

  res_char <- ""

  res_char <- paste(res_char, "Results\n")
  for (i in 1:n_g1) {
    res_char <- paste(res_char, g1_names[i], ":", g1_partner[i], "\n")
  }
  for (j in 1:n_g2) {
    if (!g2_filled[j]) res_char <- paste(res_char, "NA :", g2_names[j], "\n")
  }

  df <- data.frame(
    name = c(g1_names, g2_names),
    match = c(g1_partner, g2_partner),
    group = c(rep("proposer", n_g1), rep("proposed", n_g2))
  )

  return(list(data = df,
              algorithm = "DA (Gale-Shapley)",
              results = res_char,
              history = history))
}
