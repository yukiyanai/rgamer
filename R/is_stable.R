#' @title Check if a matching is stable
#' @description \code{is_stable()} checks if a matching result is stable.
#' @return A list containing (1) a logical value showing if a matching is
#'     stable and (2) a data frame containing the information of blocking pairs,
#'     if any.
#' @param x An object of the "matching" class obtained by \code{matching} or
#'     \code{matching_df()}. Not used if \code{pairs}, \code{proposer_pref},
#'     and \code{proposed_pref} are provided.
#' @param pairs_list An optional list of matched pairs where each element of the list
#'   is a vector of matched paris. The first element of the vector is a proposer
#'.  and the second is the person who accepted the proposal.
#' @param pairs_df An optional data frame whose first column contains the
#'   proposers' names and the second the proposed. Use \code{NA} for "no
#'   matching." This will be ignored if \code{x} is provided.
#' @param proposer_pref A named list of preference of the proposer. The name
#'   of the list must exactly same as the proposer's name, and the contents of
#'   list must be the names of the proposed. This will be ignored if \code{x}
#'   is provided.
#' @param proposed_pref  A named list of preference of the proposer. The name
#'   of the list must exactly same as the name of the proposed, and the contents
#'   of the list must be the proposers' names. This will be ignored if \code{x}
#'   is provided.
#' @param verbose If \code{TRUE}, the result and blocking pairs will be printed
#'     on screen. Default is \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
is_stable <- function(x = NULL,
                      pairs_list = NULL,
                      pairs_df = NULL,
                      proposer_pref = NULL,
                      proposed_pref = NULL,
                      verbose = FALSE) {

  if (!is.null(x) & methods::is(x, "matching")) {
    g1p <- x$preference$proposer
    g2p <- x$preference$proposed
    g1names <- names(g1p)
    g2names <- names(g2p)

    data <- x$data

    df1 <- data[data$group == "proposer", ]
    df2 <- data[data$group == "proposed", ]

  } else if (is.vector(proposer_pref) &
             is.vector(proposed_pref)) {

    if (is.null(names(proposer_pref)))
      stop("Please pass a named list to proposer_pref")
    if (is.null(names(proposed_pref)))
      stop("Please pass a named list to proposed_pref")

    if (is.list(pairs_list) & is.null(pairs_df)) {
      pairs_df <- pairs_list |>
        unlist() |>
        matrix(ncol = 2, byrow = TRUE) |>
        as.data.frame()
    }

    n1 <- pairs_df[, 1]
    n2 <- pairs_df[, 2]
    data <- tibble::tibble(name = c(n1, n2),
                           match = c(n2, n1),
                           group = rep(c("proposer", "proposed"),
                                       each = length(n1)))
    data <- data[!is.na(data$name), ]


    ## If preferences are specified by names, deal with that
    prefs_mod <- pref_name2num(proposer_pref, proposed_pref)
    g1p <- prefs_mod$g1_prefs
    g2p <- prefs_mod$g2_prefs
    g1names <- names(g1p)
    g2names <- names(g2p)

    data <- assign_rank(data,
                        g1_prefs = g1p,
                        g2_prefs = g2p)
    df1 <- data[data$group == "proposer", ]
    df2 <- data[data$group == "proposed", ]
  } else {
    stop("Please sepcify x or all of pairs_list (or pairs_df), proposer_pref, and proposed_pref")
  }


  block <- data.frame(NULL)

  for (i in 1 : nrow(df1)) {
    r1 <- df1[i, ]
    current_match <- r1$rank
    if (is.na(current_match)) {
      current_match <- length(g1p[[r1$name]]) + 1
    }
    if (current_match == 1) {
      next
    } else {
      for (j in 1 : (current_match - 1)) {
        better_match <- g2names[g1p[[r1$name]][j]]
        opp_rank <- which(g1names[g2p[[better_match]]] == r1$name)
        r2 <- df2[df2$name == better_match, ]
        if (length(opp_rank) == 0) opp_rank <- NA
        if (is.na(opp_rank)) {
          next
        } else if (is.na(r2$rank) | opp_rank < r2$rank) {
          bdf <- data.frame(proposer = r1$name,
                            proposed = better_match,
                            proposer_current = r1$rank,
                            proposer_better = j,
                            proposed_current = r2$rank,
                            proposed_better = opp_rank)
          block <- dplyr::bind_rows(block, bdf)
          break
        }
      }
    }
  }

  ## check duplicates for many-to-one matching
  if (!is.null(x)) {
    if (nrow(block) != 0 & !is.null(x$data_cleaned)) {
      keep <- rep(NA, nrow(block))
      for (i in 1:nrow(block)) {
        p_name_i <- block$proposer[i]
        cur_match <- stringr::str_replace(
          df1[df1$name == p_name_i, "match"],
          pattern = "_\\d+",
          replacement = "")
        new_match <- stringr::str_replace(
          block$proposed[i],
          pattern = "_\\d+",
          replacement = "")
        keep[i] <- ifelse(cur_match == new_match, FALSE, TRUE)
      }
      block <- block[keep, ]
    }
  }


  if (nrow(block) == 0) {
    res <- TRUE
    block <- NULL
  } else {
    res <- FALSE
  }
  out <- list(stable = res,
              blocking_pairs = block)

  if (verbose) {
    if (res) {
      cat("This matching is stable.\n")
    } else {
      cat("This matching is unstable.\nBlocking pairs:\n")
      for (i in 1 : nrow(block)) {
        cat(paste0("(", block$proposer[i], ", ", block$proposed[i], ")\n"))
      }
    }
  }

  return(out)
}
