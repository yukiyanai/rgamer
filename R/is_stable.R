#' @title Check if a matching is stable
#' @description \code{is_stable()} checks if a matching result is stable.
#' @return A list containing (1) a logical value showing if a matching is
#'     stable and (2) a data frame containing the information of blocking pairs,
#'     if any.
#' @param x An object of the "matching" class obtained by \code{matching} or
#'     \code{matching_df()}.
#' @param verbose If \code{TRUE}, the result and blocking pairs will be printed
#'     on screen. Default is \code{FALSE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
is_stable <- function(x, verbose = FALSE) {

  g1p <- x$preference$proposer
  g2p <- x$preference$proposed
  g1names <- names(g1p)
  g2names <- names(g2p)

  data <- x$data

  df1 <- data[data$group == "proposer", ]
  df2 <- data[data$group == "proposed", ]

  block <- data.frame(NULL)

  for (i in 1:nrow(df1)) {
    r1 <- df1[i, ]
    current_match <- r1$rank
    if (is.na(current_match)) {
      current_match <- length(g1p[[r1$name]]) + 1
    }
    if (current_match == 1) {
      next
    } else {
      for (j in 1:(current_match - 1)) {
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
      for (i in 1:nrow(block)) {
        cat(paste0("(", block$proposer[i], ", ", block$proposed[i], ")\n"))
      }
    }
  }

  return(out)
}
