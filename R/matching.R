#' @title Implement matching
#' @description \code{matching()} implements matching between two groups of
#'    individuals.
#' @return A list of "matching" class containing
#'     (1) a data frame of the matching results,
#'     (2) a character string showing which algorithm was used,
#'     (3) a character string of the matching results,
#'     (4) a character string of the history of matching steps, and
#'     (5) a list of preferences of each group.
#' @param g1_prefs A list of preferences of individuals who make
#'     proposals.
#' @param g2_prefs A named list of preferences of individuals who receives
#'     proposals.
#' @param g1_names A vector of names of the proposers. You can pass a named list
#'     to \code{g1_prefs} instead of specifying \code{g1_names}.
#' @param g2_names A vector of names of the proposers. You can pass a named list
#'     to \code{g2_prefs} instead of specifying \code{g2_names}.
#' @param algorithm A algorithm for matching. \code{"DA"}
#'     (\code{"Gale-Shapley"}) or \code{"Boston"}.
#' @param switch A logical value. If \code{TRUE}, the roles of g1 and g2 are
#'     switched. That is, g2 will be the proposer group, and g1 the prposed if
#'     \code{TRUE}. Default is \code{FALSE}.
#' @param verbose If \code{TRUE}, matching steps will be printed on screen.
#'     Default to \code{TRUE}.
#' @param mt1 A logical valu. \code{TRUE} for many-to-one matching.
#'     Default to \code{FALSE}
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' test1 <- matching(
#'   g1_prefs = list(w1 = c(1, 2),
#'                   w2 = c(2, 1),
#'                   w3 = c(1, 2)),
#'   g2_prefs = list(m1 = c(1, 2, 3),
#'                   m2 = c(2, 3, 1)))
#'
#' test2 <- matching(
#'   g1_names = c("w1", "w2", "w3"),
#'   g1_prefs = list(c(1, 2),
#'                   c(2, 1),
#'                   c(1, 2)),
#'   g2_names = c("m1", "m2"),
#'   g2_prefs = list(c(1, 2, 3),
#'                   c(2, 3, 1)))
#'
#' test3 <- matching(
#'   g1_names = c("Amy", "Beatrice", "Cindy"),
#'   g1_prefs = list(c("Dick", "Eric"),
#'                   c("Eric", "Dick"),
#'                   c("Dick", "Eric")),
#'   g2_names = c("Dick", "Eric"),
#'   g2_prefs = list(c("Amy", "Beatrice", "Cindy"),
#'                   c("Beatrice", "Cindy", "Amy")))
#'
#' test4 <- matching(
#'   g1_prefs <- list(w1 = c(1, 2),
#'                    w2 = c(2, 1),
#'                    w3 = c(1, 2)),
#'   g2_prefs <- list(m1 = c(1, 2),
#'                    m2 = c(2, 3)))
#'
#' \dontrun{
#'   ## The following function throws an error because a name (Jack) does not
#'   ## exist in the opponent's pool.
#'
#'   test5 <- matching(
#'     g1_names = c("Amy", "Beatrice", "Cindy"),
#'     g1_prefs = list(c("Dick", "Eric"),
#'                     c("Eric", "Dick"),
#'                     c("Dick", "Jack")),
#'     g2_names = c("Dick", "Eric"),
#'     g2_prefs = list(c("Amy", "Beatrice", "Cindy"),
#'                     c("Beatrice", "Cindy", "Amy")))
#' }
matching <- function(g1_prefs,
                     g2_prefs,
                     g1_names = NULL,
                     g2_names = NULL,
                     algorithm = "DA",
                     switch = FALSE,
                     verbose = TRUE,
                     mt1 = FALSE) {

  algorithm <- match.arg(algorithm,
                         choices = c("DA", "Gale-Shapley", "GS",
                                     "Boston"))

  g1_prefs0 <- g1_prefs
  g2_prefs0 <- g2_prefs

  n_g1 <- length(g1_prefs)
  n_g2 <- length(g2_prefs)

  ## Check the length of preferences and names
  if (!is.null(g1_names)) {
    if (length(g1_names) != n_g1)
      stop("g1_pref and g1_names must be the same length.")
  }

  if (!is.null(g2_names)) {
    if (length(g2_names) != n_g2)
      stop("g2_pref and g2_names must be the same length.")
  }


  ## name the list of preferences
  if (is.null(g1_names)) {
    if (is.null(names(g1_prefs))) {
      g1_names <- paste0("proposer_", 1:n_g1)
      names(g1_prefs) <- g1_names
    } else {
      g1_names <- names(g1_prefs)
    }
  } else {
    names(g1_prefs) <- g1_names
  }

  if (is.null(g2_names)) {
    if (is.null(names(g2_prefs))) {
      g2_names <- paste0("proposed_", 1:n_g2)
      names(g2_prefs) <- g2_names
    } else {
      g2_names <- names(g2_prefs)
    }
  } else {
    names(g2_prefs) <- g2_names
  }

  ## If preferences are specified by names, deal with that
  prefs_mod <- pref_name2num(g1_prefs, g2_prefs)
  g1_prefs <- prefs_mod$g1_prefs
  g2_prefs <- prefs_mod$g2_prefs

  ## Check if all individuals provide complete ordering of preferences.
  ## If not, fix it.
  for (i in 1:n_g1) {
    S <- length(g1_prefs[[i]])
    if (S < n_g2) {
      extra <- rep(NA, n_g2 - S)
      g1_prefs[[i]] <- c(g1_prefs[[i]], extra)
    } else if (S > n_g2) {
      stop("g1_prefs incorrectly specified: redundant preference order[s] detected")
    }
  }
  for (i in 1:n_g2) {
    S <- length(g2_prefs[[i]])
    if (S < n_g1) {
      extra <- rep(NA, n_g1 - S)
      g2_prefs[[i]] <- c(g2_prefs[[i]], extra)
    } else if (S > n_g1) {
      stop("g2_prefs incorrectly specified: redundant preference order[s] detected")
    }
  }

  ## Matching!!
  if (switch) {
    tmp <- g1_prefs
    g1_prefs <- g2_prefs
    g2_prefs <- tmp
  }
  if (algorithm %in% c("DA", "Gale-Shapley", "GA")) {
    out <- DA(g1_prefs = g1_prefs,
              g2_prefs = g2_prefs,
              verbose = verbose)
  } else if (algorithm == "Boston") {
    out <- Boston(g1_prefs = g1_prefs,
                  g2_prefs = g2_prefs,
                  verbose = verbose)
  }

  if (mt1) {
    out$data <- assign_rank_mt1(out$data,
                                g1_prefs = g1_prefs0,
                                g2_prefs = g2_prefs0)
    out$preference$proposer <- g1_prefs0
    out$preference$proposed <- g2_prefs0

  } else {
    out$data <- assign_rank(out$data,
                            g1_prefs = g1_prefs,
                            g2_prefs = g2_prefs)
    out$preference$proposer <- g1_prefs
    out$preference$proposed <- g2_prefs
  }

  structure(out, class = "matching")
}
