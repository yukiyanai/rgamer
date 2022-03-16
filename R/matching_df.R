#' @title Implement matching
#' @description \code{matching_df()} implements matching between two groups of
#'    individuals with preferences specified by either a pair ofdata frames or
#'    a pair of data files (csv, tsv, or table).
#' @return A list of "matching" class containing
#'     (1) a data frame of the matching results,
#'     (2) a character string showing which algorithm was used,
#'     (3) a character string of the matching results,
#'     (4) a character string of the history of matching steps, and
#'     (5) a list of preferences of each group.
#' @param df1 A data frame or a data file containing preferences of
#'     the proposers.
#' @param df2 A data frame or a data file containing preferences of
#'     the proposed.
#' @param df_type Type of \code{df1} and \code{df2}, which should be one of
#'     \code{"data.frame"}, \code{"csv"}, \code{"tsv"}, or \code{"table"}.
#'     If not specified, \code{matching_df()} tries to determine the type.
#' @param header A logical value showing if the data file has the header
#'     (variable names) row. If not specified, default is \code{TRUE} for
#'     "csv" and "tsv", and \code{FALSE} for "table".
#' @param sep The field separator character. Values on each line of the
#'     file are separated by this character. If not specified,
#'     default is \code{","} for "csv", \code{"\t"} for "tsv", and \code{""}
#'     (white space) for "table".
#' @param algorithm A algorithm for matching. \code{"DA"}
#'     (\code{"Gale-Shapley"}) or \code{"Boston"}.
#' @param switch A logical value. If \code{TRUE}, the roles of g1 and g2 are
#'     switched. That is, g2 will be the proposer group, and g1 the prposed if
#'     \code{TRUE}. Default is \code{FALSE}.
#' @param verbose If \code{TRUE}, matching steps will be printed on screen.
#'     Default to \code{TRUE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#'
#' \dontrun{
#'   test7 <- matching_df(
#'     df1 = "foo1.csv",
#'     df2 = "foo2.csv")
#' }
matching_df <- function(df1,
                        df2,
                        df_type = NULL,
                        header = NULL,
                        sep = NULL,
                        algorithm = "DA",
                        switch = FALSE,
                        verbose = TRUE) {

  algorithm <- match.arg(algorithm,
                         choices = c("DA", "Gale-Shapley", "Boston"))

  if (!is.null(header)) {
    if (!is.logical(header))
      stop("'header' must be a logical value (TRUE or FALSE)")
  }

  ## Detect df types
  if (is.null(df_type)) {
    ## df1
    if (is.data.frame(df1)) {
      df_type_1 <- "data.frame"
    } else if (stringr::str_ends(df1, pattern = ".csv")) {
      df_type_1 <- "csv"
    } else if (stringr::str_ends(df1, pattern = ".tsv")) {
      df_type_1 <- "tsv"
    } else if (stringr::str_ends(df1, pattern = ".table|.txt|.dat")) {
      df_type_1 <- "table"
    } else {
      stop("df1 should be either a data frame or a csv/tsv/table file")
    }

    ## df2
    if (is.data.frame(df2)) {
      df_type_2 <- "data.frame"
    } else if (stringr::str_ends(df2, pattern = ".csv")) {
      df_type_2 <- "csv"
    } else if (stringr::str_ends(df2, pattern = ".tsv")) {
      df_type_2 <- "tsv"
    } else if (stringr::str_ends(df2, pattern = ".table|.txt|.dat")) {
      df_type_2 <- "table"
    } else {
      stop("df2 should be either a data frame or a csv/tsv/table file")
    }
  } else {
      df_type <- match.arg(df_type,
                           choices = c("data.frame", "csv", "tsv", "table"))
      df_type_1 <- df_type_2 <- df_type
  }

  ## Read df1
  if (df_type_1 == "data.frame") {
    f1 <- df1
  } else if (df_type_1 == "csv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- ","
    f1 <- utils::read.csv(df1, header = header, sep = sep)
  } else if (df_type_1 == "tsv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- "\t"
    f1 <- utils::read.delim(df1, header = header, sep = sep)
  } else {
    if (is.null(header)) header <- FALSE
    if (is.null(sep)) sep <- ""
    f1 <- utils::read.table(df1, header = header, sep = sep)
  }

  ## Read df2
  if (df_type_2 == "data.frame") {
    f2 <- df2
  } else if (df_type_2 == "csv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- ","
    f2 <- utils::read.csv(df2, header = header, sep = sep)
  } else if (df_type_2 == "tsv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- "\t"
    f2 <- utils::read.delim(df2, header = header, sep = sep)
  } else {
    if (is.null(header)) header <- FALSE
    if (is.null(sep)) sep <- ""
    f2 <- utils::read.table(df2, header = header, sep = sep)
  }

  n_g1 <- nrow(f1)
  n_g2 <- nrow(f2)

  g1_names <- f1[, 1]
  g2_names <- f2[, 1]

  ## g1: proposers
  g1_prefs <- list()
  for (i in 1:n_g1) {
    g1_prefs[[i]] <- unlist(f1[i, -1])
  }
  names(g1_prefs) <- g1_names

  ## g2: proposed
  g2_prefs <- list()
  for (i in 1:n_g2) {
    g2_prefs[[i]] <- unlist(f2[i, -1])
  }
  names(g2_prefs) <- g2_names

  matching(g1_prefs = g1_prefs,
           g2_prefs = g2_prefs,
           algorithm = algorithm,
           switch = switch,
           verbose = verbose)
}
