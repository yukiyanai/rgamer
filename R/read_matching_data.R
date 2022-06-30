#' @title Read data for matching
#' @description \code{read_matching_data()} reads a file to be used in
#'    matching_df() or matching_mt1().
#' @return A data frame.
#' @param data A data frame or a data file containing preferences of
#'     proposers or proposed.
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
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
read_matching_data <- function(data,
                               df_type = NULL,
                               header = NULL,
                               sep = NULL) {

  if (!is.null(header)) {
    if (!is.logical(header))
      stop("'header' must be a logical value (TRUE or FALSE)")
  }

  ## Detect data types
  if (is.null(df_type)) {
    if (is.data.frame(data)) {
      df_type <- "data.frame"
    } else if (stringr::str_ends(data, pattern = ".csv")) {
      df_type <- "csv"
    } else if (stringr::str_ends(data, pattern = ".tsv")) {
      df_type <- "tsv"
    } else if (stringr::str_ends(data, pattern = ".table|.txt|.dat")) {
      df_type <- "table"
    } else {
      stop("data should be either a data frame or a csv/tsv/table file")
    }
  } else {
    df_type <- match.arg(df_type,
                         choices = c("data.frame", "csv", "tsv", "table"))
  }

  ## Read data
  if (df_type == "data.frame") {
    df <- data
  } else if (df_type == "csv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- ","
    df <- utils::read.csv(data, header = header, sep = sep)
  } else if (df_type == "tsv") {
    if (is.null(header)) header <- TRUE
    if (is.null(sep)) sep <- "\t"
    df <- utils::read.delim(data, header = header, sep = sep)
  } else {
    if (is.null(header)) header <- FALSE
    if (is.null(sep)) sep <- ""
    df <- utils::read.table(data, header = header, sep = sep)
  }

  return(df)
}
