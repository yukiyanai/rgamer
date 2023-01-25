#' @title Implement many-to-one matching
#' @description \code{matching_mt1()} implements many-to-one matching between
#'    two groups of individuals with preferences specified by either a pair of
#'    data frames or a pair of data files (csv, tsv, or table).
#' @return A list of "matching" class containing
#'     (1) an extended data frame of the matching results,
#'     (2) a character string showing which algorithm was used,
#'     (3) a character string of the matching results,
#'     (4) a character string of the history of matching steps,
#'     (5) a list of preferences of each group,
#'     (6) a data frame of the matching results cleaned.
#' @param df_many A data frame or a data file containing preferences of
#'     the proposers.
#' @param df_one A data frame or a data file containing preferences of
#'     the proposed.
#' @param capacity A value specifies how many people each of the proposed
#'     accepts as their matches. Give a value to this parameter when the
#'     capacity is constant among the proposed. Otherwise, specify
#'     \code{capacity_df} instead.
#' @param capacity_df A data frame specifying how many people each of the
#'     proposed accepts as their matches. The first column must be the name
#'     of the proposed, which must match the first column of \code{df_one}.
#'     The second column is the capacity for each of the proposed.
#' @param df_type Type of \code{df_many} and \code{df_one}, which should be one of
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
#'     (\code{"Gale-Shapley"},  \code{"GS"}) or \code{"Boston"}.
#' @param verbose If \code{TRUE}, matching steps will be printed on screen.
#'     Default to \code{TRUE}.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
matching_mt1 <- function(df_many,
                         df_one,
                         capacity = NULL,
                         capacity_df = NULL,
                         df_type = NULL,
                         header = NULL,
                         sep = NULL,
                         algorithm = "DA",
                         verbose = TRUE) {

  group <- name <- NULL

  f1 <- read_matching_data(data = df_many,
                           df_type = df_type,
                           header = header,
                           sep = sep)

  f2 <- read_matching_data(data = df_one,
                           df_type = df_type,
                           header = header,
                           sep = sep)


  names(f1) <- c("name",
                 paste0("pref_", 1:(ncol(f1) - 1)))

  names(f2) <- c("name",
                 paste0("pref_", 1:(ncol(f2) - 1)))

  p_names <- unlist(f1[, 1])
  r_names <- unlist(f2[, 1])

  if (is.null(capacity_df)) {
    if (is.null(capacity)) capacity <- 1

    capacity_df <- data.frame(r_name = r_names,
                              capacity = capacity)
  } else {
    capacity_df <- read_matching_data(data = capacity_df,
                                      header = header,
                                      sep = sep)
    names(capacity_df) <- c("r_name", "capacity")
  }

  ## Extend the data frame of the proposers
  p_list <- list()
  n_prefs <- sum(capacity_df$capacity)

  for (i in 1:nrow(f1)) {
    x <- unlist(f1[i, -1])
    x2 <- c()
    for (j in seq_along(x)) {
      x_target <- x[j]
      if (is.na(x_target)) break
      a <- capacity_df$capacity[capacity_df$r_name == x_target]
      x_tmp <- paste(x_target, 1:a, sep = "_")
      x2 <- c(x2, x_tmp)
    }
    names(x2) <- paste0("pref_", 1:length(x2))
    p_list[[i]] <- x2
  }

  DF1 <- dplyr::bind_rows(p_list)
  DF1$p_name <- p_names
  DF1 <- dplyr::select(DF1,
                       p_name, dplyr::starts_with("pref"))

  ## Extend the data frame of the proposed
  r_list <- list()
  for (i in 1:nrow(f2)) {
    r_name_i <- f2[i, 1]
    y <- f2[i, -1]
    b <- capacity_df$capacity[i]
    df_r <- data.frame(NULL)
    for (j in 1:b) {
      df_r_tmp <- data.frame(r_name = paste0(r_name_i, "_", j))
      df_r_tmp <- dplyr::bind_cols(df_r_tmp, y)
      df_r <- dplyr::bind_rows(df_r, df_r_tmp)
    }
    r_list[[i]] <- df_r
  }

  DF2 <- dplyr::bind_rows(r_list)

  # Implement matching
  m <- matching_df(
    df1 = DF1,
    df2 = DF2,
    df_type = "data.frame",
    verbose = verbose,
    algorithm = algorithm,
    mt1 = TRUE)

  # Fix proposers' ranking
  res_proposer <- m$data |>
    dplyr::filter(group == "proposer") |>
    dplyr::mutate(match = stringr::str_replace(match,
                                               pattern = "_\\d+",
                                               replacement = ""),
                  rank = NA_integer_)

  for (i in 1:nrow(res_proposer)) {
    p_name <- res_proposer$name[i]
    x <- f1[f1$name == p_name, -1]
    res_proposer$rank[i] <- which(x == res_proposer$match[i])
  }

  # Create the result data for the proposed
  res_proposed <- data.frame(
    name = res_proposer$match,
    match = res_proposer$name,
    group = "proposed",
    rank = NA_integer_
  ) |>
    dplyr::arrange(name)

  for (i in 1:nrow(res_proposed)) {
     r_name <- res_proposed$name[i]
     x <- f2[f2$name == r_name, -1]
     res_proposed$rank[i] <- which(x == res_proposed$match[i])
  }

  res_proposed <- res_proposed |>
    dplyr::arrange(name, rank)

  m$data_cleaned <- dplyr::bind_rows(res_proposer,
                                     res_proposed)

  return(m)
}
