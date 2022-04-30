#' @title Define a normal-form (or strategic-form) game
#' @description \code{normal_form()} defines a normal-form game and
#'     creates an object of "normal_form" class.
#' @details Creates an object of "normal_form" class, which can be passed to
#'     functions in order to find solutions of the game.
#' @param players A character vector of the name (label) for the players.
#' @param s1 A character vector of pure strategies for Player 1 (row player).
#'     Required only when the player has discrete-choice strategies.
#' @param s2 A character vector of pure strategies for Player 2 (column player).
#'      Required only when the player has discrete-choice strategies.
#' @param payoffs1 The payoff of Player1. This argument can be specified in
#'     three different ways. First, it can be a numeric vector of payoffs.
#'     Second, it can be a character string of the payoff function
#'     (e.g., payoffs1 = "x^2 - y"). Third, it can be an R function of payoff.
#' @param payoffs2 The payoff of Player 2. See the explanation of
#'     \code{payoffs1} for detail.
#' @param cells A list of vectors to specify the payoff for each cell. Each
#'     element of the list should be a numeric vector of two players' payoffs.
#' @param discretize A logical value. Set this \code{TRUE} to evaluate payoff
#'     functions at some discrete values of strategies \code{s1} and \code{s2}.
#'     Default is \code{FALSE}.
#' @param discrete_points A numeric vector of length 2 to set how many discrete
#'     points should be used to discretize the game defined by payoff functions.
#'     Default is \code{c(6, 6)}, which shows equally spaced 6 values from the
#'     range of the strategies \code{par1_lim} and \code{par2_lim}. Instead of
#'     setting this parameter, you can specify the vectors of arbitrary
#'     strategies by setting \code{s1} and \code{s2}.
#' @param symmetric A logical value. Set this \code{TRUE} when the payoffs for
#'     two players are symmetric as in the prisoners' dilemma. Then, payoffs1 is
#'     recycled for payoffs2. Default is \code{FALSE}.
#' @param byrow A logical value. If \code{TRUE}, payoffs will be lined up by
#'     row. Default is \code{FALSE}. Only used when both \code{s1} and \code{s2}
#'     are provided.
#' @param pars A character vector of parameters that are selected by players 1
#'     and 2, respectively. Only used when \code{payoffs1} and \code{payoffs2}
#'     are specified by payoff functions (either as character strings or R
#'     functions).
#' @param par1_lim A numeric vector of length 2, which defines the range of
#'     parameters from which Player 1 chooses her strategy.
#' @param par2_lim A numeric vector of length 2, which defines the range of
#'     parameters from which Player 2 chooses his strategy.
#' @param cons1 A named list of parameters contained in \code{payoffs1} that
#'     should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{payoffs2} that
#'     should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in \code{payoffs1}
#'     and \code{payoffs2} that should be treated as constants, if any. If
#'     \code{cons1} and \code{cons2} are exactly same, you can specify
#'     \code{cons_common} instead of both \code{cons1} and \code{cons2}.
#' @return An object of "normal_form" class, which defines a normal-form (or
#'     strategic-form) game.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' game1 <- normal_form(
#'   s1 = c("T", "B"),
#'   s2 = c("L", "R"),
#'   payoffs1 = c(4, 2, 3, 1),
#'   payoffs2 = c(4, 3, 2, 1),
#'   players = c("Row Player", "Column Player"))
#'
#' game1b <- normal_form(
#'   s1 = c("T", "B"),
#'   s2 = c("L", "R"),
#'   cells = list(c(4, 4),
#'                c(2, 3),
#'                c(3, 2),
#'                c(1, 1)),
#'   players = c("Row Player", "Column Player"))
#'
#' game2 <- normal_form(
#'    players = c("A", "B"),
#'    payoffs1 = "-x1^2 + (28 - x2) * x1",
#'    payoffs2 = "-x2^2 + (28 - x1) * x2",
#'    par1_lim = c(0, 30),
#'    par2_lim = c(0, 30),
#'    pars = c("x1", "x2"))
#'
#' fx <- function(x, y, a, b) -x^a + (b - y) * x
#' fy <- function(x, y, s, t) -y^s + (t - x) * y
#' game3 <- normal_form(
#'   payoffs1 = fx,
#'   payoffs2 = fy,
#'   pars = c('x', 'y'),
#'   par1_lim = c(0, 30),
#'   par2_lim = c(0, 30))
#'
#' \dontrun{
#'   ## This throws an error because payoffs1 and payoffs2 are in different forms.
#'   game4 <- normal_form(
#'     payoffs1 = fx,
#'     payoffs2 = "-y^2 + (28 - x) * y",
#'     pars = c('x', 'y'),
#'     par1_lim = c(0, 30),
#'     par2_lim = c(0, 30)
#'   )
#' }
normal_form <- function(
  players = NULL,
  s1 = NULL,
  s2 = NULL,
  payoffs1 = NULL,
  payoffs2 = NULL,
  cells = NULL,
  discretize = FALSE,
  discrete_points = c(6, 6),
  symmetric = FALSE,
  byrow = FALSE,
  pars = NULL,
  par1_lim = NULL,
  par2_lim = NULL,
  cons1 = NULL,
  cons2 = NULL,
  cons_common = NULL) {


  # If players are not named, give them default names.
  if (is.null(players)) players <- c("Player 1", "Player 2")

  # If payoffs are provided by cell, create payoffs1 and payoffs2 from cells
  if (!is.null(cells)) {
    n_cells <- length(cells)
    payoffs1 <- payoffs2 <- rep(NA, n_cells)
    for (i in 1:n_cells) {
      payoffs1[i] <- cells[[i]][1]
      payoffs2[i] <- cells[[i]][2]
    }
  }

  # error message used for a few times below
  stop_message <- "For a game with discrete strategies, please specify both s1 and s2.\nFor a game with continuous strategies, please specify all of payoffs1, payoffs2, pars, par1_lim, and par2_lim. When dicretize = TRUE, par1_lim and par2_lim are not necessary."

  if (is.null(pars)) {
    ## Game with discrete-choice strategies

    if (is.null(s1) | is.null(s2)) {
      # error if strategies are not provided for both players
      stop(stop_message)
    }

    n_rows <- length(s1)
    n_cols <- length(s2)
    n_cells <- n_rows * n_cols

    if (symmetric) payoffs2 <- payoffs1

    # error if the length of payoffs differs from the number of cells
    if (length(payoffs1) != n_cells) stop("the length of payoffs1 must equal the number of cells.")
    if (length(payoffs2) != n_cells) stop("the length of payoffs2 must equal the number of cells.")

    # Player 1's payoff matrix
    mat1 <- matrix(payoffs1, nrow = n_rows, byrow = byrow)

    # Player 2's payoff matrix
    byrow2 <- ifelse(symmetric, !byrow, byrow)
    mat2 <- matrix(payoffs2, nrow = n_rows, byrow = byrow2)

    # Gather game elements in a data frame
    if (byrow) {
      row <- rep(1:n_rows, each = n_cols)
      s1_vec <- rep(s1, each = n_cols)
      column <- rep(1:n_cols, times = n_rows)
      s2_vec <- rep(s2, times = n_rows)
    } else {
      row <- rep(1:n_rows, times = n_cols)
      s1_vec <- rep(s1, times = n_cols)
      column <- rep(1:n_cols, each = n_rows)
      s2_vec <- rep(s2, each = n_rows)
    }
    df <- data.frame(row,
                     column,
                     s1 = s1_vec,
                     s2 = s2_vec,
                     payoff1 = payoffs1,
                     payoff2 = payoffs2)

    # list for return
    value <- list(player = players,
                  strategy = list(s1 = s1, s2 = s2),
                  payoff = list(payoffs1 = payoffs1, payoffs2 = payoffs2),
                  df = df,
                  mat = list(matrix1 = mat1, matrix2 = mat2),
                  type = "matrix")

  } else if (is.character(payoffs1) & is.character(payoffs2)) {
    ## Game whose payoffs are defined by character strings describing functions

    if (is.null(par1_lim) | is.null(par2_lim)) {
      # error if domains of parameters are not provided
      stop(stop_message)
    } else if (length(par1_lim) != 2 | length(par2_lim) != 2) {
      # error if parameter domains are not specified correctly
      stop("Each of par1_lim and par2_lim must be the numeric vector of length 2.")
    } else {

      if (discretize) {

        # transform char functions to R functions
        new_fs <- char2function(f1 = payoffs1,
                                f2 = payoffs2,
                                pars = pars)

        # pick out discrete strategies
        if (is.null(s1)) {
          s1 <- seq(from = par1_lim[1],
                    to = par1_lim[2],
                    length.out = discrete_points[1])
        }
        if (is.null(s2)) {
          s2 <- seq(from = par2_lim[1],
                    to = par2_lim[2],
                    length.out = discrete_points[2])
        }

        s_set <- expand.grid(s1, s2)
        names(s_set) <- c("x", "y")
        payoff1 <- purrr::pmap(s_set, new_fs[[1]]) |> unlist()
        payoff2 <- purrr::pmap(s_set, new_fs[[2]]) |> unlist()

        s1 <- as.character(s1)
        s2 <- as.character(s2)
        n_rows <- length(s1)
        n_cols <- length(s2)
        n_cells <- n_rows * n_cols

        # payoff matrix for Players 1 and 2
        mat1 <- matrix(payoff1, nrow = n_rows, byrow = FALSE)
        mat2 <- matrix(payoff2, nrow = n_rows, byrow = FALSE)

        # gather game elements in a data frame
        row <- rep(1:n_rows, times = n_cols)
        s1_vec <- rep(s1, times = n_cols)
        column <- rep(1:n_cols, each = n_cols)
        s2_vec <- rep(s2, each = n_rows)
        df <- data.frame(row,
                         column,
                         s1,
                         s2,
                         payoff1,
                         payoff2)

        # list for return
        value <- list(player = players,
                      strategy = list(s1 = s1, s2 = s2),
                      payoff = list(payoffs1 = payoff1, payoffs2 = payoff2),
                      df = df,
                      mat = list(matrix1 = mat1, matrix2 = mat2),
                      type = "matrix")

      } else {
        value <- list(player = players,
                      strategy = list(s1 = par1_lim, s2 = par2_lim),
                      payoff = list(payoffs1 = payoffs1, payoffs2 = payoffs2),
                      pars = pars,
                      type = "char_function")
      }
    }
  } else if (is.function(payoffs1) & is.function(payoffs2)) {
    ## Game whose payoffs are defined by function objects

    if (discretize) {
      # pick out discrete strategies
      if (is.null(s1)) {
        s1 <- seq(from = par1_lim[1],
                  to = par1_lim[2],
                  length.out = discrete_points[1])
      }
      if (is.null(s2)) {
        s2 <- seq(from = par2_lim[1],
                  to = par2_lim[2],
                  length.out = discrete_points[2])
      }

      s_set <- expand.grid(s1, s2)
      names(s_set) <- pars
      payoff1 <- purrr::pmap(s_set, payoffs1) |> unlist()
      payoff2 <- purrr::pmap(s_set, payoffs2) |> unlist()

      s1 <- as.character(s1)
      s2 <- as.character(s2)
      n_rows <- length(s1)
      n_cols <- length(s2)
      n_cells <- n_rows * n_cols

      # payoff matrix for Players 1 and 2
      mat1 <- matrix(payoff1, nrow = n_rows, byrow = FALSE)
      mat2 <- matrix(payoff2, nrow = n_rows, byrow = FALSE)

      # gather game elements in a data frame
      row <- rep(1:n_rows, times = n_cols)
      s1_vec <- rep(s1, times = n_cols)
      column <- rep(1:n_cols, each = n_cols)
      s2_vec <- rep(s2, each = n_rows)
      df <- data.frame(row,
                       column,
                       s1,
                       s2,
                       payoff1,
                       payoff2)

      # list for return
      value <- list(player = players,
                    strategy = list(s1 = s1, s2 = s2),
                    payoff = list(payoffs1 = payoff1, payoffs2 = payoff2),
                    df = df,
                    mat = list(matrix1 = mat1, matrix2 = mat2),
                    type = "matrix")
    } else {
      if (is.null(par1_lim) | is.null(par2_lim)) {
        # error if domains of parameters are not provided
        stop(stop_message)
      } else if (length(par1_lim) != 2 | length(par2_lim) != 2) {
        # error if parameter domains are not specified correctly
        stop("Each of par1_lim and par2_lim must be the numeric vector of length 2.")
      } else {
        if (!is.null(cons_common)) {
          constants <- list(cons_common = cons_common)
        } else {
          constants <- list(cons1 = cons1, cons2 = cons2)
        }

        # list for return
        value <- list(player = players,
                      strategy = list(s1 = par1_lim, s2 = par2_lim),
                      payoff = list(payoffs1 = payoffs1, payoffs2 = payoffs2),
                      pars = pars,
                      constants = constants,
                      type = "function")
      }
    }
  } else {
    stop("Please specify strategies (if payoffs are not functions) and payoffs in a proper way.")
  }

  # return a "normal_form" class object
  structure(value, class = "normal_form")
}
