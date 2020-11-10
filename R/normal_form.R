#' @title Define a normal-form (or strategic-form) game
#' @description \code{normal_form()} defines a normal-form game and
#'     creates an object of "normal_form" class.
#' @details Create an object of 'normal_form' class, which can be passed to functions
#'     in order to find solutions of the game.
#' @param players A character vector of the name (label) for the players.
#' @param s1 A character vector of pure strategies for Player 1 (row player). Required only when
#'     the player has discrete-choice strategies.
#' @param s2 A character vector of pure strategies for Player 2 (column player). Required only when
#'     the player has discrete-choice strategies.
#' @param p1 The payoff of Player1. This argument can be specified in three different ways.
#'     First, it can be a numeric vector of payoffs. Second, it can be a character string of the payoff
#'     function (e.g., p1 = "x^2 - y"). Third, it can be an R function of payoff.
#' @param p2 The payoff of Player 2. See the explanation of \code{p1} for detail.
#' @param discretize A logical value. Set this \code{TRUE} to evaluate payoff functions
#'      at some discrete values of strategies \code{s1} and \code{s2}. Default is \code{FALSE}
#' @param symmetric A logical value. Set this \code{TRUE} when the payoffs for two players are
#'     symmetric as in the prisoners' dilemma. Then, p1 is recycled for p2. Default is \code{FALSE}
#' @param byrow A logical value. If \code{TRUE}, payoffs will be lined up by row. Default is \code{FALSE}.
#'     Only used when both \code{s1} and \code{s2} are provided.
#' @param pars A character vector of parameters that are selected by players 1 and 2, respectively.
#'     Only used when \code{p1} and \code{p2} are specified as a function
#'     (either as a character string or as an R function).
#' @param par1_lim A numeric vector of length 2, which defines the range of parameters
#'     from which Player 1 chooses her strategy.
#' @param par2_lim A numeric vector of length 2, which defines the range of parameters
#'     from which Player 2 chooses his strategy.
#' @param cons1 A named list of parameters contained in \code{p1} that should be treated as constants, if any.
#' @param cons2 A named list of parameters contained in \code{p2} that should be treated as constants, if any.
#' @param cons_common A named list of parameters contained in \code{p1} and \code{p2} that should be treated
#'     as constants, if any. If \code{cons1} and \code{cons2} are exactly same, you can specify \code{cons_common}
#'      instead of both \code{cons1} and \code{cons2}.
#' @return An object of "normal_form" class, which defines a normal-form (or strategic-form) game.
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
#' @examples
#' game1 <- normal_form(
#'   s1 = c("T", "B"), s2 = c("L", "R"),
#'   p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1),
#'   players = c("Row Player", "Column Player"))
#'
#' game2 <- normal_form(
#'    players = c("A", "B"),
#'    p1 = "-x1^2 + (28 - x2) * x1",
#'    p2 = "-x2^2 + (28 - x1) * x2",
#'    par1_lim = c(0, 30),
#'    par2_lim = c(0, 30),
#'    pars = c("x1", "x2"))
#'
#' fx <- function(x, y, a, b) -x^a + (b - y) * x
#' fy <- function(x, y, s, t) -y^s + (t - x) * y
#' game3 <- normal_form(
#'   p1 = fx,
#'   p2 = fy,
#'   pars = c('x', 'y'),
#'   par1_lim = c(0, 30),
#'   par2_lim = c(0, 30))
#'
#' \dontrun{
#' ## This causes an error because p1 and p2 are in different forms.
#' game4 <- normal_form(
#'   p1 = fx,
#'   p2 = "-y^2 + (28 - x) * y",
#'   pars = c('x', 'y'),
#'   par1_lim = c(0, 30),
#'   par2_lim = c(0, 30)
#' )
#' }
normal_form <- function(
  players = NULL,
  s1 = NULL,
  s2 = NULL,
  p1,
  p2,
  discretize = FALSE,
  symmetric = FALSE,
  byrow = FALSE,
  pars = NULL,
  par1_lim = NULL,
  par2_lim = NULL,
  cons1 = NULL,
  cons2 = NULL,
  cons_common = NULL) {

  stop_message <- "For a game with discrete strategies, please specify  both s1 and s2.\nFor a game with continuous strategies, please specify all of p1, p2, pars, par1_lim, and par2_lim. When dicretize = TRUE, par1_lim and par2_lim are not necessary."

  if (is.null(players)) players <- c("Player 1", "Player 2")

  if (is.null(pars)) {
    ## game with discrete-choice strategies
    if (is.null(s1) | is.null(s2)) {
      stop(stop_message)
    }

    n_rows <- length(s1)
    n_cols <- length(s2)
    n_cells <- n_rows * n_cols

    if (symmetric) p2 <- p1

    if (length(p1) != n_cells) stop("the length of p1 must equal the number of cells.")
    if (length(p2) != n_cells) stop("the length of p2 must equal the number of cells.")

    mat1 <- matrix(p1, nrow = n_rows, byrow = byrow)
    byrow2 <- ifelse(symmetric, !byrow, byrow)
    mat2 <- matrix(p2, nrow = n_rows, byrow = byrow2)

    if (byrow) {
      row <- rep(1:n_rows, each = n_cols)
      s1_vec <- rep(s1, each = n_cols)
      column <- rep(1:n_cols, times = n_rows)
      s2_vec <- rep(s2, times = n_rows)
    } else {
      row <- rep(1:n_rows, times = n_cols)
      s1_vec <- rep(s1, times = n_cols)
      column <- rep(1:n_cols, each = n_cols)
      s2_vec <- rep(s2, each = n_rows)
    }

    df <- data.frame(row, column, s1 = s1_vec, s2 = s2_vec, p1, p2)

    value <- list(player = players,
                  strategy = list(s1 = s1, s2 = s2),
                  payoff = list(p1 = p1, p2 = p2),
                  df = df,
                  mat = list(matrix1= mat1, matrix2 = mat2),
                  type = "matrix")

  } else if (is.character(p1) & is.character(p2)) {
    ## game whose payoffs are defined by character strings of functions
    if (is.null(par1_lim) | is.null(par2_lim)) {
      stop(stop_message)
    } else if (length(par1_lim) != 2 | length(par2_lim) != 2) {
      stop("Each of par1_lim and par2_lim must be the numeric vector of length 2.")
    } else {
      value <- list(player = players,
                    strategy = list(s1 = par1_lim, s2 = par2_lim),
                    payoff = list(p1 = p1, p2 = p2),
                    pars = pars,
                    type = "char_function")
    }
  } else if (is.function(p1) & is.function(p2)) {
    ## game whose payoffs are defined by function objects
    if (discretize) {
      s_set <- expand.grid(s1, s2)
      names(s_set) <- pars
      payoff1 <- purrr::pmap(s_set, p1) %>% unlist()
      payoff2 <- purrr::pmap(s_set, p2) %>% unlist()

      s1 <- as.character(s1)
      s2 <- as.character(s2)
      n_rows <- length(s1)
      n_cols <- length(s2)
      n_cells <- n_rows * n_cols

      mat1 <- matrix(payoff1, nrow = n_rows, byrow = FALSE)
      mat2 <- matrix(payoff2, nrow = n_rows, byrow = FALSE)

      row <- rep(1:n_rows, times = n_cols)
      s1_vec <- rep(s1, times = n_cols)
      column <- rep(1:n_cols, each = n_cols)
      s2_vec <- rep(s2, each = n_rows)

      df <- data.frame(row, column, s1, s2,
                       p1 = payoff1, p2 = payoff2)

      value <- list(player = players,
                    strategy = list(s1 = s1, s2 = s2),
                    payoff = list(p1 = payoff1, p2 = payoff2),
                    df = df,
                    mat = list(matrix1 = mat1, matrix2 = mat2),
                    type = "matrix")
    } else {
      if (is.null(par1_lim) | is.null(par2_lim)) {
        stop(stop_message)
      } else if (length(par1_lim) != 2 | length(par2_lim) != 2) {
        stop("Each of par1_lim and par2_lim must be the numeric vector of length 2.")
      } else {
        if (!is.null(cons_common)) {
          constants <- list(cons_common = cons_common)
        } else {
          constants <- list(cons1 = cons1, cons2 = cons2)
        }
        value <- list(player = players,
                      strategy = list(s1 = par1_lim, s2 = par2_lim),
                      payoff = list(p1 = p1, p2 = p2),
                      pars = pars,
                      constants = constants,
                      type = "function")
      }
    }
  } else {
    stop("Please specify strategies (if payoffs are not functions) and payoffs in a proper way.")
  }

  structure(value, class = "normal_form")
}
