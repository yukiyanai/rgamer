context("Finds SPE outcomes by backward induction")

matrix_game <- seq_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

char_game <- seq_form(
  players = c("A", "B"),
  p1 = "-x^2 - x * y + 28 * x",
  p2 = "-y^2 - x * y + 28 * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

f_x <- function(x, y) {
  -x^2 + (28 - y) * x
}
f_y <- function(x, y) {
  -y^2 + (28 - x) * y
}
f_x2 <- function(x, y, a, s) {
  -x^s + (a - y) * x
}
f_y2 <- function(x, y, a, s) {
  -y^s + (a - x) * y
}

fcn_game <- seq_form(
  players = c("A", "B"),
  p1 = f_x,
  p2 = f_y,
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

fcn_game2 <- seq_form(
  players = c("A", "B"),
  p1 = f_x2,
  p2 = f_y2,
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"),
  cons_common = list(a = 28, s = 2))

fcn_game2x <- seq_form(
  players = c("A", "B"),
  p1 = f_x2,
  p2 = f_y,
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"),
  cons1 = list(a = 28, s = 2))

fcn_game2y <- seq_form(
  players = c("A", "B"),
  p1 = f_x,
  p2 = f_y2,
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"),
  cons2 = list(a = 28, s = 2))

RPS <- seq_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("R", "P", "S"),
  s2 = c("R", "P", "S"),
  p1 = c(0,  1,  1, -1, 0,  1,  1, -1, 0),
  p2 = c(0, -1, -1,  1, 0, -1, -1,  1, 0))

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

test_that("solve_seq_matrix finds SPE outcomes of matrix-type games", {
  expect_type(solve_seq_matrix(matrix_game), "list")
  expect_type(solve_seq_matrix(RPS), "list")
  expect_message(solve_seq_matrix(matrix_game))
  expect_message(solve_seq_matrix(RPS))
  expect_length(solve_seq_matrix(matrix_game), 2)
  expect_length(solve_seq_matrix(RPS), 2)
  expect_error(solve_nfg_matrix(char_game))
  expect_error(solve_nfg_matrix(fcn_game))
})


test_that("solve_seq_char finds SPE outcomes of 'char_function'-type games", {
  expect_type(solve_seq_char(char_game), "list")
  expect_length(solve_seq_char(char_game), 2)
  expect_message(solve_seq_char(char_game))
  expect_error(solve_seq_char(matrix_game))
  expect_error(solve_seq_char(fcn_game))
})

test_that("solve_seq_fcn finds SPE oucomes of 'function'-type games", {
  expect_type(solve_seq_fcn(fcn_game), "list")
  expect_type(solve_seq_fcn(fcn_game2), "list")
  expect_length(solve_seq_fcn(fcn_game), 2)
  expect_length(solve_seq_fcn(fcn_game2x), 2)
  expect_message(solve_seq_fcn(fcn_game))
  expect_message(solve_seq_fcn(fcn_game2y))
  expect_error(solve_seq_fcn(matrix_game))
  expect_error(solve_seq_fcn(char_game))
})

test_that("solve_seq finds SPE outcomes a sequential-form game", {
  expect_type(solve_seq(matrix_game), "list")
  expect_type(solve_seq(char_game), "list")
  expect_type(solve_seq(fcn_game), "list")
  expect_type(solve_seq(fcn_game2), "list")
  expect_message(solve_seq(matrix_game))
  expect_message(solve_seq(char_game))
  expect_message(solve_seq(fcn_game))
  expect_equal(solve_seq(matrix_game), solve_seq_matrix(matrix_game))
  expect_equal(solve_seq(char_game), solve_seq_char(char_game))
  expect_equal(solve_seq(fcn_game), solve_seq_fcn(fcn_game))
  expect_error(solve_seq(PD))
})
