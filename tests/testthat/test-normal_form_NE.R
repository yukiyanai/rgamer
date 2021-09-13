context("Finds NEs of normal form games")

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

RPS <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("R", "P", "S"),
  s2 = c("R", "P", "S"),
  p1 = c(0, 1, -1, -1, 0, 1, 1, -1, 0),
  symmetric = TRUE)

SH <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stag", "Hare"),
  s2 = c("Stag", "Hare"),
  p1 = c(10, 8, 0, 7),
  p2 = c(10, 0, 8, 7))

char_game <- normal_form(
  players = c("A", "B"),
  p1 = "-x^2 + (28 - y) * x",
  p2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

char_game2 <- normal_form(
  players = c("A", "B"),
  p1 = "-x^2 + (28 - y) * x",
  p2 = "-y^2 + (28 - x) * y",
  par1_lim = c(3, 4),
  par2_lim = c(3, 4),
  pars = c("x", "y"))

f_x <- function(xA, xB, a) {
  (a - xA - xB) * xA - 3 * xA
}
f_y <- function(xA, xB, b) {
  (b - xA - xB) * xB - 3 * xB
}
fcn_game <- normal_form(
  players = c("K", "Y"),
  p1 = f_x,
  p2 = f_y,
  pars = c("xA", "xB"),
  cons1 = list(a = 30),
  cons2 = list(b = 30),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30))

matrix_game <- seq_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))


test_that("find_pure_NE finds pure-st. NE if any", {
  expect_type(find_pure_NE(PD), "character")
  expect_length(find_pure_NE(PD), 1)
  expect_type(find_pure_NE(SH), "character")
  expect_length(find_pure_NE(SH), 2)
  expect_null(find_pure_NE(RPS))
  expect_error(find_pure_NE(char_game))
})

test_that("find_mixed_NE finds mixed-st. NE if any", {
  expect_null(find_mixed_NE(PD)$mixed_NE)
  expect_length(find_mixed_NE(PD), 2)
  expect_type(find_mixed_NE(SH), "list")
  expect_length(find_mixed_NE(SH), 2)
  expect_type(find_mixed_NE(SH)[[1]], "list")
  expect_type(find_mixed_NE(SH)[[2]], "list")
  expect_type(find_mixed_NE(SH)[[1]][[1]], "double")
  expect_error(find_mixed_NE(char_game))
})

test_that("solve_nfg_matrix finds NEs of matrix-type games", {
  expect_type(solve_nfg_matrix(PD), "list")
  expect_null(solve_nfg_matrix(RPS)$psNE)
  expect_message(solve_nfg_matrix(PD))
  expect_message(solve_nfg_matrix(PD, mixed = TRUE))
  expect_message(solve_nfg_matrix(RPS, mixed = TRUE))
  expect_length(solve_nfg_matrix(PD), 5)
  expect_length(solve_nfg_matrix(SH), 5)
  expect_length(solve_nfg_matrix(RPS, mixed = TRUE, quietly = TRUE), 5)
  expect_error(solve_nfg_matrix(char_game))
  expect_error(solve_nfg_matrix(fcn_game))
})

test_that("solve_nfg_char finds NEs of 'char_function'-type games", {
  expect_type(solve_nfg_char(char_game), "list")
  expect_length(solve_nfg_char(char_game), 3)
  expect_message(solve_nfg_char(char_game))
  expect_message(solve_nfg_char(char_game, mark_NE = TRUE))
  expect_message(solve_nfg_char(char_game2))
  expect_error(solve_nfg_char(PD))
  expect_error(solve_nfg_char(fcn_game))
})

test_that("solve_nfg_fcn finds NEs of 'function'-type games", {
  expect_type(solve_nfg_fcn(fcn_game), "list")
  expect_length(solve_nfg_fcn(fcn_game), 3)
  expect_message(solve_nfg_fcn(fcn_game))
  expect_error(solve_nfg_fcn(PD))
  expect_error(solve_nfg_fcn(char_game))
})

test_that("solve_nfg finds equilibria of a normal-form game", {
  expect_error(solve_nfg(matrix_game))
  expect_type(solve_nfg(PD), "list")
  expect_message(solve_nfg(PD))
  expect_message(solve_nfg(char_game))
  expect_message(solve_nfg(fcn_game))
  expect_equal(solve_nfg(PD), solve_nfg_matrix(PD))
  expect_equal(solve_nfg(SH), solve_nfg_matrix(SH))
  expect_equal(solve_nfg(char_game), solve_nfg_char(char_game))
  expect_equal(solve_nfg(fcn_game), solve_nfg_fcn(fcn_game))
})
