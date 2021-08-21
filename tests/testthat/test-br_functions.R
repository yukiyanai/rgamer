context("Find best responses")


f_x <- function(x, y) {
  -x^2 + (28 - y) * x
}
f_y <- function(x, y, s, t) {
  -y^2 + (28 - x) * y
}

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

matrix_game <- normal_form(
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

fcn_game <- normal_form(
  players = c("A", "B"),
  p1 = f_x,
  p2 = f_y,
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

seq_game <- seq_form(
  players = c("Leader", "Follower"),
  s1 = c("R", "S", "P"),
  s2 = c("R", "S", "P"),
  p1 = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
  p2 = c(0, 1, -1, -1, 0, 1, 1, -1, 0)
)

test_that("as_df_br returns a list of two data frames", {
  expect_type(as_df_br(players = c("A", "B"),
                       p1 = f_x,
                       p2 = f_y,
                       pars = c("x", "y"),
                       par1_lim = c(0, 30),
                       par2_lim = c(0, 30)),
                  "list")
 expect_length(as_df_br(players = c("A", "B"),
                        p1 = f_x,
                        p2 = f_y,
                        pars = c("x", "y"),
                        par1_lim = c(0, 30),
                        par2_lim = c(0, 30)), 2)
})

test_that("br_plot draws best response correpondence given a matrix", {
  expect_s3_class(br_plot(matrix_game),
                  "ggplot")
  expect_error(br_plot(char_game))
  expect_error(br_plot(fcn_game))
})

test_that("find_best_response returns a data.frame containing pairs of br", {
  expect_s3_class(find_best_response(PD), "data.frame")
  expect_s3_class(find_best_response(seq_game), "data.frame")
  expect_error(find_best_response(char_game))
  expect_error(find_best_response(fcn_game))
})
