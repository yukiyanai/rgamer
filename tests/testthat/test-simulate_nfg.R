context("Simulate repeated plays of a normal-form game")

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

char_game <- normal_form(
  players = c("A", "B"),
  p1 = "-x^2 + (28 - y) * x",
  p2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
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


seq_game <- seq_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))


test_that("simu_game_br simulates best-responses repeatedly", {
  expect_s3_class(simu_game_br(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_br(PD, n_periods = 5)), 3)
  expect_equal(ncol(simu_game_br(PD, n_periods = 5, rho = 0.5)), 3)
  expect_equal(ncol(simu_game_br(PD, n_periods = 5, rho = 1)), 3)
  expect_equal(nrow(simu_game_br(PD, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_br(PD,
                                 n_periods = 5,
                                 rho = 0.5,
                                 init1 = "Stays silent",
                                 init2 = "Stays silent")), 3)

  expect_s3_class(simu_game_br(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_br(char_game, n_periods = 5)), 3)
  expect_equal(nrow(simu_game_br(char_game, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_br(char_game,
                                 n_periods = 5,
                                 rho = 0.5,
                                 init1 = 10,
                                 init2 = 10)), 3)

  expect_s3_class(simu_game_br(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_br(fcn_game, n_periods = 5)), 3)
  expect_equal(nrow(simu_game_br(fcn_game, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_br(fcn_game,
                                 n_periods = 5,
                                 rho = 0.5,
                                 init1 = 10,
                                 init2 = 10)), 3)
})

test_that("simu_game_sbr simulates softly best-responses repeatedly", {
  expect_s3_class(simu_game_sbr(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_sbr(PD, n_periods = 5)), 3)
  expect_equal(ncol(simu_game_sbr(PD, n_periods = 5, rho = 0.5)), 3)
  expect_equal(ncol(simu_game_sbr(PD, n_periods = 5, rho = 1)), 3)
  expect_equal(nrow(simu_game_sbr(PD, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_sbr(PD,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = "Stays silent",
                                  init2 = "Stays silent")), 3)
  expect_error(simu_game_sbr(PD, n_periods = 3, lambda = -2))

  expect_s3_class(simu_game_sbr(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_sbr(char_game, n_periods = 5)), 3)
  expect_equal(nrow(simu_game_sbr(char_game, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_sbr(char_game,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = 10,
                                  init2 = 10)), 3)

  expect_s3_class(simu_game_sbr(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_sbr(fcn_game, n_periods = 5)), 3)
  expect_equal(nrow(simu_game_sbr(fcn_game, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_sbr(fcn_game,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = 10,
                                  init2 = 10)), 3)
})

test_that("simu_game_abr simulates alternate best-responses", {
  expect_s3_class(simu_game_abr(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_abr(PD, n_periods = 5)), 4)
  expect_equal(ncol(simu_game_abr(PD, n_periods = 5, rho = 0.5)), 4)
  expect_equal(ncol(simu_game_abr(PD, n_periods = 5, rho = 1)), 4)
  expect_equal(nrow(simu_game_abr(PD, n_periods = 5)), 5 * 2)
  expect_equal(ncol(simu_game_abr(PD,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = "Stays silent",
                                  init2 = "Stays silent")), 4)

  expect_s3_class(simu_game_abr(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_abr(char_game, n_periods = 5)), 4)
  expect_equal(nrow(simu_game_abr(char_game, n_periods = 5)), 5 * 2)
  expect_equal(ncol(simu_game_abr(char_game,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = 10,
                                  init2 = 10)), 4)

  expect_s3_class(simu_game_abr(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_abr(fcn_game, n_periods = 5)), 4)
  expect_equal(nrow(simu_game_abr(fcn_game, n_periods = 5)), 5 * 2)
  expect_equal(ncol(simu_game_abr(fcn_game,
                                  n_periods = 5,
                                  rho = 0.5,
                                  init1 = 10,
                                  init2 = 10)), 4)
})

test_that("simu_game_imitation simulates imitation plays", {
  expect_s3_class(simu_game_imitation(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_imitation(PD, n_periods = 5)), 3)
  expect_equal(ncol(simu_game_imitation(PD, n_periods = 5, rho = 0.5)), 3)
  expect_equal(ncol(simu_game_imitation(PD, n_periods = 5, rho = 1)), 3)
  expect_equal(nrow(simu_game_imitation(PD, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_imitation(PD,
                                        n_periods = 5,
                                        rho = 0.5,
                                        init1 = "Stays silent",
                                        init2 = "Stays silent")), 3)

  expect_s3_class(simu_game_imitation(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(simu_game_imitation(char_game, n_periods = 5)), 3)
  expect_equal(nrow(simu_game_imitation(char_game, n_periods = 5)), 5)
  expect_equal(ncol(simu_game_imitation(char_game,
                                        n_periods = 5,
                                        rho = 0.5,
                                        init1 = 10,
                                        init2 = 10)), 3)

  expect_s3_class(simu_game_imitation(fcn_game,
                                      n_periods = 5,
                                      cons1 = fcn_game$constants[[1]],
                                      cons2 = fcn_game$constants[[2]]),
                  "data.frame")
  expect_equal(ncol(simu_game_imitation(fcn_game,
                                        n_periods = 5,
                                        cons1 = fcn_game$constants[[1]],
                                        cons2 = fcn_game$constants[[2]])), 3)
  expect_equal(nrow(simu_game_imitation(fcn_game,
                                        n_periods = 5,
                                        cons1 = fcn_game$constants[[1]],
                                        cons2 = fcn_game$constants[[2]])), 5)
  expect_equal(ncol(simu_game_imitation(fcn_game,
                                        n_periods = 5,
                                        rho = 0.5,
                                        cons1 = fcn_game$constants[[1]],
                                        cons2 = fcn_game$constants[[2]],
                                        init1 = 10,
                                        init2 = 10)), 3)
})

test_that("simu_game simulates normal-form games", {
  expect_equal(ncol(simu_game(PD,
                              n_periods = 5,
                              n_samples = 10)), 4)
  expect_equal(ncol(simu_game(PD,
                              n_periods = 5,
                              n_samples = 10,
                              type = "sbr")), 4)
  expect_equal(ncol(simu_game(PD,
                              n_periods = 5,
                              n_samples = 10,
                              type = "abr")), 5)
  expect_equal(ncol(simu_game(PD,
                              n_periods = 5,
                              n_samples = 10,
                              type = "imitation")), 4)
  expect_equal(nrow(simu_game(PD,
                              n_periods = 5,
                              n_samples = 10)), 5 * 10)

  expect_error(simu_game(seq_game))
  expect_error(simu_game(PD, rho = 1.2))
  expect_error(simu_game(PD,
                         n_periods = 5,
                         n_samples = 10,
                         type = "super"))
})
