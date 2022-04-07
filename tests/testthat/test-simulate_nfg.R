context("Simulate repeated plays of a normal-form game")

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  payoffs1 = c(-1,  0, -3, -2),
  payoffs2 = c(-1, -3,  0, -2))

char_game <- normal_form(
  players = c("A", "B"),
  payoffs1 = "-x^2 + (28 - y) * x",
  payoffs2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

disc_game <- normal_form(
  players = c("A", "B"),
  payoffs1 = "-x^2 + (28 - y) * x",
  payoffs2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"),
  discretize = TRUE)

f_x <- function(xA, xB, a) {
  (a - xA - xB) * xA - 3 * xA
}
f_y <- function(xA, xB, b) {
  (b - xA - xB) * xB - 3 * xB
}
fcn_game <- normal_form(
  players = c("K", "Y"),
  payoffs1 = f_x,
  payoffs2 = f_y,
  pars = c("xA", "xB"),
  cons1 = list(a = 30),
  cons2 = list(b = 30),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30))


seq_game <- seq_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  payoffs1 = c(-1,  0, -3, -2),
  payoffs2 = c(-1, -3,  0, -2))


test_that("sim_game_br simulates best-responses repeatedly", {
  expect_s3_class(sim_game_br(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_br(PD, n_periods = 5)), 3)
  expect_equal(ncol(sim_game_br(PD, n_periods = 5, omega = 0.5)), 3)
  expect_equal(ncol(sim_game_br(PD, n_periods = 5, omega = 1)), 3)
  expect_equal(nrow(sim_game_br(PD, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_br(PD,
                                n_periods = 5,
                                omega = 0.5,
                                init1 = "Stays silent",
                                init2 = "Stays silent")), 3)

  expect_s3_class(sim_game_br(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_br(char_game, n_periods = 5)), 3)
  expect_equal(nrow(sim_game_br(char_game, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_br(char_game,
                                n_periods = 5,
                                omega = 0.5,
                                init1 = 10,
                                init2 = 10)), 3)

  expect_s3_class(sim_game_br(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_br(fcn_game, n_periods = 5)), 3)
  expect_equal(nrow(sim_game_br(fcn_game, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_br(fcn_game,
                                n_periods = 5,
                                omega = 0.5,
                                init1 = 10,
                                init2 = 10)), 3)
})

test_that("sim_game_sbr simulates softly best-responses repeatedly", {
  expect_s3_class(sim_game_sbr(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_sbr(PD, n_periods = 5)), 3)
  expect_equal(ncol(sim_game_sbr(PD, n_periods = 5, omega = 0.5)), 3)
  expect_equal(ncol(sim_game_sbr(PD, n_periods = 5, omega = 1)), 3)
  expect_equal(nrow(sim_game_sbr(PD, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_sbr(PD,
                                 n_periods = 5,
                                 omega = 0.5,
                                 init1 = "Stays silent",
                                 init2 = "Stays silent")), 3)
  expect_error(sim_game_sbr(PD, n_periods = 3, lambda = -2))

  expect_s3_class(sim_game_sbr(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_sbr(char_game, n_periods = 5)), 3)
  expect_equal(nrow(sim_game_sbr(char_game, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_sbr(char_game,
                                  n_periods = 5,
                                  omega = 0.5,
                                  init1 = 10,
                                  init2 = 10)), 3)

  expect_s3_class(sim_game_sbr(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_sbr(fcn_game, n_periods = 5)), 3)
  expect_equal(nrow(sim_game_sbr(fcn_game, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_sbr(fcn_game,
                                 n_periods = 5,
                                 omega = 0.5,
                                 init1 = 10,
                                 init2 = 10)), 3)
})

test_that("sim_game_abr simulates alternate best-responses", {
  expect_s3_class(sim_game_abr(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_abr(PD, n_periods = 5)), 4)
  expect_equal(ncol(sim_game_abr(PD, n_periods = 5, omega = 0.5)), 4)
  expect_equal(ncol(sim_game_abr(PD, n_periods = 5, omega = 1)), 4)
  expect_equal(nrow(sim_game_abr(PD, n_periods = 5)), 5 * 2)
  expect_equal(ncol(sim_game_abr(PD,
                                 n_periods = 5,
                                 omega = 0.5,
                                 init1 = "Stays silent",
                                 init2 = "Stays silent")), 4)
  expect_s3_class(sim_game_abr(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_abr(char_game, n_periods = 5)), 4)
  expect_equal(nrow(sim_game_abr(char_game, n_periods = 5)), 5 * 2)
  expect_equal(ncol(sim_game_abr(char_game,
                                 n_periods = 5,
                                 omega = 0.5,
                                 init1 = 10,
                                 init2 = 10)), 4)

  expect_s3_class(sim_game_abr(fcn_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_abr(fcn_game, n_periods = 5)), 4)
  expect_equal(nrow(sim_game_abr(fcn_game, n_periods = 5)), 5 * 2)
  expect_equal(ncol(sim_game_abr(fcn_game,
                                 n_periods = 5,
                                 omega = 0.5,
                                 init1 = 10,
                                 init2 = 10)), 4)
})

test_that("sim_game_imitation simulates imitation plays", {
  expect_s3_class(sim_game_imitation(PD, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_imitation(PD, n_periods = 5)), 3)
  expect_equal(ncol(sim_game_imitation(PD, n_periods = 5, omega = 0.5)), 3)
  expect_equal(ncol(sim_game_imitation(PD, n_periods = 5, omega = 1)), 3)
  expect_equal(nrow(sim_game_imitation(PD, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_imitation(PD,
                                       n_periods = 5,
                                       omega = 0.5,
                                       init1 = "Stays silent",
                                       init2 = "Stays silent")), 3)

  expect_s3_class(sim_game_imitation(char_game, n_periods = 5), "data.frame")
  expect_equal(ncol(sim_game_imitation(char_game, n_periods = 5)), 3)
  expect_equal(nrow(sim_game_imitation(char_game, n_periods = 5)), 5)
  expect_equal(ncol(sim_game_imitation(char_game,
                                       n_periods = 5,
                                       omega = 0.5,
                                       init1 = 10,
                                       init2 = 10)), 3)

  expect_s3_class(sim_game_imitation(fcn_game,
                                     n_periods = 5,
                                     cons1 = fcn_game$constants[[1]],
                                     cons2 = fcn_game$constants[[2]]),
                  "data.frame")
  expect_equal(ncol(sim_game_imitation(fcn_game,
                                       n_periods = 5,
                                       cons1 = fcn_game$constants[[1]],
                                       cons2 = fcn_game$constants[[2]])), 3)
  expect_equal(nrow(sim_game_imitation(fcn_game,
                                       n_periods = 5,
                                       cons1 = fcn_game$constants[[1]],
                                       cons2 = fcn_game$constants[[2]])), 5)
  expect_equal(ncol(sim_game_imitation(fcn_game,
                                       n_periods = 5,
                                       omega = 0.5,
                                       cons1 = fcn_game$constants[[1]],
                                       cons2 = fcn_game$constants[[2]],
                                       init1 = 10,
                                       init2 = 10)), 3)
})

test_that("sim_game simulates normal-form games", {
  expect_length(sim_game(PD,
                         n_periods = 5,
                         n_samples = 10), 3)
  expect_length(sim_game(PD,
                         n_periods = 5,
                         n_samples = 10,
                         type = "sbr"), 3)
  expect_length(sim_game(PD,
                         n_periods = 5,
                         n_samples = 10,
                         type = "abr"), 3)
  expect_length(sim_game(PD,
                         n_periods = 5,
                         n_samples = 10,
                         type = "imitation"), 3)

  expect_error(sim_game(seq_game))
  expect_error(sim_game(PD, omega = 1.2))
  expect_error(sim_game(PD,
                        n_periods = 5,
                        n_samples = 10,
                        type = "super"))
})


test_that("sim_lerning simulates learning proces", {
  expect_length(sim_learning(disc_game,
                             n_samples = 2,
                             n_periods = 10), 5)
  expect_length(sim_learning(disc_game,
                             type = "reinforcement",
                             n_samples = 2,
                             n_periods = 10), 5)
  expect_length(sim_learning(disc_game,
                             type = "belief",
                             n_samples = 2,
                             n_periods = 10), 5)
  expect_error(sim_learning(disc_game,
                            type = "super",
                            n_samples = 2,
                            n_periods = 10))
  expect_error(sim_learning(disc_game,
                            lambda = -1,
                            n_samples = 2,
                            n_periods = 10))
  expect_error(sim_learning(disc_game,
                            delta = 2,
                            n_samples = 2,
                            n_periods = 10))
  expect_error(sim_learning(disc_game,
                            rho = 3.2,
                            n_samples = 2,
                            n_periods = 10))
  expect_error(sim_learning(disc_game,
                            phi = 1.1,
                            n_samples = 2,
                            n_periods = 10))
  expect_error(sim_learning(disc_game,
                            N_init = -1,
                            n_samples = 2,
                            n_periods = 10))
})

test_that("sim_fict simulates fictitious plays", {
  expect_length(sim_fict(disc_game,
                         n_samples = 2,
                         n_periods = 10), 7)
  expect_length(sim_fict(disc_game,
                         init = list(rep(1, 6) / 6,
                                     rep(1, 6) / 6),
                         n_samples = 2,
                         n_periods = 10), 7)
  expect_error(sim_fict(disc_game,
                        lambda = -1,
                        n_samples = 1,
                        n_periods = 10))
  expect_error(sim_fict(disc_game,
                        sigma = -2,
                        n_samples = 2,
                        n_periods = 10))
})
