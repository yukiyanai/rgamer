context("Dominant and dominated strategies")


PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  p1 = c(-1,  0, -3, -2),
  p2 = c(-1, -3,  0, -2))

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


test_that("find_dominant finds dominant startegies", {
  expect_type(find_dominant(PD), "list")
  expect_equal(length(find_dominant(PD)), 2)
  expect_identical(all(is.na(unlist(find_dominant(SH)))), TRUE)
  expect_message(find_dominant(PD))
  expect_error(find_dominant(char_game))
})

test_that("find_dominated finds dominted startegies", {
  expect_type(find_dominated(PD), "list")
  expect_equal(length(find_dominated(PD)), 2)
  expect_identical(all(is.na(unlist(find_dominated(SH)))), TRUE)
  expect_message(find_dominated(PD))
})

test_that("dom finds dominant or dominated strategies", {
  expect_identical(dom(PD), find_dominated(PD))
  expect_identical(dom(PD, type = "dominant"),
                   find_dominant(PD))
  expect_message(dom(PD))
  expect_error(dom(char_game))
})

test_that("eliminate_strategy eliminates a specified strategy from the game", {
  expect_s3_class(eliminate_strategy(PD,
                                     player = "Kamijo",
                                     eliminated = "Stays silent"),
                  "normal_form")
  expect_identical(eliminate_strategy(PD,
                                      player = "Kamijo",
                                      eliminated = "Stays silent"),
                   normal_form(players = c("Kamijo", "Yanai"),
                               s1 = c("Betrays"),
                               s2 = c("Stays silent", "Betrays"),
                               p1 = c(-0, -2),
                               p2 = c(-3, -2)))
})
