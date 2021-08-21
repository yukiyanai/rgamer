context("Game Definitions")

test_that("normal_form constructs a normal_form class", {
  expect_s3_class(normal_form(s1 = c("T", "B"), s2 = c("L", "R"),
                              p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1),
                              players = c("Kamijo", "Yanai")),
                  "normal_form")
  expect_s3_class(normal_form(players = c("A", "B"),
                              p1 = "-x1^2 + (28 - x2) * x1",
                              p2 = "-x2^2 + (28 - x1) * x2",
                              par1_lim = c(0, 30),
                              par2_lim = c(0, 30),
                              pars = c("x1", "x2")),
                  "normal_form")
  expect_s3_class(normal_form(players = c("A", "B"),
                              p1 = function(x, y){-x^2 + (28-y) * x},
                              p2 = function(x, y){-y^2 + (28-x) * y},
                              pars = c("x", "y"),
                              par1_lim = c(0, 30),
                              par2_lim = c(0, 30)),
                  "normal_form")
  expect_error(normal_form(s1 = c("T", "B"), s2 = c("L", "R"),
                           p1 = c(4, 2, 3), p2 = c(4, 3, 2),
                           players = c("Kamijo", "Yanai")))
})

test_that("seq_form constructs a sequential_form class", {
  expect_s3_class(seq_form(s1 = c("T", "B"), s2 = c("L", "R"),
                           p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1),
                           players = c("Kamijo", "Yanai")),
                  "sequential_form")
  expect_s3_class(seq_form(players = c("A", "B"),
                           p1 = "-x1^2 + (28 - x2) * x1",
                           p2 = "-x2^2 + (28 - x1) * x2",
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30),
                           pars = c("x1", "x2")),
                  "sequential_form")
  expect_s3_class(seq_form(players = c("A", "B"),
                           p1 = function(x, y){-x^2 + (28-y) * x},
                           p2 = function(x, y){-y^2 + (28-x) * y},
                           pars = c("x", "y"),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)),
                  "sequential_form")
  expect_error(seq_form(s1 = c("T", "B"), s2 = c("L", "R"),
                        p1 = c(4, 2, 3), p2 = c(4, 3, 2),
                        players = c("Kamijo", "Yanai")))
})

test_that("extensive form constructs an extensive_form class", {
  expect_s3_class(extensive_form(players = list("Kamijo",
                                 rep("Yanai", 2),
                                 rep(NA, 4)),
                                 actions <- list(c("U", "D"),
                                                 c("U'", "D'"),
                                                 c("U''", "D''")),
                                 payoffs = list(Kamijo = c(0, 2, 1, 3),
                                                 Yanai  = c(0, 1, 2, 1))),
                  "extensive_form")
  expect_s3_class(extensive_form(players = list("Kamijo",
                                                c("Yanai", NA),
                                                c("Kamijo", NA),
                                                c(NA, NA)),
                                 direction = "horizontal",
                                 actions = list(c("P", "T"),
                                                c("P'", "T'"),
                                                c("P''", "T''")),
                                 payoffs = list(Kamijo = c(0, 1, 5, 3),
                                                Yanai = c(0, 2, 4, 1))),
                  "extensive_form")
  expect_s3_class(extensive_form(players = list("p1",
                                                rep("p2", 3),
                                                rep(NA, 6)),
                                 action = list(c("C", "D", "E"),
                                               c("F", "G"),
                                               c("H", "I"),
                                               c("J", "K")),
                                 payoff = list(p1 = c(3, 1, 1, 2, 2, 1),
                                               p2 = c(0, 0, 1, 1, 2, 3))),
                  "extensive_form")
  expect_error(extensive_form(players = list("p1",
                                             rep("p2", 3)),
                              action = list(c("C", "D", "E"),
                                            c("F", "G"),
                                            c("H", "I"),
                                            c("J", "K")),
                              payoff = list(p1 = c(3, 1, 1, 2, 2, 1),
                                            p2 = c(0, 0, 1, 1, 2, 3))))
})


test_that("normal_form expects payoffs of two players are in the same form", {
  expect_error(normal_form(players = c("A", "B"),
                           p1 = function(x, y){-x^2 + (28-y) * x},
                           p2 =  "-y^2 + (28 - x) * y",
                           pars = c("x", "y"),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)))
})

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

seq_game <- seq_form(
  players = c("Leader", "Follower"),
  s1 = c("R", "S", "P"),
  s2 = c("R", "S", "P"),
  p1 = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
  p2 = c(0, 1, -1, -1, 0, 1, 1, -1, 0)
)

test_that("game_table makes a payoff matrix of normal-form games", {
  expect_s3_class(game_table(PD), "gt_tbl")
  expect_s3_class(game_table(SH), "gt_tbl")
  expect_error(game_table(char_game))
  expect_s3_class(game_table(seq_game), "gt_tbl")
})

test_that("set_nodes determines the node positions of a game tree", {
  expect_s3_class(set_nodes(players = c("P1", "P2", "P1"),
                            n_node = c(1, 2, 2, 2),
                            n_choice = list(2,
                                            c(2, 0),
                                            c(2, 0),
                                            c(0, 0))),
                  "data.frame")
  expect_equal(ncol(set_nodes(players = c("P1", "P2", "P1"),
                              n_node = c(1, 2, 2, 2),
                              n_choice = list(2,
                                              c(2, 0),
                                              c(2, 0),
                                              c(0, 0)))),
               5)
})
