context("Game Definitions")

func_price1 <- function(p, q) {
  if (p < q) {
    profit <- p
  } else if (p == q) {
    profit <- 0.5 * p
  } else {
    profit <- 0
  }
  profit
}

func_price2 <- function(p, q){
  if (p > q) {
    profit <- q
  } else if (p == q) {
    profit <- 0.5 * q
  } else {
    profit <- 0
  }
  profit
}

test_that("normal_form constructs a normal_form class", {
  expect_s3_class(normal_form(s1 = c("T", "B"),
                              s2 = c("L", "R"),
                              payoffs1 = c(4, 2, 3, 1),
                              payoffs2 = c(4, 3, 2, 1),
                              players = c("Kamijo", "Yanai")),
                  "normal_form")
  expect_s3_class(normal_form(s1 = c("T", "B"),
                              s2 = c("L", "R"),
                              payoffs1 = c(4, 3, 2, 1),
                              byrow = TRUE,
                              symmetric = TRUE,
                              players = c("Kamijo", "Yanai")),
                  "normal_form")
  expect_s3_class(normal_form(players = c("A", "B"),
                              payoffs1 = "-x1^2 + (28 - x2) * x1",
                              payoffs2 = "-x2^2 + (28 - x1) * x2",
                              par1_lim = c(0, 30),
                              par2_lim = c(0, 30),
                              pars = c("x1", "x2")),
                  "normal_form")
  expect_s3_class(normal_form(players = c("A", "B"),
                              payoffs1 = function(x, y){-x^2 + (28-y) * x},
                              payoffs2 = function(x, y){-y^2 + (28-x) * y},
                              pars = c("x", "y"),
                              par1_lim = c(0, 30),
                              par2_lim = c(0, 30)),
                  "normal_form")
  expect_s3_class(normal_form(payoffs1 = func_price1,
                              payoffs2 = func_price2,
                              pars = c("p", "q"),
                              par1_lim = c(0, 10),
                              par2_lim = c(0, 10),
                              discretize = TRUE),
                  "normal_form")
  expect_s3_class(normal_form(players = c("A", "B"),
                              payoffs1 = function(x, y, s){-x^s + (28-y) * x},
                              payoffs2 = function(x, y, s){-y^s + (28-x) * y},
                              pars = c("x", "y"),
                              par1_lim = c(0, 30),
                              par2_lim = c(0, 30),
                              cons_common = list(s = 2)),
                  "normal_form")
  expect_error(normal_form(s1 = c("T", "B"),
                           s2 = c("L", "R"),
                           payoffs1 = c(4, 2, 3),
                           payoffs2 = c(4, 3, 2, 1),
                           players = c("Kamijo", "Yanai")))
  expect_error(normal_form(s1 = c("T", "B"),
                           s2 = c("L", "R"),
                           payoffs1 = c(4, 2, 3, 1),
                           payoffs2 = c(4, 3, 2),
                           players = c("Kamijo", "Yanai")))
  expect_error(normal_form(s1 = NULL,
                           s2 = c("L", "R"),
                           payoffs1 = c(4, 2, 3, 1),
                           payoffs2 = c(4, 3, 2, 1),
                           players = c("Kamijo", "Yanai")))
  expect_error(normal_form(payoffs1 = func_price1,
                           payoffs2 = "-y^2 + (28 - x) * y",
                           pars = c('x', 'y'),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)))
  expect_error(normal_form(players = c("A", "B"),
                           payoffs1 = "-x1^2 + (28 - x2) * x1",
                           payoffs2 = "-x2^2 + (28 - x1) * x2",
                           par1_lim = NULL,
                           par2_lim = c(0, 30),
                           pars = c("x1", "x2")))
  expect_error(normal_form(players = c("A", "B"),
                           payoffs1 = "-x1^2 + (28 - x2) * x1",
                           payoffs2 = "-x2^2 + (28 - x1) * x2",
                           par1_lim = c(0, 15, 30),
                           par2_lim = c(0, 30),
                           pars = c("x1", "x2")))
  expect_error(normal_form(players = c("A", "B"),
                           payoffs1 = function(x, y){-x^2 + (28-y) * x},
                           payoffs2 = function(x, y){-y^2 + (28-x) * y},
                           pars = c("x", "y"),
                           par1_lim = NULL,
                           par2_lim = c(0, 30)))
  expect_error(normal_form(players = c("A", "B"),
                           payoffs1 = function(x, y){-x^2 + (28-y) * x},
                           payoffs2 = function(x, y){-y^2 + (28-x) * y},
                           pars = c("x", "y"),
                           par1_lim = c(0, 15, 30),
                           par2_lim = c(0, 30)))
})

test_that("seq_form constructs a sequential_form class", {
  expect_s3_class(seq_form(s1 = c("T", "B"),
                           s2 = c("L", "R"),
                           payoffs1 = c(4, 2, 3, 1),
                           payoffs2 = c(4, 3, 2, 1),
                           players = c("Kamijo", "Yanai")),
                  "sequential_form")
  expect_s3_class(seq_form(s1 = c("T", "B"),
                           s2 = c("L", "R"),
                           payoffs1 = c(4, 2, 2, 1),
                           symmetric = TRUE,
                           byrow = TRUE,
                           players = c("Kamijo", "Yanai")),
                  "sequential_form")
  expect_s3_class(seq_form(players = c("A", "B"),
                           payoffs1 = "-x1^2 + (28 - x2) * x1",
                           payoffs2 = "-x2^2 + (28 - x1) * x2",
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30),
                           pars = c("x1", "x2")),
                  "sequential_form")
  expect_s3_class(seq_form(players = c("A", "B"),
                           payoffs1 = function(x, y){-x^2 + (28-y) * x},
                           payoffs2 = function(x, y){-y^2 + (28-x) * y},
                           pars = c("x", "y"),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)),
                  "sequential_form")
  expect_s3_class(seq_form(payoffs1 = func_price1,
                           payoffs2 = func_price2,
                           pars = c("p", "q"),
                           par1_lim = c(0, 10),
                           par2_lim = c(0, 10),
                           discretize = TRUE),
                  "sequential_form")
  expect_error(seq_form(s1 = c("T", "B"),
                        s2 = c("L", "R"),
                        payoffs1 = c(4, 2, 3, 2),
                        payoffs2 = c(4, 3, 2),
                        players = c("Kamijo", "Yanai")))
  expect_error(seq_form(s1 = c("T", "B"),
                        s2 = c("L", "R"),
                        payoffs1 = c(4, 2, 3),
                        payoffs2 = c(4, 3, 2, 1),
                        players = c("Kamijo", "Yanai")))
  expect_error(seq_form(payoffs1 = c(4, 2, 3),
                        payoffs2 = c(4, 3, 2),
                        players = c("Kamijo", "Yanai")))
  expect_error(seq_form(players = c("A", "B"),
                        payoffs1 = "-x1^2 + (28 - x2) * x1",
                        payoffs2 = "-x2^2 + (28 - x1) * x2",
                        pars = c("x1", "x2")))
  expect_error(seq_form(players = c("A", "B"),
                        payoffs1 = "-x1^2 + (28 - x2) * x1",
                        payoffs2 = "-x2^2 + (28 - x1) * x2",
                        par1_lim = c(1, 2, 3),
                        par2_lim = c(0, 10),
                        pars = c("x1", "x2")))
  expect_error(seq_form(players = c("A", "B"),
                        payoffs1 = function(x, y){-x^2 + (28-y) * x},
                        payoffs2 = function(x, y){-y^2 + (28-x) * y},
                        pars = c("x", "y"),
                        par2_lim = c(0, 30)))
  expect_error(seq_form(players = c("A", "B"),
                        payoffs1 = function(x, y){-x^2 + (28-y) * x},
                        payoffs2 = function(x, y){-y^2 + (28-x) * y},
                        pars = c("x", "y"),
                        par1_lim = c(0, 5, 10),
                        par2_lim = c(0, 30)))
  expect_error(seq_form(players = c("A", "B"),
                        payoffs1 = 1:4,
                        payoffs2 = 1:4,
                        pars = c("x", "y"),
                        par2_lim = c(0, 30)))
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
                                                rep("Yanai", 2),
                                                rep(NA, 4)),
                                 actions <- list(c("U", "D"),
                                                 c("U'", "D'"),
                                                 c("U''", "D''")),
                                 payoffs = list(Kamijo = c(0, 2, 1, 3),
                                                 Yanai  = c(0, 1, 2, 1)),
                                 scale = 0.5),
                  "extensive_form")
  expect_error(extensive_form(players = list("Kamijo",
                                             rep("Yanai", 2),
                                             rep(NA, 4)),
                              actions <- list(c("U", "D"),
                                              c("U'", "D'"),
                                              c("U''", "D''")),
                              payoffs = list(Kamijo = c(0, 2, 1, 3),
                                              Yanai  = c(0, 1, 2, 1)),
                              scale = -2))
  expect_s3_class(extensive_form(players = list("Kamijo",
                                                rep("Yanai", 2),
                                                rep(NA, 4)),
                                 actions <- list(c("U", "D"),
                                                 c("U'", "D'"),
                                                 c("U''", "D''")),
                                 payoffs = list(Kamijo = c(0, 2, 1, 3),
                                                Yanai  = c(0, 1, 2, 1)),
                                 direction = "right"),
                  "extensive_form")
  expect_s3_class(extensive_form(players = list("Kamijo",
                                                rep("Yanai", 2),
                                                rep(NA, 4)),
                                 actions <- list(c("U", "D"),
                                                 c("U'", "D'"),
                                                 c("U''", "D''")),
                                 payoffs = list(Kamijo = c(0, 2, 1, 3),
                                                Yanai  = c(0, 1, 2, 1)),
                                 direction = "up"),
                  "extensive_form")
  expect_s3_class(extensive_form(players = list("Kamijo",
                                                rep("Yanai", 2),
                                                rep(NA, 4)),
                                 actions <- list(c("U", "D"),
                                                 c("U'", "D'"),
                                                 c("U''", "D''")),
                                 payoffs = list(Kamijo = c(0, 2, 1, 3),
                                                Yanai  = c(0, 1, 2, 1)),
                                 direction = "bidirectional"),
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
                                 actions = list(c("C", "D", "E"),
                                               c("F", "G"),
                                               c("H", "I"),
                                               c("J", "K")),
                                 payoffs = list(p1 = c(3, 1, 1, 2, 2, 1),
                                                p2 = c(0, 0, 1, 1, 2, 3)),
                                 family = "sans"),
                  "extensive_form")

 expect_error(extensive_form(players = list("p1",
                                            rep("p2", 3),
                                            rep(NA, 6)),
                             actions = list(c("C", "D", "E"),
                                            c("F", "G"),
                                            c("H", "I"),
                                            c("J", "K")),
                             payoffs = list(p1 = c(3, 1, 1, 2, 2, 1),
                                            p2 = c(0, 0, 1, 1, 2, 3)),
                             direction = "bidirectional"))
  expect_error(extensive_form(players = list("p1",
                                             rep("p2", 3)),
                              actions = list(c("C", "D", "E"),
                                             c("F", "G"),
                                             c("H", "I"),
                                             c("J", "K")),
                              payoffs = list(p1 = c(3, 1, 1, 2, 2, 1),
                                             p2 = c(0, 0, 1, 1, 2, 3))))
  expect_error(extensive_form(players = list("p1",
                                             rep("p2", 3),
                                             rep(NA, 6)),
                              action = list(c("C", "D", "E"),
                                            c("F", "G"),
                                            c("H", "I"),
                                            c("J", "K")),
                              payoff = list(p1 = c(3, 1, 1, 2, 2, 1),
                                            p2 = c(0, 0, 1, 1, 2, 3)),
                              direction = "trichotomous"))
})

test_that("seq_extensive transform seq-form into extensive-form", {
  expect_s3_class(seq_extensive(seq_form(s1 = c("T", "B"),
                                         s2 = c("L", "R"),
                                         payoffs1 = c(4, 2, 3, 1),
                                         payoffs2 = c(4, 3, 2, 1),
                                         players = c("Kamijo", "Yanai"))),
                  "extensive_form")
  expect_error(seq_extensive(seq_form(players = c("A", "B"),
                                      payoffs1 = "-x1^2 + (28 - x2) * x1",
                                      payoffs2 = "-x2^2 + (28 - x1) * x2",
                                      par1_lim = c(1, 2, 3),
                                      par2_lim = c(0, 10),
                                      pars = c("x1", "x2"))))
  expect_error(seq_extensive(extensive_form(players = list("p1",
                                                           rep("p2", 3),
                                                           rep(NA, 6)),
                                            actions = list(c("C", "D", "E"),
                                                           c("F", "G"),
                                                           c("H", "I"),
                                                           c("J", "K")),
                                            payoffs = list(p1 = c(3, 1, 1, 2, 2, 1),
                                                           p2 = c(0, 0, 1, 1, 2, 3)))))
})

test_that("normal_form expects payoffs of two players are in the same form", {
  expect_error(normal_form(players = c("A", "B"),
                           payoffs1 = function(x, y){-x^2 + (28-y) * x},
                           payoffs2 =  "-y^2 + (28 - x) * y",
                           pars = c("x", "y"),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)))
})

PD <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stays silent", "Betrays"),
  s2 = c("Stays silent", "Betrays"),
  payoffs1 = c(-1,  0, -3, -2),
  payoffs2 = c(-1, -3,  0, -2))

SH <- normal_form(
  players = c("Kamijo", "Yanai"),
  s1 = c("Stag", "Hare"),
  s2 = c("Stag", "Hare"),
  payoffs1 = c(10, 8, 0, 7),
  payoffs2 = c(10, 0, 8, 7))

char_game <- normal_form(
  players = c("A", "B"),
  payoffs1 = "-x^2 + (28 - y) * x",
  payoffs2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

seq_game <- seq_form(
  players = c("Leader", "Follower"),
  s1 = c("R", "S", "P"),
  s2 = c("R", "S", "P"),
  payoffs1 = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
  payoffs2 = c(0, 1, -1, -1, 0, 1, 1, -1, 0)
)

test_that("game_table makes a payoff matrix of normal-form games", {
  expect_s3_class(game_table(PD), "kableExtra")
  expect_s3_class(game_table(SH), "kableExtra")
  expect_error(game_table(char_game))
  expect_s3_class(game_table(seq_game), "kableExtra")
})
