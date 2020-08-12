context("test-normal_form")

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
})

test_that("normal_form expects payoffs of two players are in the same form", {
  expect_error(normal_form(players = c("A", "B"),
                           p1 = function(x, y){-x^2 + (28-y) * x},
                           p2 =  "-y^2 + (28 - x) * y",
                           pars = c("x", "y"),
                           par1_lim = c(0, 30),
                           par2_lim = c(0, 30)))
})
