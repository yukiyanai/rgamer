context("Numerically find equilibria by grid search")


f_x <- function(x, y) {
  -x^2 + (28 - y) * x
}
f_y <- function(x, y, s, t) {
  -y^2 + (28 - x) * y
}

test_that("gridsearch_br numerically finds NE of normal-form games", {
  expect_s3_class(gridsearch_br(players = 1:2,
                                payoffs1 = f_x,
                                payoffs2 = f_y,
                                pars = c("x", "y"),
                                par1_lim = c(0, 30),
                                par2_lim = c(0, 30)),
                  "data.frame")
  expect_equal(ncol(gridsearch_br(players = 1:2,
                                  payoffs1 = f_x,
                                  payoffs2 = f_y,
                                  pars = c("x", "y"),
                                  par1_lim = c(0, 30),
                                  par2_lim = c(0, 30))), 2)
  expect_equal(ncol(gridsearch_br(players = 1:2,
                                  payoffs1 = f_x,
                                  payoffs2 = f_y,
                                  pars = c("x", "y"),
                                  par1_lim = c(0, 30),
                                  par2_lim = c(0, 30),
                                  precision = 3)), 2)
  expect_error(gridsearch_br(players = 1:2,
                             payoffs1 = f_x,
                             payoffs2 = f_y,
                             pars = c("x", "y")))
  expect_error(gridsearch_br(players = 1:2,
                             payoffs1 = f_x,
                             payoffs2 = f_y,
                             par1_lim = c(0, 30),
                             par2_lim = c(0, 30)))
})

test_that("gridsearch_backward finds spe outcomes by grid search", {
  expect_s3_class(gridsearch_backward(f_x, f_y, 0:100, 0:100),
                  "data.frame")
  expect_equal(ncol(gridsearch_backward(f_x, f_y, 0:100,0:100)), 4)
  expect_equal(ncol(gridsearch_backward(f_x, f_y, 0:100,0:100,
                                        pars = c("x", "y"))), 4)
  expect_error(gridsearch_backward(f_x, f_y))
  expect_error(gridsearch_backward("-x^2 + x*y + 28 * x",
                                   "-y^2 + x*y + 28 * y",
                                   0:100, 0:100))
})
