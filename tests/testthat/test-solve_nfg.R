context("test-solve_nfg")

game1 <- normal_form(
  players = c("Kicker", "GK"),
  s1 = c("left", "right"),
  s2 = c("left", "right"),
  p1 = c(-1, 1, 1, -1),
  p2 = c(1, -1, -1, 1))

game2 <- normal_form(
  players = c("A", "B"),
  p1 = "-x^2 + (28 - y) * x",
  p2 = "-y^2 + (28 - x) * y",
  par1_lim = c(0, 30),
  par2_lim = c(0, 30),
  pars = c("x", "y"))

test5_f1 <- function(xA, xB, a) {
  (a - xA - xB) * xA - 3 * xA
}
test5_f2 <- function(xA, xB, b) {
  (b - xA - xB) * xB - 3 * xB
}
game3 <- normal_form(
  players = c("K", "Y"),
  p1 = test5_f1,
  p2 = test5_f2,
  pars = c("xA", "xB"),
  cons1 = list(a = 30),
  cons2 = list(b = 30),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30))

test_that("solve_nfg finds NE of the game", {
  expect_equal(solve_nfg(game1)$psNE, NULL)
  skip_on_cran()
  expect_equal(solve_nfg(game1,
                         mixed = TRUE,
                         show_table = FALSE,
                         quietly = TRUE)$msNE$s1,
               c(0.5, 0.5))
  expect_equal(solve_nfg(game1,
                         mixed = TRUE,
                         show_table = FALSE,
                         quietly = TRUE)$msNE$s2,
               c(0.5, 0.5))
  expect_equal(solve_nfg(game2,
                         plot = FALSE,
                         quietly = TRUE)$NE,
               c(28/3, 28/3))
  expect_equal(round(solve_nfg(game3,
                               plot = FALSE,
                               precision = 3,
                               quietly = TRUE)$NE,
                     digits = 0),
               c(9, 9))
})
