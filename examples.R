# examples.R
library(tidyverse)
library(stringi)
library(gt)
library(MASS)

## load all functions of the package
source("normal_form.R")
source("find_best_response.R")
source("show_best_response.R")
source("game_matrix.R")
source("find_pure_NE.R")
source("find_mixed_NE.R")
source("solve_normal_form.R")
source("br_plot.R")

## Examples
Game1 <- normal_form(s1 = c("T", "B"), s2 = c("L", "R"),
                     p1 = c(4, 2, 3, 1), p2 = c(4, 3, 2, 1),
                     players = c("Kamijo", "Yanai"))
s1 <- solve_nfg_matrix(Game1)
s1m <- solve_nfg(Game1, mixed = TRUE)
s1$br_plot

Game2 <-normal_form(players = c("Player A", "Player B"),
                    s1 = c("グー", "チョキ", "パー"),
                    s2 = c("グー", "チョキ", "パー"),
                    p1 = c(0, -1, 1, 1, 0, -1, -1, 1, 0),
                    p2 = c(0, 1, -1, -1, 0, 1, 1, -1, 0))
s2 <- solve_nfg(Game2, mixed = TRUE,mark_br = FALSE)
## The following returns NULL because the game is not 2x2
s2$br_plot

Game3 <- normal_form(s1 = c("B", "F"), s2 = c("B", "F"),
                     p1 = c(2, 0, 0, 1), p2 = c(1, 0, 0, 2))
s3 <- solve_normal_form(Game3, mixed = TRUE)
br_plot(Game3)

Game4 <- normal_form(s1 = c("left", "right"), s2 = c("left", "right"),
                     p1 = c(-1, 1, 1, -1), p2 = c(1, -1, -1, 1),
                     players = c("Kicker", "GK"))
s4 <- solve_normal_form(Game4, mixed = TRUE)
br_plot(Game4)

Game5 <- normal_form(s1 = c("right", "left"), s2 = c("right", "left"),
                     p1 = c(1, 0, 0, 1), p2 = c(1, 0, 0, 1),
                     players = c("E", "W"))
s5 <- solve_normal_form(Game5, mixed = TRUE)
br_plot(Game5)

Game6 <- normal_form(s1 = c("Drama", "Reality"), s2 = c("Drama", "Reality"),
                     p1 = c(7, 5, 4, 6), p2 = c(3, 5, 6, 4),
                     players = c('A', 'B'))
s6 <- solve_normal_form(Game6, mixed = TRUE)
br_plot(Game6)

Game7 <- normal_form(s1 = c("T", "B"), s2 = c("L", "R"),
                     p1 = rep(1, 4), p2 = rep(1, 4))
s7 <- solve_normal_form(Game7, mixed = TRUE)
br_plot(Game7)


#####################
# solve_nfg_char.R
###################
## Examples
Game1 <- normal_form(players = c("A", "B"),
                     p1 = "-x1^2 + (28 - x2) * x1",
                     p2 = "-x2^2 + (28 - x1) * x2",
                     par1_lim = c(0, 30),
                     par2_lim = c(0, 30),
                     pars = c("x1", "x2"))
eg1 <- solve_nfg_char(Game1)


#Game2 <- normal_form(
#  players = c("Microsoft", "Apple"),
#  p1 = "(30 - xA - xB) * xA - 3 * xA",
#  p2 = "(30 - xA - xB) * xB - 3 * xB",
#  pars = c("xA", "xB"),
#  par1_lim = c(0, 30),
#  par2_lim = c(0, 30)
#)
#eg2 <- solve_nfg_char(Game2)


#Game3 <- normal_form(
#  players = c("A", "B"),
#  p1 = "(30 - x - y) * x - x^2" ,
#  p2 = "-2 * y^2 + (30 - x) * y",
#  pars = c("x", "y"),
#  par1_lim = c(0, 40),
#  par2_lim = c(0, 40)
#)
#eg3 <- solve_nfg_char(Game3)

#Game4 <- normal_form(
#  players = c("Microsoft", "Apple"),
#  p1 = "(30 - xA - xB) * xA - 3 * xA",
#  p2 = "(30 - xA - xB) * xB - 3 * xB",
#  pars = c("xA", "xB"),
#  par1_lim = c(0, 40),
#  par2_lim = c(0, 30)
#)
#eg4 <- solve_nfg_char(Game4, plot = FALSE)
#eg4$NE
#plot(eg4$br_plot)


####################
##solve_nfg_fcn.R
####################
## Examples

## Example 1
test1_f1 <- function(x1, x2) {
  -x1^2 + (28 - x2) * x1
}
test1_f2 <- function(x1, x2) {
  -x2^2 + (28 - x1) * x2
}
Game1 <- normal_form(
  players = c("A", "B"),
  p1 = test1_f1,
  p2 = test1_f2,
  pars = c("x1", "x2"),
  par1_lim = c(0, 40),
  par2_lim = c(0, 40))
eg1 <- solve_nfg_fcn(Game1, precision = 3)

## Example 2: when functions have more than 2 arguments
test2_f1 <- function(x1, x2, a, b) {
  -x1^a + (b - x2) * x1
}
test2_f2 <- function(x1, x2, a, b) {
  -x2^a + (b - x1) * x2
}
## Need to specify "cons_common" by a named list for parameters that are not under examination
Game2 <- normal_form(
  players = c("A", "B"),
  p1 = test2_f1,
  p2 = test2_f2,
  pars = c("x1", "x2"),
  cons_common = list(a = 2, b = 28),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30)
)
eg2 <- solve_nfg_fcn(Game2, precision = 2)

## Example 3: 2 functions can have different arguments for "other" parameters
test3_f1 <- function(x1, x2, a, b) {
  -x1^a + (b - x2) * x1
}
test3_f2 <- function(x1, x2, s, t) {
  -x2^s + (t - x1) * x2
}
## Need to specify "cons" and "cons2" separately for 2 functions that have different arguments
Game3 <- normal_form(
  players = c("A", "B"),
  p1 = test3_f1,
  p2 = test3_f2,
  pars = c("x1", "x2"),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30))
eg3 <- solve_nfg_fcn(Game3,
                     cons1 = list(a = 2, b = 28),
                     cons2 = list(s = 2, t = 28),
                     precision = 2)

## Example 4: just another example -- the order of the argument does NOT matter
test4_f1 <- function(x, y) {
  (30 - x - y) * x - x^2
}
test4_f2 <- function(y, x) {
  -2 * y^2 + (30 - x) * y
}
Game4 <- normal_form(
  p1 = test4_f1,
  p2 = test4_f2,
  par1_lim = c(0, 40),
  par2_lim = c(0, 40),
  pars = c("x", "y"))
eg4 <- solve_nfg_fcn(Game4)

## Example 5: Yet another example
test5_f1 <- function(xA, xB, a) {
  (a - xA - xB) * xA - 3 * xA
}
test5_f2 <- function(xA, xB, b) {
  (b - xA - xB) * xB - 3 * xB
}
Game5 <- normal_form(
  players = c("K", "Y"),
  p1 = test5_f1,
  p2 = test5_f2,
  pars = c("xA", "xB"),
  cons1 = list(a = 30),
  cons2 = list(b = 30),
  par1_lim = c(0, 30),
  par2_lim = c(0, 30))
eg5 <- solve_nfg_fcn(Game5, plot = FALSE, precision = 3, quietly = TRUE)
eg5$NE
plot(eg5$br_plot)

