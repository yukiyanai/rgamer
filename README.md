
<!-- README.md is generated from README.Rmd. Please edit that file -->

rgamer
======

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/yukiyanai/rgamer.svg?branch=master)](https://travis-ci.org/yukiyanai/rgamer)
[![Codecov test
coverage](https://codecov.io/gh/yukiyanai/rgamer/branch/master/graph/badge.svg)](https://codecov.io/gh/yukiyanai/rgamer?branch=master)
<!-- badges: end -->

Overview
--------

The goal of `rgamer` is to help students learn Game Theory using R. The
functions provided by the package not only solve basic games such as
two-person normal-form games but also provides the users with visual
displays that highlight some aspects of the games — game matrices, best
response correspondence, etc. In addition, it suggests some numerical
solutions for games of which it is difficult — or even seems impossible
— to derive a closed-form analytic solution.

Installation
------------

<!--
#You can install the released version of rgamer from [CRAN](https://CRAN.R-project.org) with:

```r
install.packages("rgamer")
#> Warning: package 'rgamer' is not available (for R version 4.0.2)
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("yukiyanai/rgamer")

Example
-------

    library(rgamer)

### Example 1

An example of a normal-form game:

-   Player: {Kicker, GK }
-   Strategy: {(left, right), (left, right)}
-   Payoff: {(-1, 1, 1, -1), (1, -1, -1, 1)}

First, you define the game by `normal_form()`:

    game1 <- normal_form(
      players = c("Kicker", "GK"),
      s1 = c("left", "right"), 
      s2 = c("left", "right"),
      p1 = c(-1, 1, 1, -1), 
      p2 = c(1, -1, -1, 1))

Then, you can pass it to `solve_nfg()` function to get the table of the
game (not shown here) and the Nash equilibrium.

    s_game1 <- solve_nfg(game1, mixed = TRUE, show_table = FALSE)
    #> Pure strategy NE does not exist.
    #> Mixed-strategy NE: [(1/2, 1/2), (1/2, 1/2)]

For a 2-by-2 game, you can plot the best response correspondences as
well.

    s_game1$br_plot

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

### Example 2

An example of a normal-form game:

-   Player: { A, B }
-   Strategy: {*x* ∈ \[0, 30\], *y* ∈ \[0, 30\] }
-   Payoff:
    {*f*<sub>*x*</sub>(*x*, *y*) =  − *x*<sup>2</sup> + (28 − *y*)*x*,
    *f*<sub>*y*</sub>(*x*, *y*) =  − *y*<sup>2</sup> + (28 − *x*)*y*}

You can define a game by specifying payoff functions as character
vectors using `normal_form()`:

    game2 <- normal_form(
      players = c("A", "B"),
      p1 = "-x^2 + (28 - y) * x",
      p2 = "-y^2 + (28 - x) * y",
      par1_lim = c(0, 30),
      par2_lim = c(0, 30),
      pars = c("x", "y"))

Then, you can pass it to `solve_nfg()`, which displays the best response
correspondences by default.

    s_game2 <- solve_nfg(game2)

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

    #> NE: (28/3, 28/3)

### Example 3

An example of a normal-form game:

-   Player: { A, B }
-   Strategy: {*x* ∈ \[0, 30\], *y* ∈ \[0, 30\] }
-   Payoff:
    {*f*<sub>*x*</sub>(*x*, *y*) =  − *x*<sup>*a*</sup> + (*b* − *y*)*x*,
    *f*<sub>*y*</sub>(*x*, *y*) =  − *y*<sup>*s*</sup> + (*t* − *x*)*y*}

You can define a normal-form game by specifying payoffs by R functions.

    f_x <- function(x, y, a, b) {
      -x^a + (b - y) * x
    }
    f_y <- function(x, y, s, t) {
      -y^s + (t - x) * y
    }
    game3 <- normal_form(
      players = c("A", "B"),
      p1 = f_x,
      p2 = f_y,
      par1_lim = c(0, 30),
      par2_lim = c(0, 30),
      pars = c("x", "y"))

Then, you can approximate a solution numerically by `solve_nfg()`. Note
that you need to set the parameter values of the function that should be
treated as constants by arguments `cons1` and `cons2`, each of which
accepts a named list. In addition, you can suppress the plot of best
responses by `plot = FALSE`.

    s_game3 <- solve_nfg(
      game = game3,
      cons1 = list(a = 2, b = 28),
      cons2 = list(s = 2, t = 28),
      plot = FALSE
    )
    #> approximated NE: (9.3, 9.3)

You can increase the precision of approximation by `precision` (default
is `1`), which takes a natural number.

    s_game3b <- solve_nfg(
      game = game3,
      cons1 = list(a = 2, b = 28),
      cons2 = list(s = 2, t = 28),
      precision = 3
    )

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

    #> approximated NE: (9.333, 9.333)
