# rgamer 0.0.26

- Now `is_stable()` can check the stability of user specified matching.
- Make `spe()` a little more efficient by modifying `extensive_strategy()`.

# rgamer 0.0.25

- Fix bugs in the game trees with SPEs.
  - `spe()`

# rgamer 0.0.24

- Fix bugs in the functions to find dominant/dominated strategies.
  - `find_dominated()`
  - `find_dominant()`

# rgamer 0.0.23

- Fix bugs in:
  - `extensive_form()`
  - `draw_tree()`
  - `spe()`

# rgamer 0.0.22

- Bug fixes of the functions related to many-to-one matching.

# rgamer 0.0.21

- First release of the stable version.
- Fixed a bug of reinforcement learning in `sim_learning()` function.

# rgamer 0.0.20

- `extensive_form()` now accepts player names containing white spaces, but each white space will be replaced with an underscore.

# rgamer 0.0.19

## New Features

- `matching_mt1()` implements many-to-one matching.

## Changes

- Fixed bugs in `matching()` with DA (Gale-Shapley) algorithm used.

# rgamer 0.0.18

## Changes

- Replace magrittr pipes `%>%` with R's native pipes `|>`.

# rgamer 0.0.17

## New Features

- `sim_learning()` simulates how players learn to play better in a two-person normal-form game.
- `sim_fict()` simulates fictitious plays with a two-person normal-form game.

## Changes

- `normal_fomr()` can now handle `discretize = TRUE` for payoff functions defined by character strings.
- Improves performance of `sim_game()`.


# rgamer 0.0.16

## New Features

- `sim_game()` (which was previously `simu_game()`) now automatically produce the ggplot graphs of simulation results.

## Changes

- For `normal_form()` and `seq_form()`, the parameters `p1` and `p2` are renamed to `payoffs1` and `payoffs2`, respectively.
- `simu_game()` is now `sim_game()`, that is, "u" has been removed from the function name.



# rgamer 0.0.15

## New Features

- Results of `matching()` and `matching_df()` now contains the information of
the ranking of matches.
- Users can examine if a matching is stable by `is_stable()` function.

# rgamer 0.0.14

## New Features

- `matching()` and `matching_df()` now implements "Boston mechanism" matching. Still works only for one-to-one matching, though.
- `eval_matching()` evaluates how desirable a matching result is for a player.


## Bug Fix

- `solve_nfg()`
- `print.matching()`

# rgamer 0.0.13

## New Functions

- The following two functions to implement matching has been added. Currently, only Gale-Shapley (deferred acceptance; DA) algorithm is supported.
  - `matching()`
  - `matching_df()`
  
## Bug Fix

- `restrict_action()` now works with imperfect-information games. 


# rgamer 0.0.12

## New Functions

- `restrict_action()` restricts actions at specified nodes in an extensive-form game.



# rgamer 0.0.11

## New Functions

- `subgames()` finds subgames of an extensive-form game.
- Now `solve_efg()` can find subgame perfect equilibria by setting `concept = "spe"`.

## Changes

- Appearance of payoff matrix has changed. It has shifted from gt to kableExtra. An argument `cell_width` has been removed.

# rgamer 0.0.10

## Changes in normal-form and extensive-form games

- Now payoffs of `normal_form()` can be specified by each cell with `cells` argument rather than by each player with `p1` and `p2`. You need to add `byrow = TRUE` to fill the matrix by row. The matrix is filled by column by default.
- Similarly, payoffs of `extensive_form()` can be specified by each terminal node with `payoffs2` argument rather than by each player with `payoff` argument.
- Display order of strategies has been changed when an extensive-form game is transformed into a matrix game by `to_matrix()`.
- Users can explicitly specify a single-element information set.
- `extensive_form()` throws an error if a node belongs to more than one information set.

# rgamer 0.0.9

## New functions

- `to_matrix()` transforms an extensive form game defined by `extensive_form()` into a normal-form game. 
- In a game tree, You can specify what actions each player takes and show them with color by `draw_path()`


## Changes in extensive-form games

- You can specify information sets by the `info_set` argument.
- An object of `extensive_form` class now has the list of strategies, `strategy`.


# rgamer 0.0.8

## Changes in extensive-form games

- `extensive_form()` function has been drastically modified. Now the function only defines a game. It no longer finds solution of an extensive-form game. 
- To find a solution of the game, please use `solve_efg()` function. Currently, it finds backward-induction solutions of the game.
- To display game treese with the solution paths colored, please use `show_path()`.

## New functions

- `seq_extensive()` transforms a "seq_form" class game into an "extensive_form" class.
- `get_payoff()` returns the payoff corresponding to a specified pair of strategies or a specified set of actions profiles.


# rgamer 0.0.7

## Making a game tree 

- The way to draw a game tree by `extensive_form()` has been simplified.
  - Users do not have to specify `n_node` or `n_choices`.
- The positions of payoffs has been adjusted.  
- Two new options for `direction`, "horizontal" and "vertical" are available.

## Functions for a simple leader-follower game

Functions to define and solve a simple leader-follower sequential game (e.g., Stackelberg competition) have been added.

- Users can define such a game by `seq_form()` function.
- Users can obtain the subgame perfect equilibrium outcome by `solve_seq()` function.


# rgamer 0.0.6

This version contains three new functions: `find_dominant()` and `find_dominated()` finds (weakly) dominant and (weakly) dominated strategies, respectively, and `eliminate_strategy()` eliminates the specific player's specific strategy from a normal-form game. In addition, Some bugs have been fixed.

# rgamer 0.0.5

A function to simulate plays expected in a normal-form game is added.

# rgamer 0.0.4

This version adds options to (1) discretized strategies with continuous payoff functions (see Example 5 in README) and (2) draw a bidirectional game tree for an extensive-form game Some minor bugs have been fixed. Now users can choose a color palette for ggplot2 figures. 

# rgamer 0.0.3

Two bugs have been fixed. First, the bug that incorrectly finds a NE, which should not exist, has been fixed. Second, a discrete best response correspondence was displayed as a continuous path in the previous version, but the current versions shows it as discrete.

Now, when you specify payoffs as functions, you can choose if NE should be displayed in the best response plot by `mark_NE`.  After saving the solution to an object, you can retract the best response plot with and without the NE by `br_plot_NE` and `br_plot`, respectively,



# rgamer 0.0.2

`extensive_form()` has been added to solve a simple extensive form game. The function draws a game tree, finds a subgame perfect equilibrium, and shows the equilibrium path on the tree.

Now `game_table()`, which is called by `solve_nfg()` to display game tables, automatically adjusts the cell size of the table displayed unless `cell_width` is specified by the user.

In br\_plot (the plot of best response correspondences), two players' best responses are drawn by the lines with different width (ggplot's size) so that we can distinguish the two even when they overlap. And a minor bug has been fixed.

The message that warns users that the NE obtained by `solve_nfg()` might be a part of the solutions and asks them to examine the br\_plot.


# rgamer 0.0.1

The first, experimental version is out.
