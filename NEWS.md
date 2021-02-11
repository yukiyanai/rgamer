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
