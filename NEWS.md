# rgamer 0.0.2

`extensive_form()` has been added to solve a simple extensive form game. The function draws a game tree, finds a subgame perfect equilibrium, and shows the equilibrium path on the tree.

Now `game_table()`, which is called by `solve_nfg()` to display game tables, automatically adjusts the cell size of the table displayed unless `cell_width` is specified by the user.

In br\_plot (the plot of best response correspondences), two players' best responses are drawn by the lines with different width (ggplot's size) so that we can distinguish the two even when they overlap. And a minor bug has been fixed.

The message that warns users that the NE obtained by `solve_nfg()` might be a part of the solutions and asks them to examine the br\_plot.


# rgamer 0.0.1

The first, experimental version is out.
