# rgamer 0.0.2

Now `game_table()`, which is called by `solve_nfg()` to display game tables, automatically adjusts the cell size of the table displayed unless `cell_width` is specified by the user.

In br\_plot (the plot of best response correspondences), two players' best responses are drawn by the lines with different width (ggplot's size) so that we can distinguish the two even when they overlap.

The message that warns users that the NE obtained by `solve_nfg()` might be a part of the solutions and asks them to examine the br\_plot.

# rgamer 0.0.1

The first, experimental version is out.
