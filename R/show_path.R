#' @title Plot a game tree with solution paths colored.
#' @description \code{show_path} displays the paths played under a solution.
#' @param x An extensive_form object defined by \code{extensive_form()} or its
#'     solution (extensive_sol object) found by \code{solve_efg()}.
#' @param id An integer value vector of id numbers to specify which solutions to
#'      show. By default, the first solution is displayed. This parameter is
#'      ignored when \code{all = TRUE}.
#' @param all A logical value to determine if all solutions will be displayed.
#'     Default is \code{FALSE}.
#' @return A ggplot object of the specified game tree.
#' @include solve_efg.R
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
#' @export
show_path <- function(x, id = NULL, all = FALSE) {

  if (!(class(x) %in% c("extensive_form", "extensive_sol"))) {
    stop("x must be a game defined by extensive_form() or its solution found by solve_efg().")
  }

  if (methods::is(x, "extensive_form")) x <- solve_efg(x, quietly = TRUE)

  n_sols <- x$n_sols
  if (n_sols > 1 & !all)
    warning("Multiple versions of solution trees exist. Set all = TRUE to see all trees.")

  if (all) {
    id <- 1:n_sols
  } else if (is.null(id)) {
    id <- 1
  }

  if (length(id) == 1) {
    plot(x$trees[[id]])
  } else {
    p_list <- list()
    for (i in seq_along(id)) {
      p_list[[i]] <- x$trees[[id[i]]]
    }
    patchwork::wrap_plots(p_list)
  }
}
