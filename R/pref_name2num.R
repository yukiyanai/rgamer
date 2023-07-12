#' @title Transform preference given by names to number
#' @return A list
#' @param g1_prefs A named list of preferences of individuals who make
#'     proposals.
#' @param g2_prefs A named list of preferences of individuals who receives
#'     proposals.
#' @noRd
#' @author Yoshio Kamijo and Yuki Yanai <yanai.yuki@@kochi-tech.ac.jp>
pref_name2num <- function(g1_prefs, g2_prefs) {

  g1_names <- names(g1_prefs)
  g2_names <- names(g2_prefs)
  n_g1 <- length(g1_prefs)
  n_g2 <- length(g2_prefs)

  if (!all(sapply(g1_prefs, is.numeric)) | !all(sapply(g2_prefs, is.numeric))) {
    for (i in 1 : n_g1) {
      J <- length(g1_prefs[[i]])
      for (j in 1 : J) {
        continue <- TRUE
        k <- 1
        while (continue & k <= n_g2) {
          if (g1_prefs[[i]][j] == g2_names[k]) {
            g1_prefs[[i]][j] <- k
            continue <- FALSE
          } else {
            k <- k + 1
          }
        }
        if (continue)
          stop(paste(g1_prefs[[i]][j], "does not exist among candidates."))
      }
    }
    g1_prefs <- lapply(g1_prefs, as.integer)

    for (i in 1 : n_g2) {
      J <- length(g2_prefs[[i]])
      for (j in 1 : J) {
        continue <- TRUE
        k <- 1
        while (continue & k <= n_g1) {
          if (g2_prefs[[i]][j] == g1_names[k]) {
            g2_prefs[[i]][j] <- k
            continue <- FALSE
          } else {
            k <- k + 1
          }
        }
        if (continue)
          stop(paste(g2_prefs[[i]][j], "does not exist among candidates."))
      }
    }
    g2_prefs <- lapply(g2_prefs, as.integer)
  }
  list(g1_prefs = g1_prefs,
       g2_prefs = g2_prefs)
}
