plot_simu <- function(sim_res) {

  df <- sim_res %>%
    dplyr::group_by(period) %>%
    dplyr::summarize(play1 = mean(play1),
                     play2 = mean(play2),
                     .groups = "drop") %>%
    tidyr::pivot_longer(cols = play1:play2,
                        names_to = "Player",
                        values_to = "")

  p <- ggplot2::ggplot(df, aes(x = period, ))
}
