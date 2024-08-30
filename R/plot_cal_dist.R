plot_cal_dist <- function(radiocarbon_age, error, length = 10, algorithm = calibrate_one_year, curve = intcal20) {
  calibrated_distribution <- algorithm(radiocarbon_age, error, length, curve)
  x_min <- min(calibrated_distribution$cal_AD[which(calibrated_distribution$probability > .000001)])
  x_max <- max(calibrated_distribution$cal_AD[which(calibrated_distribution$probability > .000001)])
  library(tidyverse)
  ggplot(calibrated_distribution, (aes(x = cal_AD, y = probability))) +
    geom_line() +
    xlim(x_min, x_max) +
    labs(title = paste(radiocarbon_age, "+/-", error, "bp"), x = "cal BC/AD") +
    theme_bw() +
    theme(plot.title = element_text(size=18, hjust=.5))
}
