plot_intervals <- function(radiocarbon_age, error, interval_prob = .95, length = 10, algorithm = calibrate_one_year, curve = intcal20) {
  library(tidyverse)

  #get the data to plot
  calibrated_distribution <- algorithm(radiocarbon_age, error, length, curve)
  cont <- shortest_cont_int(radiocarbon_age = radiocarbon_age, error = error,
                             interval_prob = interval_prob, algorithm = algorithm,
                             curve = curve)
  central <- central_int(radiocarbon_age = radiocarbon_age, error = error,
                         interval_prob = interval_prob, algorithm = algorithm,
                         curve = curve)
  short <- shortest_int(radiocarbon_age = radiocarbon_age, error = error,
                        interval_prob = interval_prob, algorithm = algorithm,
                        curve = curve)


  # convert BC dates to negative AD
  if (cont$AD_BC_start == "BC") {
    cont$start <- 0 - cont$start
  }
  if (cont$AD_BC_end == "BC") {
    cont$end <- 0 - cont$end
  }

  if (central$AD_BC_start == "BC") {
    central$start <- 0 - central$start
  }
  if (central$AD_BC_end == "BC") {
    central$end <- 0 - central$end
  }

  for (i in 1:nrow(short)) {
    if (short$AD_BC_start[i] == "BC") {
      short$start[i] <- 0 - short$start[i]
    }
    if (short$AD_BC_end[i] == "BC") {
      short$end[i] <- 0 - short$end[i]
    }


  }

  #separate out the total combined range of the shortest interval
  sum_row <- short[nrow(short), ]
  short <- filter(short, type != "total shortest")

  #find the range of dates to plot
  x_min <- min(calibrated_distribution$cal_AD[which(calibrated_distribution$probability > .000001)])
  x_max <- max(calibrated_distribution$cal_AD[which(calibrated_distribution$probability > .000001)])

  #find the appropriate x-axis label
    if (x_min > 0) {
    x_label <- "cal AD"
  }
  else {
    if (x_max < 0) {
      x_label <- "cal BC"
    }
    else {
      x_label <- "cal BC/AD"
    }

  }

  #find the y-axis scale and spacing
  ymax <- max(calibrated_distribution$probability)
  spacing <- ymax/20


  library(tidyverse)
plot <- ggplot() +
    geom_line(calibrated_distribution, mapping = aes(x = cal_AD, y = probability)) +
    geom_segment(data = cont, aes(x = start, xend = end, y = (ymax + (6 * spacing))), linewidth = 1.5) +
    geom_text(data = cont, mapping = aes(x = ((end + start) / 2), y = (ymax + (7 * spacing)), label = type)) +
    geom_segment(data = central, aes(x = start, xend = end, y = (ymax + (3.5 * spacing))), linewidth = 1.5) +
    geom_text(data = central, mapping = aes(x = ((end + start) / 2), y = (ymax + (4.5 * spacing)), label = type)) +
    xlim(x_min, x_max) +
    labs(title = paste(radiocarbon_age, "+/-", error, "bp"), x = x_label) +
    theme_bw() +
    theme(plot.title = element_text(size=18, hjust=.5))

  for (i in 1:nrow(short)) {
    plot_seg <- short[i,]
    plot <- plot + geom_segment(data = plot_seg, aes(x = start, xend = end,
                               y = (ymax + spacing)), linewidth = 1.5)
  }

  sum_row$type <- "shortest"
  plot <- plot + geom_text(data = sum_row, mapping = aes(x = ((end + start) / 2), y = (ymax + (2 * spacing)), label = type))
  return(plot)
}

