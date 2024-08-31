shortest_cont_int <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year, curve = intcal20) {
  calibrated_distribution <- algorithm(radiocarbon_age = radiocarbon_age, error = error, curve = curve)

  continuous_interval <- data.frame(matrix(nrow = nrow(calibrated_distribution), ncol = 11))
  colnames(continuous_interval) <-  c("radiocarbon_age", "error", "nominal_prob", "type", "start",
                                   "AD_BC_start", "end", "AD_BC_end", "length", "total_range",
                                   "probability")


  for (i in 1:nrow(calibrated_distribution)) {
    continuous_interval$start[i] <- calibrated_distribution$cal_AD[i]
    sum_prob <- calibrated_distribution$probability[i]
    j <- i
    while (sum_prob < interval_prob & j < nrow(calibrated_distribution)) {
      j <- j + 1
      sum_prob = sum_prob + calibrated_distribution$probability[j]
    }
    continuous_interval$end[i] <- calibrated_distribution$cal_AD[j]
    continuous_interval$probability[i] <- sum_prob
  }

  for (i in 1:nrow(continuous_interval)) {
    if(continuous_interval$start[i] > 0) {
      continuous_interval$AD_BC_start[i] <- "AD"
    }
    else {
      continuous_interval$AD_BC_start[i] <- "BC"
      continuous_interval$start[i] <- abs(continuous_interval$start[i])
    }
    if(continuous_interval$end[i] > 0) {
      continuous_interval$AD_BC_end[i] <- "AD"
    }
    else {
      continuous_interval$AD_BC_end[i] <- "BC"
      continuous_interval$end[i] <- abs(continuous_interval$end[i])
    }
      if (continuous_interval$AD_BC_start[i] == continuous_interval$AD_BC_end[i]) {
        seg_length <- abs(continuous_interval$end[i] - continuous_interval$start[i]) + 1
      }
      else {
        seg_length <- abs(continuous_interval$start[i]) + (continuous_interval$end[i])
      }
      continuous_interval$length[i] <- seg_length
  }




  #this finds the shortest continuous interval that has the specified probability
  continuous_interval <- filter(continuous_interval, probability >= interval_prob)
  continuous_interval <- filter(continuous_interval,length == min(length))
  # when there are multiple intervals of the same length, and the shortest possible interval is continuous,
  # filtering for the maximum probability here should find the interval that matches the one found by the
  # shortest_interval function
  continuous_interval <- filter(continuous_interval, probability == max(probability))

  continuous_interval$radiocarbon_age <- radiocarbon_age
  continuous_interval$error <- error
  continuous_interval$nominal_prob <- interval_prob
  continuous_interval$type <- "shortest continuous"
  continuous_interval$total_range <- continuous_interval$length

  return(continuous_interval)
}
