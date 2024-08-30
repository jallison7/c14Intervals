shortest_cont_int <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year, curve = intcal20) {
  calibrated_distribution <- algorithm(radiocarbon_age = radiocarbon_age, error = error, curve = curve)

  continuous_intervals <- data.frame(matrix(nrow = nrow(calibrated_distribution), ncol = 6))
  colnames(continuous_intervals) <-  c("start", "AD_BC_start", "end", "AD_BC_end", "length", "probability")


  for (i in 1:nrow(calibrated_distribution)) {
    continuous_intervals$start[i] <- calibrated_distribution$cal_AD[i]
    sum_prob <- calibrated_distribution$probability[i]
    j <- i
    while (sum_prob < interval_prob & j < nrow(calibrated_distribution)) {
      j <- j + 1
      sum_prob = sum_prob + calibrated_distribution$probability[j]
    }
    continuous_intervals$end[i] <- calibrated_distribution$cal_AD[j]
    continuous_intervals$probability[i] <- sum_prob
  }

  for (i in 1:nrow(continuous_intervals)) {
    if(continuous_intervals$start[i] > 0) {
      continuous_intervals$AD_BC_start[i] <- "AD"
    }
    else {
      continuous_intervals$AD_BC_start[i] <- "BC"
      continuous_intervals$start[i] <- abs(continuous_intervals$start[i])
    }
    if(continuous_intervals$end[i] > 0) {
      continuous_intervals$AD_BC_end[i] <- "AD"
    }
    else {
      continuous_intervals$AD_BC_end[i] <- "BC"
      continuous_intervals$end[i] <- abs(continuous_intervals$end[i])
    }
      if (continuous_intervals$AD_BC_start[i] == continuous_intervals$AD_BC_end[i]) {
        seg_length <- abs(continuous_intervals$end[i] - continuous_intervals$start[i]) + 1
      }
      else {
        seg_length <- abs(continuous_intervals$start[i]) + (continuous_intervals$end[i])
      }
      continuous_intervals$length[i] <- seg_length
  }




  #this finds the shortest continuous interval that has the specified probability
  continuous_intervals <- filter(continuous_intervals, probability >= interval_prob)
  continuous_intervals <- filter(continuous_intervals,length == min(length))
  # when there are multiple intervals of the same length, and the shortest possible interval is continuous,
  # filtering for the maximum probability here should find the interval that matches the one found by the
  # shortest_interval function
  continuous_intervals <- filter(continuous_intervals, probability == max(probability))




  return(continuous_intervals)
}
