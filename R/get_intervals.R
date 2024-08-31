  get_intervals <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year,
                           curve = intcal20) {

  continuous_interval <- shortest_cont_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)
  interval_segments <- shortest_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)
  central_interval <- central_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)






  intervals_combined <- rbind(interval_segments, continuous_interval,
                             central_interval)

  intervals_combined <- mutate(intervals_combined, total_range = NA, .after = length)


  for (i in 1:nrow(intervals_combined)) {
    if (intervals_combined$AD_BC_start[i] == intervals_combined$AD_BC_end[i]) {
      total_length <- abs(intervals_combined$end[i] - intervals_combined$start[i]) + 1
    }
    else {
      total_length <- abs(intervals_combined$start[i]) + (intervals_combined$end[i])
    }
    intervals_combined$total_range[i] <- total_length
  }

  intervals_combined$probability <- round(intervals_combined$probability, 3)

 return(intervals_combined)


}
