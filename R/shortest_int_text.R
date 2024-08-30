shortest_int_text <- function(radiocarbon_age, error, interval_prob = .95,
                               algorithm = calibrate_one_year, curve = intcal20) {

  interval_segments <- shortest_int(radiocarbon_age = radiocarbon_age, error = error,
                                interval_prob = interval_prob, algorithm = algorithm,
                                curve = curve)
  count_segments <- nrow(interval_segments)


  years <- 0
  for (i in 1:nrow(interval_segments)) {
    years <- years + interval_segments$length[i]
  }
  result <- paste("The shortest", interval_prob, "calibrated interval for", radiocarbon_age,
                  "+/-", error, "includes", years, "years in", count_segments, "segment(s)",
                  sep = " ")
  for (i in 1:count_segments) {
    temp <- paste(interval_segments$start[i], interval_segments$AD_BC_start[i], "-", interval_segments$end[i],
                interval_segments$AD_BC_end[i], ", p =", round(interval_segments$prob[i], 3))
    result <- paste(result, temp, sep = "; ")
  }

  return(result)

}
