shortest_cont_int_text <- function(radiocarbon_age, error, interval_prob = .95,
                                   algorithm = calibrate_one_year, curve = intcal20) {
  continuous_intervals <- shortest_cont_int(radiocarbon_age = radiocarbon_age, error = error,
                           interval_prob = interval_prob, algorithm = algorithm,
                           curve = curve)

for (i in 1:nrow(continuous_intervals))  {
  temp <- paste0(continuous_intervals$start[i], " ", continuous_intervals$AD_BC_start[i]," - ",
                 continuous_intervals$end[i], " ", continuous_intervals$AD_BC_end[i], " , p =",
                 round(continuous_intervals$probability[i], 3))
  if (i == 1) {
    result <- temp
  }
    else {
      result <- paste(result, temp, sep = "; ")
    }
}

  return(result)

}
