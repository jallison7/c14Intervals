calibrate_one_year <- function(radiocarbon_age, error, length = 10, curve = intcal20) {
  calibration_curve <- curve
  library(tidyverse)

  min_diff <- min(abs(calibration_curve$c14_age - radiocarbon_age))
  earliest_cal_date <- max(calibration_curve$cal_bp[which(abs(calibration_curve$c14_age - radiocarbon_age) == min_diff)]) + (length * error)
  latest_cal_date <- min(calibration_curve$cal_bp[which(abs(calibration_curve$c14_age - radiocarbon_age) == min_diff)]) - (length * error)

  # the next lines just make sure the start and end dates correspond to actual
  # existing cal_bp dates in the calibration curve, and, if not, adjusts them to the
  # closest existing cal_bp dates
  min_diff_early <- min(abs(calibration_curve$cal_bp - earliest_cal_date))
  min_diff_late <- min(abs(calibration_curve$cal_bp - latest_cal_date))
  earliest_cal_date <- calibration_curve$cal_bp[which(abs(calibration_curve$cal_bp - earliest_cal_date) == min_diff_early)]
  latest_cal_date <- calibration_curve$cal_bp[which(abs(calibration_curve$cal_bp - latest_cal_date) == min_diff_late)]

  # the code here creates a dataframe
  # called 'cal_prob' to store the calibrated probabilities

  start_row <- which(calibration_curve[,1] == earliest_cal_date)
  end_row <- which(calibration_curve[,1] == latest_cal_date)

  cal_prob <- as.data.frame(matrix(nrow = ((end_row + 1) - start_row), ncol = 4))
  colnames(cal_prob) <- c("cal_bp", "cal_AD", "probability", "cumul_probability")

  cal_prob$cal_bp <- calibration_curve[start_row:end_row, 1]

  #this creates a column with dates expressed as years AD (BC dates are negative)
  cal_prob$cal_AD <- 1950 - cal_prob$cal_bp

  #this takes out the non-existent year 0 AD
  for (i in 1:nrow(cal_prob)) {
    if (cal_prob[i,2] < 1) {
      cal_prob[i,2] <- cal_prob[i,2] - 1
    }
  }

  #finding the calibrated probability distribution
  for (i in 1:nrow(cal_prob)) {
    c14_age <- calibration_curve$c14_age[which(calibration_curve$cal_bp == cal_prob$cal_bp[i])]
    curve_error <- calibration_curve$sigma[which(calibration_curve$cal_bp == cal_prob$cal_bp[i])]
    sd <- sqrt(error^2 + curve_error^2)
    # this finds the cumulative probability of the radiocarbon age
    # at the point corresponding to the calibrated year, then the next year, then takes
    # the difference and assigns that to the calibrated year
    cumulative_1 <- pnorm(c14_age, radiocarbon_age, error)
    cumulative_2 <- pnorm((c14_age + 1), radiocarbon_age, error)
    cal_prob[i,3] <- cumulative_2 - cumulative_1
  }
  # finds the total calibrated probabilities for all dates,
  # then normalizes so that the calibrated probabilities sum to 1
  total <- sum(cal_prob[,3])
  for (i in 1:nrow(cal_prob)) {
    cal_prob[i,3] <- cal_prob[i,3]/total
  }

  #filling the cumulative probability column
  cumul <- 0
  for (i in 1:nrow(cal_prob)) {
    cumul <- cumul + cal_prob[i,3]
    cal_prob[i,4] <- cumul
  }


  return(cal_prob)
}
