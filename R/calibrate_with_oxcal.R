#using 'raw probabilities' from oxcal through Martin Heinz et al.'s oxcAAR package

calibrate_with_oxcal <- function(radiocarbon_age, error, length = 10, curve = intcal20, oxcal_path = "C:/Program Files/OxCal/bin/OxCalWin.exe") {
  library(oxcAAR)
  library(tidyverse)
  setOxcalExecutablePath(oxcal_path)
  oxcal_calibration <- oxcalCalibrate(radiocarbon_age, error)
  cal_prob <- oxcal_calibration$'1'$raw_probabilities


  #put the cal_BP dates into the data frame, reordering and renaming columns to match
  # my other calibration functions
  colnames(cal_prob) <- c("cal_AD", "probability")
  cal_prob <- mutate(cal_prob, cal_bp = (1950 - cal_AD)) %>%
    mutate(cumul_probability = NA) %>%
    relocate(cal_bp, .before = cal_AD)

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
