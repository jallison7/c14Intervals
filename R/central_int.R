central_int <- function(radiocarbon_age, error, interval_prob = .95,
                        algorithm = calibrate_one_year, curve = intcal20) {
  calibrated_distribution <- algorithm(radiocarbon_age = radiocarbon_age, error = error,
                        curve = curve)
  tail_prob <- (1 - interval_prob) / 2

  central_interval <- data.frame(matrix(nrow = 1, ncol = 6))
  colnames(central_interval) <-  c("start", "AD_BC_start", "end", "AD_BC_end", "length", "probability")

  i <- 1
  while (calibrated_distribution$cumul_probability[i] <= tail_prob) {
       i <- i + 1
  }

  j <- nrow(calibrated_distribution)
  while (calibrated_distribution$cumul_probability[j] >= (1 - tail_prob)) {
        j <- j - 1
  }

  central_interval$start <- calibrated_distribution$cal_AD[i - 1]
  central_interval$end <- calibrated_distribution$cal_AD[j + 1]

  if(central_interval$start > 0) {
    central_interval$AD_BC_start <- "AD"
  }
  else {
    central_interval$AD_BC_start <- "BC"
    central_interval$start <- abs(central_interval$start)
  }
  if(central_interval$end > 0) {
    central_interval$AD_BC_end <- "AD"
  }
  else {
    central_interval$AD_BC_end <- "BC"
    central_interval$end <- abs(central_interval$end)
  }

  if (central_interval$AD_BC_start == central_interval$AD_BC_end) {
    central_interval$length <- abs(central_interval$end - central_interval$start) + 1
  }
  else {
    central_interval$length <- abs(central_interval$start) + (central_interval$end)
  }

  central_interval$probability <- calibrated_distribution$cumul_probability[j + 1] -
                                calibrated_distribution$cumul_probability[i - 1]

  return(central_interval)




}
