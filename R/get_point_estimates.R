get_point_estimates <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year,
                                curve = intcal20) {
  intervals <- get_intervals(radiocarbon_age = radiocarbon_age, error = error,
                             interval_prob = interval_prob, algorithm = algorithm,
                             curve = curve)


  point_estimates <- as.data.frame(matrix(nrow = 3, ncol = 4))
  colnames(point_estimates) <- c("date", "AD_BC", "type", "interval_probability")

  library(dplyr)



  continuous <- filter(intervals, type == "continuous")
  point_estimates$interval_probability[1] <- interval_prob
  point_estimates$type[1] <- "midpoint of shortest continuous interval"
  if (continuous$AD_BC_start == continuous$AD_BC_end) {
    point_estimates$date[1] <- (continuous$start + continuous$end) / 2
    point_estimates$AD_BC[1] <- continuous$AD_BC_start
  }
  else {
    point_estimates$date[1] <- continuous$start - (continuous$length / 2)
    point_estimates$AD_BC[1] <- continuous$AD_BC_start
    if (point_estimates$date[1] < 0) {
      point_estimates$date[1] <- abs(point_estimates$date[1])
      point_estimates$AD_BC[1] <- continuous$AD_BC_end
    }
  }

  central <- filter(intervals, type == "central")
  point_estimates$interval_probability[2] <- interval_prob
  point_estimates$type[2] <- "midpoint of central interval"
  if (central$AD_BC_start == central$AD_BC_end) {
    point_estimates$date[2] <- (central$start + central$end) / 2
  point_estimates$AD_BC[2] <- central$AD_BC_start
  }
  else {
    point_estimates$date[2] <- central$start - (central$length / 2)
    point_estimates$AD_BC[2] <- central$AD_BC_start
    if (point_estimates$date[2] < 0) {
      point_estimates$date[2] <- abs(point_estimates$date[2])
      point_estimates$AD_BC[2] <- central$AD_BC_end
    }
  }

  shortest  <- filter(intervals, type == "segment of shortest")
  shortest <- filter(shortest, probability == max(probability))
  point_estimates$interval_probability[3] <- interval_prob
  point_estimates$type[3] <- "midpoint of segment of shortest interval with highest probability"
  if (shortest$AD_BC_start == shortest$AD_BC_end) {
    point_estimates$date[3] <- (shortest$start + shortest$end) / 2
    point_estimates$AD_BC[3] <- shortest$AD_BC_start
  }
  else {
    point_estimates$date[3] <- shortest$start - (shortest$length / 2)
    point_estimates$AD_BC[3] <- shortest$AD_BC_start
    if (point_estimates$date[3] < 0) {
      point_estimates$date[3] <- abs(point_estimates$date[3])
      point_estimates$AD_BC[3] <- shortest$AD_BC_end
    }
  }

return(point_estimates)

}
