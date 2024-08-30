get_intervals <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year,
                           curve = intcal20) {

 continuous_intervals <- shortest_cont_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)
 interval_segments <- shortest_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)
 central_interval <- central_int(radiocarbon_age = radiocarbon_age, error = error,
                                           interval_prob = interval_prob, algorithm = algorithm,
                                           curve = curve)

 library(dplyr)
 continuous_intervals <- mutate(continuous_intervals, type = "continuous")
 interval_segments <- mutate(interval_segments, type = "segment of shortest")
 central_interval <- mutate(central_interval, type = "central")

 sum_row <- data.frame(matrix(nrow = 1, ncol = 7))
 colnames(sum_row) <-  c("start", "AD_BC_start", "end", "AD_BC_end", "length", "probability", "type")

 intervals_combined <- rbind(continuous_intervals, central_interval, interval_segments, sum_row)

 intervals_combined$length[nrow(intervals_combined)] <- sum(interval_segments$length)
 intervals_combined$probability[nrow(intervals_combined)] <- sum(interval_segments$probability)
 intervals_combined$type[nrow(intervals_combined)] <- "total shortest"

 intervals_combined$probability <- round(intervals_combined$probability, 3)

 return(intervals_combined)


}
