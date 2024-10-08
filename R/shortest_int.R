#function to find the shortest possible (often discontinuous) interval
shortest_int <- function(radiocarbon_age, error, interval_prob = .95, algorithm = calibrate_one_year, curve = intcal20) {
  calibrated_distribution <- algorithm(radiocarbon_age = radiocarbon_age, error = error, curve = curve)
  #check the spacing of the years in the dataframe (oxcal is 5 years, others depend on the curve spacing)
  res_test <- vector(mode = "numeric", length = nrow(calibrated_distribution) - 1)
  for (i in 1:nrow(calibrated_distribution) - 1) {
    res_test[i] <- calibrated_distribution$cal_bp[i] - calibrated_distribution$cal_bp[i + 1]
  }
  resolution <- max(res_test)

  #finding the resolution like this could go wrong if the spacing of the probabilities
  # is inconsistent -- have to fix that

  sort_df <- calibrated_distribution


  sort_df <- arrange(sort_df, desc(probability))
  prob_sum <- 0
  j <- 1
  while (prob_sum <= interval_prob) {
    prob_sum = prob_sum + sort_df$prob[j]
    j <- j + 1
  }

  in_interval <- sort_df[1:j,]
  in_interval <- arrange(in_interval, cal_AD)



  count_segments <- 1
  for (i in 2:nrow(in_interval)) {
    if (in_interval$cal_bp[i-1] - in_interval$cal_bp[i]  > resolution) {
      count_segments <- count_segments + 1
    }
  }



  # create a data frame to hold the segments of the interval and their associated probability
  interval_segments <- data.frame(matrix(nrow = count_segments, ncol = 11))
  colnames(interval_segments) <- c("radiocarbon_age", "error", "nominal_prob", "type", "start",
                                   "AD_BC_start", "end", "AD_BC_end", "length", "total_range",
                                   "probability")

  p <- in_interval$prob[1]
  segment <- 1

  interval_segments$start[1] <- in_interval$cal_AD[1]
  for (i in 2:nrow(in_interval)) {
    if (in_interval$cal_bp[i-1] - in_interval$cal_bp[i]  > resolution) {
      interval_segments$end[segment] <- in_interval$cal_AD[i-1]
      interval_segments$probability[segment] <- p
      segment <- segment + 1
      interval_segments$start[segment] <- in_interval$cal_AD[i]
      p <- 0
    }
    p <- p + in_interval$prob[i]
  }
  interval_segments$end[count_segments] <- in_interval$cal_AD[nrow(in_interval)]
  interval_segments$probability[segment] <- p


  for (i in 1:count_segments) {
    if(interval_segments$start[i] > 0) {
      interval_segments$AD_BC_start[i] <- "AD"
    }
    else {
      interval_segments$AD_BC_start[i] <- "BC"
      interval_segments$start[i] <- abs(interval_segments$start[i])
    }
    if(interval_segments$end[i] > 0) {
      interval_segments$AD_BC_end[i] <- "AD"
    }
    else {
      interval_segments$AD_BC_end[i] <- "BC"
      interval_segments$end[i] <- abs(interval_segments$end[i])
    }
    if (interval_segments$AD_BC_start[i] == interval_segments$AD_BC_end[i]) {
      seg_length <- abs(interval_segments$end[i] - interval_segments$start[i]) + 1
    }
    else {
      seg_length <- abs(interval_segments$start[i] - 0) + (interval_segments$end[i] - 0)
    }
    interval_segments$length[i] <- seg_length
  }


  interval_segments$type <- ("segment of shortest")

  #creates a line of data for the total, combined, interval segments
  sum_row <- data.frame(matrix(nrow = 1, ncol = 11))
  colnames(sum_row) <-  c("radiocarbon_age", "error", "nominal_prob", "type", "start",
                          "AD_BC_start", "end", "AD_BC_end", "length", "total_range",
                          "probability")


  sum_row$length <- sum(interval_segments$length)
  sum_row$probability <- sum(interval_segments$probability)
  sum_row$type <- "total shortest"
  sum_row$start <- interval_segments$start[1]
  sum_row$AD_BC_start <- interval_segments$AD_BC_start[1]
  sum_row$end <- interval_segments$end[nrow(interval_segments)]
  sum_row$AD_BC_end <- interval_segments$AD_BC_end[nrow(interval_segments)]


  interval_segments <- rbind(interval_segments, sum_row)

  interval_segments$radiocarbon_age <- radiocarbon_age
  interval_segments$error <- error
  interval_segments$nominal_prob <- interval_prob



  return(interval_segments)
}
