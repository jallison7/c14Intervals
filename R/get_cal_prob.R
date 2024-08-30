get_cal_prob <- function(radiocarbon_age, error, length = 10,
                         algorithm = calibrate_one_year, curve = intcal20) {
  cal_prob <- algorithm(radiocarbon_age = radiocarbon_age, error = error, length = length,
                        curve = curve)
  return(cal_prob)

}
