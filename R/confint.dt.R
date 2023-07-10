# Confidence inteval ---------------
confint.dt <- function(est, se) {
  conf.low <- est - 1.96 * se
  conf.high <- est + 1.96 * se

  aux <- cbind(conf.low, conf.high)
  return(aux)
}
