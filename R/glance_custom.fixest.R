library(fixest)
# Fixest add mean at bottom ---------------------
glance_custom.fixest <- function(x, ...) {
  out <- data.frame("Mean(DV)" = as.numeric(fitstat(x, type = "my")))
  return(out)
}
