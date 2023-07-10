require(data.table)
require(broom)
# broom::tidy as.data.table()
tidy.dt <- function(x) {
  pacman::p_load(data.table, broom, tidyverse)

  x <- tidy(x, conf.int = T) %>%
    as.data.table()

  return(x)
}
