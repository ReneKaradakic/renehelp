# required packages
require(fastverse, tidyverse, stringdist)

# fuzzy matching function old --------------------
match_fuzzy <- function(indta_a, indta_b, by_a, by_b, unique_id_a, unique_id_b, accuracy, relative) {


  data_a <- indta_a
  data_b <- indta_b

  # prepare column names
  co_a <- c(unique_id_a, by_a)
  auxc_a <- paste0("var", c(1:(length(by_a) - 1)), "_a")
  cn_a <- c("id_a", "name_a", auxc_a)
  co_b <- c(unique_id_b, by_b)
  auxc_b <- paste0("var", c(1:(length(by_a) - 1)), "_b")
  cn_b <- c("id_b", "name_b", auxc_b)

  if (length(by_a) < 2) {
    setnames(data_a, c(by_a, unique_id_a), c("name_a", "id_a"))
    setnames(data_b, c(by_b, unique_id_b), c("name_b", "id_b"))

    cn_a <- c("id_a", "name_a")
    cn_b <- c("id_b", "name_b")
  } else {
    setnames(data_a, co_a, cn_a)
    setnames(data_b, co_b, cn_b)
  }

  # create Cartesian product by columns
  cols <- c(cn_a, cn_b[1:2])

  if (length(by_a) < 2) {
    aux <- CJ.dt(data_a[, ..cn_a], data_b[, ..cn_b])
  } else {
    aux <- merge(data_a, data_b,
                 by.x = cn_a[3:length(cn_a)],
                 by.y = cn_b[3:length(cn_b)],
                 allow.cartesian = TRUE
    ) %>%
      .[, ..cols]
  }

  # calculate string distances and filter
  aux[, dist := 1 - stringdist(name_a, name_b, method = "jw")]
  aux <- aux[order(name_a, -dist)]
  aux[, ldist := data.table::shift(dist, type = "lead"), .(name_a)][, rel := round(dist / ldist, 1)]
  aux <- aux[dist >= accuracy & rel >= relative]
  aux[, rank_a := frank(-dist), .(name_a)][, rank_b := frank(-dist), .(name_b)]

  # only keep if both first rank and unique
  aux <- aux[rank_a == 1 & rank_b == 1] %>%
    unique(by = c("name_a", "name_b")) %>%
    .[, .(name_a, name_b, id_a, id_b, dist)] %>%
    setnames(c("id_a", "id_b"), c(unique_id_a, unique_id_b))


  if (length(by_a) < 2) {
    setnames(data_a, c("name_a", "id_a"), c(by_a, unique_id_a))
    setnames(data_b, c("name_b", "id_b"), c(by_b, unique_id_b))
  } else {
    setnames(data_a, cn_a, co_a)
    setnames(data_b, cn_b, co_b)
  }
  rm(data_a, data_b)

  return(aux)
}
