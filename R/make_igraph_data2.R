require(tidyverse)
require(data.table)
require(igraph)

# igraph data function (less memory intensive)-----------------------
make_igraph_data2 <- function(data, node.var, edge.var, weight.var,
                              directed = FALSE, vertices = NULL) {

  data <- as.data.table(data)
  setnames(data, c(node.var, edge.var, weight.var), c("node", "edge", "weight_var"))

  # create cartesian product of all combinations (nr. hcpcs * nr. npis^2)
  keep <- data[, .(aux = lenuniq(node)), .(edge)] %>%
    .[aux != 1, unique(edge)]
  data <- data[edge %in% keep & weight_var != 0]

  auxdta <- foreach(x = seq_along(keep), .combine = rbind) %dopar% {
    test <- CJ(
      node.x = data[edge == keep[x], node],
      node.y = data[edge == keep[x], node]
    )
    test[, node.x := fifelse(as.integer(node.x) > as.integer(node.y), node.x, node.y)]
    test[, node.y := fifelse(as.integer(node.x) > as.integer(node.y), node.y, node.x)]

    test <- unique(test[node.x != node.y], by = c("node.x", "node.y"))
    test
  }

  auxdta <- auxdta[, .(weight = .N), .(node.x, node.y)]

  npi_net_dta <- graph_from_data_frame(auxdta, directed = directed)
  rm(auxdta, keep, data)
  gc()

  return(npi_net_dta)
}
