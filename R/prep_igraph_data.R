require(data.table)
require(tidyverse)
require(doMC)
require(foreach)
require(fastverse)


# Prepare data for igraph format --------------------------
prep_igraph_data <- function(inpdta, node.var = "npi", edge.var = "hcpcs_cd",
                             edge.weight = "tot_revenue") {
  pacman::p_load(data.table, collapse, tidyverse, foreach, doMC)

  inpdta <- as.data.table(inpdta)
  setnames(inpdta,
           c(node.var, edge.var, edge.weight),
           c("node", "edge", "edge.weight"))

  # create cartesian product of all combinations (nr. hcpcs * nr. npis^2)
  keep <- inpdta[, list(aux = length(unique(node))), list(edge)] %>%
    .[aux != 1, unique(edge)]
  inpdta <- inpdta[edge %in% keep & edge.weight != 0]

  auxdta <- foreach(x = seq_along(keep), .combine = rbind) %dopar% {
    test <- CJ(
      node.x = inpdta[edge == keep[x], node],
      node.y = inpdta[edge == keep[x], node]
    )
    test[, node.x := fifelse(as.integer(node.x) > as.integer(node.y), node.x, node.y)]
    test[, node.y := fifelse(as.integer(node.x) > as.integer(node.y), node.y, node.x)]
    test[, hcpcs_cd := keep[x]]

    test <- unique(test[node.x != node.y], by = c("node.x", "node.y"))
    test
  }

  auxdta <- merge(auxdta, inpdta[, .(node, edge, edge.weight)],
                  by.x = c("node.x", "hcpcs_cd"),
                  by.y = c("node", "edge"), all.x = TRUE
  ) %>% merge(
    inpdta[, .(node, edge, edge.weight)],
    by.x = c("node.y", "hcpcs_cd"),
    by.y = c("node", "edge"), all.x = TRUE
  )

  auxdta[, edge_shared := pmin(edge.weight.x, edge.weight.y)]
  auxdta[, aux.x := as.integer(node.x)][, aux.y := as.integer(node.y)]
  auxdta[, id := fifelse(
    aux.x < aux.y,
    paste0(node.x, "_", node.y),
    paste0(node.y, "_", node.x)
  )][, c("aux.x", "aux.y") := NULL]

  auxdta <- unique(auxdta, by = c("id", "hcpcs_cd"))

  return(auxdta)
}
