require(data.table)
# create Cartesian product of two data.tables----------------
CJ.dt <- function(X, Y) {
  stopifnot(is.data.table(X), is.data.table(Y))

  if (nrow(X) > 0 & nrow(Y) > 0) {
    k <- NULL
    X <- X[, c(k = 1, .SD)]
    setkey(X, k)
    Y <- Y[, c(k = 1, .SD)]
    setkey(Y, NULL)
    return(X[Y, allow.cartesian = T][, k := NULL][])
  } else {
    duplicatedNames <- names(Y)[names(Y) %in% names(X)]
    if (length(duplicatedNames) > 0) {
      setnames(Y, duplicatedNames, paste0("i.", duplicatedNames))
    }
    setkey(Y)
    setkey(X)
    return(cbind(X[!X], Y[!Y]))
  }
}
