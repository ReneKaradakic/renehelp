exvarnam <- function(data, string) {
  aux <- grep(string, names(data), value = TRUE)
  return(aux)
}
