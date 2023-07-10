require(plotly)

# plotly layout function ----------------
ly_gg <- function(object, ttxt = "", sttxt = "", ltxt = "") {
  aux <- plotly::layout(object,
                        legend = list(title = list(text = as.character(ltxt))),
                        title = list(text = paste0(
                          as.character(ttxt),
                          "<br>",
                          "<sup>",
                          as.character(sttxt),
                          "</sup>"
                        ))
  )
  return(aux)
}
