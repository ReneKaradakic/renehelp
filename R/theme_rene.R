require(ggplot2)

theme_rene <- function(base_size = x) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0, 0, 5, 0), hjust = 0),
      plot.subtitle = element_text(size = rel(0.9), margin = margin(0, 0, 5, 0), hjust = 0),
      plot.caption = element_text(size = rel(0.6), hjust = 1, margin = margin(0, 0, 5, 0)),
      # The axis
      axis.title = element_text(size = rel(0.85), face = "italic", hjust = 1),
      axis.text = element_text(size = rel(0.75)),
      # The Legend
      legend.title = element_text(size = rel(0.85)),
      legend.text = element_text(size = rel(0.70)),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Facetting
      strip.text = element_text(size = rel(0.85), hjust = 0, margin = margin(5, 0, 5, 0)),
      # Background
      panel.background = element_blank(),
      # Grid
      panel.grid.minor = element_blank()
    )
}
