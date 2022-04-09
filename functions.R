

# create consistent theme for all plots
# papers used in: all
dark_theme <- function() {
  
  theme(
    panel.grid.major = element_line(color = '#99a1b3'),
    panel.grid.minor = element_line(color = 'black'),
    panel.background = element_rect(fill = 'black'),
    panel.border=element_blank(),
    plot.background=element_rect(fill = "black", color = "black"),
    axis.text.x = element_text(color = 'white'),
    axis.text.y = element_text(color = 'white'),
    plot.title = element_text(face = 'bold', color = 'white', hjust = 0.5),
    plot.subtitle = element_text(face = 'bold', color = 'white'),
    axis.title.x = element_text(color = 'white'),
    axis.title.y = element_text(color = 'white'),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )
  
}


