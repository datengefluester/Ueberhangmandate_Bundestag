###############################################################################
# base theme
###############################################################################
base_theme <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75,
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0",
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0,
                                       lineheight = 0.9, margin = margin(),
                                       debug = FALSE),
      plot.margin =       margin(12,10,5,10),
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(0.75), family = '' ,
                                       face = 'bold', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B'),
      plot.subtitle =     element_text(size = rel(0.4), family = '' ,
                                       face = 'plain', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B', 
                                       margin = margin(0,0,15,0)),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(color = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}

###############################################################################
# for line charts
###############################################################################

line_chart_theme <- function(base_size = 13, base_family = "") {
  base_theme(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 7.5, color = "black"),
      axis.title.x = element_blank(),
      plot.title.position = "plot",
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = .2, color = "#656565"),
      axis.line.x = element_line(size = .3, color = "black"),
      legend.position = "right", legend.key = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(size = .3, color = "black"),
      plot.caption = element_text(size = 5, hjust = 1),
      axis.text.y = element_blank()
    )
}


###############################################################################
# for maps
###############################################################################

map_theme <- function(base_size = 13, base_family = "") {
  base_theme(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = rel(0.4), 
                                color = "black",
                                margin = unit(c(-1, 0, -1, 0), "line"),
                                hjust = 0),
      legend.margin = margin(t = -1, b = 0.5, unit = "line"),
      plot.caption = element_text(size = rel(0.2),
                                  hjust = 1),
      legend.key = element_blank(),
      legend.key.size = unit(0.25, "line"),
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.2), color = "black")
    )
}
