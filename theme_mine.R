# MY THEME =====================================================================

#' This code modifies theme_minimal to create a new theme for the ggplot2 
#' tutorial. The modifications increase whitespace and apply more consistent
#' font styling.

# store default font size
text_size <- theme_minimal()$text$size
# create a label size
label_size <- text_size * 0.8
# specify even smaller text for footnote
ftnt_size <- text_size * 0.7

theme_mine <- function() {
  theme_minimal() +
    theme(
      plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
      plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
      legend.title=element_text(face="bold", size=label_size),
      axis.title.x=element_text(size=label_size, margin=margin(t=10)),
      axis.title.y=element_text(size=label_size, margin=margin(r=10)),
      plot.caption=element_text(size=ftnt_size, hjust=0),
      plot.margin=margin(t=15, b=10, l=10, r=20),
      panel.spacing=unit(1.25, "lines"), 
      strip.text=element_text(face="bold", margin=margin(b=10)))
}

# ==============================================================================