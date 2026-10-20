# MY THEME =====================================================================

#' This code modifies theme_minimal to create a new theme for the ggplot2 
#' tutorial. The modifications increase whitespace and apply more consistent
#' font styling.

# Install fonts from Google Fonts ----------------------------------------------

library(showtext)
font_add_google("Nunito Sans", "nunito sans")
showtext_auto()

# Customize --------------------------------------------------------------------

# store default font size
text_size <- theme_minimal()$text$size * 2.0
# specify h1 title size
title_size <- text_size*1.2
# specify h2 title size
subtitle_size <- text_size*1.1
# specify label size
label_size <- text_size
# specify smaller text for footnote (70%)
ftnt_size <- text_size * 0.7

theme_mine <- function() {
  theme_minimal() +
    theme(
      text=element_text(family="nunito sans", size=text_size),
      # Plot titles
      plot.title=element_markdown(face="bold", lineheight=1.1, size=title_size),
      plot.subtitle=element_markdown(margin=margin(b=5), lineheight=1.1, size=subtitle_size),
      # Legend
      legend.title=element_text(face="bold", size=label_size),
      legend.box.spacing=unit(1, "pt"),
      strip.text=element_text(face="bold", margin=margin(b=10)),
      # Axes 
      axis.title.x=element_text(size=label_size, margin=margin(t=10, b=10)),
      axis.title.y=element_text(size=label_size, margin=margin(r=10)),
      # Footnote
      plot.caption=element_text(size=ftnt_size, hjust=0),
      # Margins/spacing
      plot.margin=margin(t=15, b=10, l=10, r=20),
      panel.spacing=unit(1.25, "lines"))
}

# ==============================================================================