# MY THEME =====================================================================

#' This code modifies theme_minimal to create a new theme for the ggplot2 
#' tutorial. The modifications increase whitespace and apply fonts more 
#' closely aligned to the CDC brand.

# Install fonts from Google Fonts ----------------------------------------------

library(showtext)
font_add_google("Nunito Sans", "nunito sans")
showtext_auto()
showtext_opts(dpi = 300)

# Customize --------------------------------------------------------------------

# store default font size
body_size <- theme_minimal()$text$size * 0.85

# specify h1 title size (125%)
title_size <- body_size*1.25
# specify h2 title size (115%)
subtitle_size <- body_size*1.15
# specify anno title size (95%)
anno_size <- body_size*0.95
# specify smaller text for footnote (80%)
ftnt_size <- body_size * 0.8


theme_mine <- function() {
  theme_minimal() +
    theme(
      text=element_text(family="nunito sans", size=body_size),
      # Plot titles ------------------------------------------------------------
      plot.title=element_markdown(face="bold", lineheight=1.1, size=title_size),
      plot.subtitle=element_markdown(margin=margin(b=10), lineheight=1.1, size=subtitle_size),
      # Legend -----------------------------------------------------------------
      legend.title=element_text(face="bold", size=body_size),
      legend.text=element_text(size=body_size),
      legend.box.spacing=unit(1, "pt"),
      # Panels -----------------------------------------------------------------
      strip.text=element_text(face="bold", margin=margin(b=10), size=body_size),
      panel.spacing=unit(1.25, "lines"),
      # Axes -------------------------------------------------------------------
      axis.text=element_text(size=body_size),
      axis.title.x=element_text(size=body_size, margin=margin(t=10, b=10)),
      axis.title.y=element_text(size=body_size, margin=margin(r=10)),
      # Footnote ---------------------------------------------------------------
      plot.caption=element_text(size=ftnt_size, hjust=0),
      # Margins/spacing --------------------------------------------------------
      plot.margin=margin(t=15, b=10, l=10, r=20))
}

geom_text_mine <- function(...) {
  geom_text(
    family = "nunito sans",
    ...
  )
}

geom_richtext_mine <- function(...) {
  geom_richtext(
    family = "nunito sans",
    ...
  )
}

# ==============================================================================
