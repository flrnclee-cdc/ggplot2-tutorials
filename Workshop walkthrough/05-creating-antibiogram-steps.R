# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating an antibiogram

# SET-UP =======================================================================

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 

# READ IN DATA =================================================================

bac_dat <- read.csv(".\\Datasets\\antibiogram-samp.csv")

# view summary
str(bac_dat)

# CREATE ANTIBIOGRAM HEAT MAP ==================================================

#' our heat map will have bacteria in the rows, antibiotics in the columns, and
#' percent susceptibility in the cells
#' 
#' we will apply color coding to highlight areas of low susceptibility and flag
#' cells where there were fewer than 30 isolates
 

# Assign data to aesthetics ----------------------------------------------------

ggplot(bac_dat, aes(x = ab_name, y = org_name, fill = pct_susc))

# Assign geometry and customize ------------------------------------------------

ggplot(bac_dat, aes(x = ab_name, y = org_name, fill = pct_susc)) +
  geom_tile(color = "white", linewidth = 0.8)

# customize colors

bac_dat <- bac_dat %>%
  mutate(susc_bin = cut(
      pct_susc,
      breaks = c(-Inf, 40, 60, 70, 80, 90, Inf),
      labels = c("<40", "40-60", "60-70", "70-80", "80-90", "90-100"),
      right = FALSE))

ggplot(bac_dat, aes(x = ab_name, y = org_name, fill = susc_bin)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(
    values = c("<40" = "#dc2626", "40-60" = "#fb923c", "60-70" = "#fde047",
      "70-80" = "#bef264", "80-90" = "#4ade80", "90-100" = "#15803d"),
    na.value = "grey90",
    name = "% susceptible"
  )

# Add and modify scales --------------------------------------------------------

#' everything looks ok with the scales

# Add annotations --------------------------------------------------------------

ctitle <- "Hospital Antibiogram, Cumulative Percent Susceptible Jan–Dec 2025"
csubtitle <- "\\* indicates samples with <30 isolates. Interpret with caution (CLSI)."

ggplot(bac_dat, aes(x = ab_name, y = org_name, fill = susc_bin)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(
    values = c("<40" = "#dc2626", "40-60" = "#fb923c", "60-70" = "#fde047",
               "70-80" = "#bef264", "80-90" = "#4ade80", "90-100" = "#15803d"),
    na.value = "grey90",
    name = "% susceptible"
  ) +
  geom_text(aes(label = ifelse(is.na(pct_susc), "—", 
                               paste0(pct_susc, ifelse(low_n, "*", "")))),
    color = ifelse(is.na(bac_dat$pct_susc), "grey50",
                   ifelse(bac_dat$pct_susc >= 90 | bac_dat$pct_susc < 40, "white", "grey20")), 
    fontface = "bold") +
  labs(title = ctitle, subtitle=csubtitle, x = NULL, y = NULL)
  
# Apply theme ------------------------------------------------------------------

source("theme_mine.R")

ctitle <- "Hospital Antibiogram, Cumulative Percent Susceptible Jan–Dec 2025"
csubtitle <- "\\* indicates samples with <30 isolates. Interpret with caution (CLSI)."

ggplot(bac_dat, aes(x = ab_name, y = org_name, fill = susc_bin)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(
    values = c("<40" = "#dc2626", "40-60" = "#fb923c", "60-70" = "#fde047",
               "70-80" = "#bef264", "80-90" = "#4ade80", "90-100" = "#15803d"),
    na.value = "grey90",
    name = "% susceptible"
  ) +
  geom_text_mine(aes(label = ifelse(is.na(pct_susc), "—", 
                               paste0(pct_susc, ifelse(low_n, "*", "")))),
            color = ifelse(is.na(bac_dat$pct_susc), "grey50",
                           ifelse(bac_dat$pct_susc >= 90 | bac_dat$pct_susc < 40, "white", "grey20")), 
            size=anno_size/.pt, fontface = "bold") +
  labs(title = ctitle, subtitle=csubtitle, x = NULL, y = NULL) +
  theme_mine() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank())

# Save plot --------------------------------------------------------------------

ggsave(
  filename="antibiogram.png",
  path=".\\Charts",
  width=10,height=7,
  dpi="print" #save as print quality
)
