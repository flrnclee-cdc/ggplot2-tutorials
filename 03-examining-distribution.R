# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating charts that show (pie charts) distributions

# SET-UP =======================================================================

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 

# use my theme!
source("theme_mine.R")

# READ IN DATA =================================================================

tbi_dat <- read.csv(".\\Datasets\\tbi-severity-age.csv")

# view summary
str(tbi_dat)

# separate all data from age data
tbi_dat_total <- tbi_dat %>%
  filter(age=="All ages")

tbi_dat_age <- tbi_dat %>%
  filter(age!="All ages")

# CREATE PIE CHART =============================================================

#' ggplot2 does not have a slice geometry 
#'
#' to create a pie chart, you create a bar chart with one bar and use a
#' polar coordinate system to alter the geometry

# Assign data to aesthetics ----------------------------------------------------

ctitle <- "Most people (44.5%) with a traumatic brain injury-related discharge stayed in the hospital for 2 to 6 days."
ftnt <- "Data source: Langlois JA, Kegler SR, Butler JA, Gotsch KE, Johnson RL, Reichard AA, Webb KW, Coronado VG, Selassie AW, Thurman DJ. Traumatic brain injury-related hospital discharges. Results from a 14-state surveillance system, 1997. MMWR Surveill Summ. 2003 Jun 27;52(4):1-20. PMID: 12836629."

ctitle_wrap <- str_wrap(ctitle, width=50)
ftnt_wrap <- str_wrap(ftnt, width=80, whitespace_only = FALSE)
ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)

ctitle_wrap_fmt <- gsub("2 to 6 days", "<span style='color: #0D86B7;'>2 to 6 days</span>", 
                        ctitle_wrap)

ggplot(data=tbi_dat_total, aes(x=age, y=percent, fill=stay_length)) +
  # geometry--
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_richtext_mine(aes(label=paste0("<span style='font-size: 13pt;'><b>", round(percent, 1), 
                                 "%</b><br></span><span style='font-size: 9pt;'>stayed ", 
                                 stay_length, "</span>"),
                colour=stay_length), 
            position=position_stack(vjust=0.5), fill=NA, label.color=NA) +
  # scales--
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#404040", "2-6 days"="#FFFFFF",
                               "7+ days"="#404040")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # titles--
  labs(title=ctitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # legend--
  guides(fill="none", colour="none") +
  # coordinate--
  coord_polar(theta="y", start=0, direction=-1) +
  # theme--
  theme_mine() + 
  theme(
    panel.grid=element_blank())

# Save chart -------------------------------------------------------------------

ggsave(
  filename="tbi_stays_pie.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)


# CREATE DONUT CHART ===========================================================

tbi_dat_total <- tbi_dat_total %>%
  mutate(age_val=-1) 

donut_anno <- tbi_dat_total %>%
  filter(percent==max(percent)) %>%
  mutate(x=-4, y=0)

ggplot(data=tbi_dat_total, aes(x=age_val, y=percent, fill=stay_length)) +
  # geometry--
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_richtext_mine(data=donut_anno, aes(x=x, y=y, label=paste0("<span style='font-size: 18pt'><b>", round(percent, 1), 
                                 "%</b><br></span><span style='font-size: 9pt'>stayed ", 
                                 stay_length, "</span>")), fill=NA, label.color=NA) +
  # scales--
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_continuous(labels=NULL, limits=c(-4, 0.4)) +
  scale_y_continuous(labels=NULL) +
  # titles--
  labs(title=ctitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # legend--
  guides(fill="none", colour="none") +
  # annotation--
  annotate("text", x=0, y=0, size=9/.pt, label="7+ days", fontface="bold",
           hjust=1.5, color="#A6A6A6") + 
  annotate("text", x=0, y=0, size=9/.pt, label="0-1 days", fontface="bold",
           hjust=-0.5, color="#A6A6A6") + 
  # coordinate--
  coord_polar(theta="y", start=0, direction=-1) +
  # theme--
  theme_mine() + 
  theme(
    panel.grid=element_blank())

# Save chart -------------------------------------------------------------------

ggsave(
  filename="tbi_stays_pie-2.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
