# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating charts that show (pie charts) and compare
#' (100% stacked bar charts) distributions

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

str(tbi_dat_age)

# set age and stay_length as ordinal variables
tbi_dat_age$age <- factor(tbi_dat_age$age, levels=c("0-4", "5-14", "15-19",
                                                    "20-24", "25-34", "35-44",
                                                    "45-64", "65+"))

tbi_dat_age$stay_length <- factor(tbi_dat_age$stay_length, 
                                  levels=c("0-1 days", "2-6 days", "7+ days"))

str(tbi_dat_age)

# CREATE 100% STACKED BAR CHART ================================================

# add embedded formatting to title

ctitle <- "Among people with a traumatic brain injury-related discharge, the percentage who stay in the hospital for 7 or more days increases with age."
csubtitle <- "Among people aged 65 and over, 35.7% of TBI-related discharges lasted 7 or more days compared with 11.9% for children aged 0-4."
ftnt <- "Data source: Langlois JA, Kegler SR, Butler JA, Gotsch KE, Johnson RL, Reichard AA, Webb KW, Coronado VG, Selassie AW, Thurman DJ. Traumatic brain injury-related hospital discharges. Results from a 14-state surveillance system, 1997. MMWR Surveill Summ. 2003 Jun 27;52(4):1-20. PMID: 12836629."

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=75)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)
ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

ctitle_wrap_fmt <- gsub("7 or more days", "<span style='color: #DE5A18;'>7 or more days</span>", 
                        ctitle_wrap)

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # geometry--
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text_mine(aes(label=paste0(percent, "%"), colour=stay_length), 
                 size=anno_size/.pt, position=position_stack(vjust=0.5)) +
  # scales--
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # titles--
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # legend--
  guides(fill=guide_legend(title="Stay length", position="top") ,
       colour="none") +
  # coordinate--
  coord_cartesian() +
  # theme-- 
  theme_mine()

# Save chart -------------------------------------------------------------------

ggsave(
  filename="tbi_stays_bar.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
