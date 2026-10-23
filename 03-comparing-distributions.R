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

# Assign data to aesthetics ----------------------------------------------------

ggplot(data=tbi_dat_age, 
       aes(x=age, y=percent, fill=stay_length))

# Assign geometry --------------------------------------------------------------

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  geom_bar(stat="identity")

# add border

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1)

# customize colors

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                               "7+ days"="#DE5A18")) 

# Modify scales ----------------------------------------------------------------  
 
ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous()

# Add annotations ----------------------------------------------------------------  

# Add chart title and footnotes

ctitle <- "Among people with a traumatic brain injury-related discharge, the percentage who stay in the hospital for 7 or more days increases with age."
csubtitle <- "Among people aged 65 and over, 35.7% of TBI-related discharges lasted 7 or more days compared with 11.9% for children aged 0-4."
ftnt <- "Data source: Langlois JA, Kegler SR, Butler JA, Gotsch KE, Johnson RL, Reichard AA, Webb KW, Coronado VG, Selassie AW, Thurman DJ. Traumatic brain injury-related hospital discharges. Results from a 14-state surveillance system, 1997. MMWR Surveill Summ. 2003 Jun 27;52(4):1-20. PMID: 12836629."

ggplot(data=tbi_dat_age, 
       aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt)  

# Modify x-axis and y-axis labels

ggplot(data=tbi_dat_age, 
       aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges")

# Modify legend

#' clean up legend title and move to top

ggplot(data=tbi_dat_age, 
       aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top"))

# Add data labels
ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # format as 11%
  geom_text(aes(label=paste0(percent, "%"))) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top"))

#' adjust labeling position to be in bar center

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(percent, "%")), position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top"))

#' change color of labels so it's easier to read

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous() +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top"))


# remove extra legend
# remove y-axis labeling 

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top") ,
         colour="none") 

# Apply coordinate system ------------------------------------------------------
ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top") ,
       colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_cartesian()

# Apply theme ------------------------------------------------------------------

# use my theme!
source("theme_mine.R")

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text_mine(aes(label=paste0(percent, "%"), colour=stay_length), 
            position=position_stack(vjust=0.5), size=anno_size/.pt) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top") ,
       colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_cartesian() +
  # THEME --------------------------------------------------------------- 
  theme_mine()

# Minor adjustments -----------------------------------------------------------

# wrap titles and footnotes

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=75)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)
ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text_mine(aes(label=paste0(percent, "%"), colour=stay_length), 
            size=anno_size/.pt, position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top") ,
       colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_cartesian() +
  # THEME --------------------------------------------------------------- 
  theme_mine()

# add embedded formatting to title

ctitle_wrap_fmt <- gsub("7 or more days", "<span style='color: #DE5A18;'>7 or more days</span>", 
                        ctitle_wrap)

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  # GEOMETRY -----------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text_mine(aes(label=paste0(percent, "%"), colour=stay_length), 
                 size=anno_size/.pt, position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#96DCF8", "2-6 days"="#0D86B7",
                           "7+ days"="#DE5A18"))  +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete() +
  scale_y_continuous(label=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Age group in years") +
  ylab(label="Percent of TBI-related discharges") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top") ,
       colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_cartesian() +
  # THEME --------------------------------------------------------------- 
  theme_mine()

# Save chart -------------------------------------------------------------------

ggsave(
  filename="tbi_stays_bar.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
