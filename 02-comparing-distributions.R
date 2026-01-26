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

# CREATE PIE CHART =============================================================

#' ggplot2 does not have a slice geometry 
#'
#' to create a pie chart, you create a bar chart with one bar and use a
#' polar coordinate system to alter the geometry

# Assign data to aesthetics ----------------------------------------------------

base <- ggplot(data=tbi_dat_total, 
               aes(x=age, y=percent, fill=stay_length))

base

# Assign geometry --------------------------------------------------------------

# use stat="identity" to direct ggplot to use data as-is
base +
  geom_bar(stat="identity")

# Customize colors

base +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                             "7+ days"="#A6A6A6"))

# Add outline
base +
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                             "7+ days"="#A6A6A6"))

# Add and modify scales --------------------------------------------------------

base +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                             "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL)

# Add annotations --------------------------------------------------------------

# Add chart title

#' since chart titles are long, you can store them in variables

ctitle <- "Most people (44.5%) with a traumatic brain injury-related discharge stayed in the hospital for 2 to 6 days."

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                             "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle)

#' the title bleeds out of chart space, but we will deal with this later

# Add footnote

ftnt <- "Data source: Langlois JA, Kegler SR, Butler JA, Gotsch KE, Johnson RL, Reichard AA, Webb KW, Coronado VG, Selassie AW, Thurman DJ. Traumatic brain injury-related hospital discharges. Results from a 14-state surveillance system, 1997. MMWR Surveill Summ. 2003 Jun 27;52(4):1-20. PMID: 12836629."

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt)

# Modify axis labels

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  # remove x-axis because group is all ages
  scale_x_discrete(labels=NULL) +
  # remove y-axis because we will directly label
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="")

# Modify legend

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top"))

# Add data label

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%")), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill=guide_legend(title="Stay length", position="top")) 
  
#' we can add more detailed data labels to remove legend

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length)), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none")

#' for groups with dark colors, use white text

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length),
                colour=stay_length), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") 
  
# Apply theme ------------------------------------------------------------------

# use theme created in example 01

source("theme_mine.R")

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length),
                colour=stay_length), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#000000", "2-6 days"="#FFFFFF",
                               "7+ days"="#FFFFFF")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # THEME ---------------------------------------------------------------
  theme_mine()

# Define coordinate system -----------------------------------------------------

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length),
                colour=stay_length), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#404040", "2-6 days"="#FFFFFF",
                               "7+ days"="#404040")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle, caption=ftnt) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_polar(theta="y", start=0, direction=-1) +
  # THEME ---------------------------------------------------------------
  theme_mine()
  
#' theta takes in the variable that you want to map angle to, either x or y
#' set direction=-1 to start re-sort slices

# Fix text wrapping: Title, subtitle, footnote ---------------------------------

#' str_wrap() helps wrap strings based on a specified character width
#' examine export to determine where break needs to be
#' may need to play around with this a bit

ctitle_wrap <- str_wrap(ctitle, width=50)
ftnt_wrap <- str_wrap(ftnt, width=80, whitespace_only = FALSE)
ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length),
                colour=stay_length), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#404040", "2-6 days"="#FFFFFF",
                               "7+ days"="#404040")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_polar(theta="y", start=0, direction=-1) +
  # THEME ---------------------------------------------------------------
  theme_mine() + 
  theme(
    panel.grid=element_blank())

# Minor design adjustments -----------------------------------------------------

# add color coding to title

ctitle_wrap_fmt <- gsub("2 to 6 days", "<span style='color: #0D86B7;'>2 to 6 days</span>", 
                        ctitle_wrap)

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_text(aes(label=paste0(round(percent, 1), "%\nstayed ", stay_length),
                colour=stay_length), 
            position=position_stack(vjust=0.5)) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#404040", "2-6 days"="#FFFFFF",
                               "7+ days"="#404040")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_polar(theta="y", start=0, direction=-1) +
  # THEME ---------------------------------------------------------------
  theme_mine() + 
  theme(
    panel.grid=element_blank())

# add more formatting to data labels

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_richtext(aes(label=paste0("<span style='font-size: 13pt'><b>", round(percent, 1), 
                                 "%</b><br></span><span style='font-size: 9pt'>stayed ", 
                                 stay_length, "</span>"),
                colour=stay_length), 
            position=position_stack(vjust=0.5), fill=NA, label.color=NA) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_colour_manual(values=c("0-1 days"="#404040", "2-6 days"="#FFFFFF",
                               "7+ days"="#404040")) + 
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_polar(theta="y", start=0, direction=-1) +
  # THEME ---------------------------------------------------------------
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

base <- ggplot(data=tbi_dat_total, 
               aes(x=age_val, y=percent, fill=stay_length))

donut_anno <- tbi_dat_total %>%
  filter(percent==max(percent)) %>%
  mutate(x=-4, y=0)

base +
  # GEOMETRY ------------------------------------------------------------
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1) +
  geom_richtext(data=donut_anno, aes(x=x, y=y, label=paste0("<span style='font-size: 18pt'><b>", round(percent, 1), 
                                 "%</b><br></span><span style='font-size: 9pt'>stayed ", 
                                 stay_length, "</span>")), fill=NA, label.color=NA) +
  # SCALES --------------------------------------------------------------
  scale_fill_manual(values=c("0-1 days"="#D9D9D9", "2-6 days"="#0D86B7",
                           "7+ days"="#A6A6A6")) +
  scale_x_continuous(labels=NULL, limits=c(-4, 0.4)) +
  scale_y_continuous(labels=NULL) +
  # TITLES --------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="") + 
  ylab(label="") +
  # LEGEND --------------------------------------------------------------
  guides(fill="none", colour="none") +
  # ANNOTATION ----------------------------------------------------------
  annotate("text", x=0, y=0, size=9/.pt, label="7+ days", fontface="bold",
           hjust=1.5, color="#A6A6A6") + 
  annotate("text", x=0, y=0, size=9/.pt, label="0-1 days", fontface="bold",
           hjust=-0.5, color="#A6A6A6") + 
  # COORDINATE SYSTEM ---------------------------------------------------
  coord_polar(theta="y", start=0, direction=-1) +
  # THEME ---------------------------------------------------------------
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

# CREATE 100% STACKED BAR CHART ================================================

# Assign data to aesthetics ----------------------------------------------------

ggplot(data=tbi_dat_age, 
       aes(x=age, y=percent, fill=stay_length))

# resort ordinal data by turning into factor
tbi_dat_age$age <- factor(tbi_dat_age$age, levels=c("0-4", "5-14", "15-19",
                                                    "20-24", "25-34", "35-44",
                                                    "45-64", "65+"))

tbi_dat_age$stay_length <- factor(tbi_dat_age$stay_length, 
                                  levels=c("0-1 days", "2-6 days", "7+ days"))

# recreate base
ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length))

# Assign geometry --------------------------------------------------------------

ggplot(data=tbi_dat_age, aes(x=age, y=percent, fill=stay_length)) +
  geom_bar(stat="identity", colour="#FFFFFF", linewidth=1)

# resort stay length so 7+ is at the top
tbi_dat_age$stay_length <- factor(tbi_dat_age$stay_length, levels=c("7+ days",
                                                                    "2-6 days",
                                                                    "0-1 days"))
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
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), 
            position=position_stack(vjust=0.5)) +
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
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), 
            size=label_size/.pt, position=position_stack(vjust=0.5)) +
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
  geom_text(aes(label=paste0(percent, "%"), colour=stay_length), 
            size=label_size/.pt, position=position_stack(vjust=0.5)) +
  # SCALES -------------------------------------------------------------
  scale_fill_manual(values=c("0-
                             1 days"="#96DCF8", "2-6 days"="#0D86B7",
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
