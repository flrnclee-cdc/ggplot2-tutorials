# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating epi curve 

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

covid_dat <- read.csv(".\\Datasets\\covid-cases.csv")

# view summary
str(covid_dat)

# reformat date
covid_dat$onset_dt <- as.Date(covid_dat$onset_dt, "%m/%d/%Y")

str(covid_dat)

# CREATE EPI CURVE =============================================================

#' set boundary of first bin to be the sunday corresponding to the first date,
#' where first date is onset_dt_min
#' 
#' weeks are defined as sunday to saturday

onset_dt_min <- min(covid_dat$onset_dt)
onset_dt_min_wk <- floor_date(onset_dt_min, unit="week", week_start=7)

# optional code to check what the bin cut-offs are:
# 
# # without boundary
# a <- ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) +
#   geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk))
# 
# bin_data <- ggplot_build(a)[[1]]
# 
# bin_cutoffs <- bin_data %>%
#   as.data.frame() %>%
#   mutate(
#     xmin_date = as.Date(xmin, origin = "1970-01-01"),
#     xmax_date = as.Date(xmax, origin = "1970-01-01")
#   ) %>%
#   select(xmin_date, xmax_date, group, count) %>%
#   arrange(xmin_date) %>%
#   distinct()
# 
# bin_cutoffs

# bins are defined as [xmin_date, xmax_date)

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ctitle_wrap <- str_wrap(ctitle, width=65)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  # geometry --
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  # scale -- 
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  # annotations --
  labs(title=ctitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  
  guides(fill=guide_legend(title="Current status", position="top")) +
  # theme --
  theme_mine()

# SAVE CHART -------------------------------------------------------------------

ggsave(
  filename="covid-epi-curve.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
