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


# READ IN DATA =================================================================

covid_dat <- read.csv(".\\Datasets\\covid-cases.csv")

# view summary
str(covid_dat)

# reformat date
covid_dat$onset_dt <- as.Date(covid_dat$onset_dt, "%m/%d/%Y")

str(covid_dat)

# CREATE EPI CURVE =============================================================

#' There are two approaches to creating an epi curve:

#' (1) Take line-level data and use geom_histogram to have ggplot split data into
#' bins and count the number of cases in each bin

#' (2) Summarize line-level data prior to visualization and use geom_bar using
#' the data directly.

# GEOM_HISTOGRAM ---------------------------------------------------------------

# Assign data to aesthetics ----------------------------------------------------

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status))

# Assign geometry --------------------------------------------------------------

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) +
  geom_histogram()

#' By default, ggplot broke data into 30 bins. We can ask ggplot to create bins
#' that are 7-days/1 week wide by using binwidth. 

# Customize --------------------------------------------------------------------

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) +
  geom_histogram(binwidth=7)

#' It's a bit of a black box what the starting date of the bin is. For greater
#' control, I recommend using the boundary= option to set the date you want the
#' bin to start on.

onset_dt_min <- min(covid_dat$onset_dt)

# start first bin at min date
ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) +
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min))

# start first bin at sunday (sunday to saturday = one week) of onset_dt_min
onset_dt_min_wk <- floor_date(onset_dt_min, unit="week", week_start=7)
onset_dt_min_wk

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) +
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk))

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

# customize colors

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk)) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7"))

# flip bars so Confirmed is at the bottom

covid_dat$current_status <- factor(covid_dat$current_status, levels=c("Probable",
                                                                      "Confirmed"))
ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk)) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7"))

# add outline to make bars more visually distinct

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) 

# Add and modify scales --------------------------------------------------------

#' default scales were added based on our data, but we can customize to what we want

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  # y-axis is continuous
  # limit: min of 0, max of 20000
  # breaks: tick marks for every 5000 cases
  # expand to edge of chart
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  # x-axis is date
  # date_breaks: tick marks for every month 
  # date_labels: format as "Jan YYYY"
  # expand to edge of chart
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0))

# NOTE: Coordinate system is inferred to be Cartesian.

# Add annotations --------------------------------------------------------------

# Add chart title

#' Since chart titles are long, you can store them in variables

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle)

# Add footnote

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt)

#' Both title and footnote bleed out of the chart space. We'll fix later!

# Add axis labels

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases")

# Modify legend

#' Moving legend to the top makes sure it's seen before the chart. It also leaves
#' more horizontal space for the chart.

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  guides(fill=guide_legend(title="Current status", position="top"))

# Apply theme ------------------------------------------------------------------

#' ggplot comes with a lot of built-in themes. Try playing around with different
#' ones and pick one that you like!

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  guides(fill=guide_legend(title="Current status", position="top")) +
  theme_minimal()

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  guides(fill=guide_legend(title="Current status", position="top")) +
  theme_classic()

#' You can also create your own custom theme! It can take a while to build a 
#' theme, but once you do, you can apply it to multiple charts.
#' 
#' This is nice because any requests to update chart formatting can be done
#' to your theme file vs. individually for each chart.

# read code for theme_mine
source("theme_mine.R")

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle, caption=ftnt) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  guides(fill=guide_legend(title="Current status", position="top")) +
  theme_mine()

# Adjustments ------------------------------------------------------------------

# Fix text wrapping: Title and footnote 

#' str_wrap() helps wrap strings based on a specified character width
#' examine export to determine where break needs to be
#' may need to play around with this a bit

source("theme_mine.R")

ctitle <- "COVID-19 cases by date of onset in the U.S., July–December 2023"
ftnt <- "Data source: CDC COVID-19 Case Surveillance Public Use Data. Data available at: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf"

ctitle_wrap <- str_wrap(ctitle, width=65)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

ggplot(data=covid_dat, aes(x=onset_dt, fill=current_status)) + 
  geom_histogram(binwidth=7, boundary=as.numeric(onset_dt_min_wk), colour="#FFFFFF",
                 linewidth=1) +
  scale_fill_manual(values=c("Confirmed"="#4a75d9", "Probable"="#b2c6f7")) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, 5000), expand=c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b %Y", expand=c(0,0)) +
  labs(title=ctitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week of onset") +
  ylab(label="Number of cases") + 
  
  guides(fill=guide_legend(title="Current status", position="top")) +
  theme_mine()

# Save chart -------------------------------------------------------------------

ggsave(
  filename="covid-epi-curve.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
