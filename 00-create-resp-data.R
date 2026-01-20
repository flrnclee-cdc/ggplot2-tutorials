# DESCRIPTION ------------------------------------------------------------------

#' This code transforms full data downloaded from the data source (URL below) to
#' a tidy national dataset for ggplot2 training. 
#' 
#' https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data

# SET-UP -----------------------------------------------------------------------

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)
library(ggrepel)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 

# EXAMINE DATA -----------------------------------------------------------------

resp_dat <- read.csv(".\\Raw\\nssp_ed_covid19_flu_rsv.csv")

# preview data 
str(resp_dat)

#' percent_visits
#' * Denominator: all ED patient visits for the specified geography that were 
#'   observed for the given week.
#' * Numerator: ED patient visits for the pathogen of interest for the specified 
#'   geography that were observed for the given week.

# CREATE TIDY DATASET ----------------------------------------------------------

# filter to usa
resp_dat_usa <- resp_dat %>%
  filter(geography=="United States" & county=="All")

# transform to tidy format
resp_dat_usa <- resp_dat_usa %>%
  select(week_end, geography, county, 
         percent_visits_covid, percent_visits_influenza, percent_visits_rsv) %>%
  pivot_longer(
    cols = starts_with("percent_visits"),
    names_to = "condition",
    names_prefix = "percent_visits_",
    values_to = "percent_visits"
  )

# preview data 
str(resp_dat_usa)

# change week_end from chr to date
# specify current format as mm/dd/yyyy
resp_dat_usa$week_end <- as.Date(resp_dat_usa$week_end, "%m/%d/%Y")

# clean up condition names

resp_dat_usa <- resp_dat_usa %>%
  mutate(condition=case_when(
    condition=="covid" ~ "COVID-19",
    condition=="influenza" ~ "Influenza",
    condition=="rsv" ~ "RSV")
  )

# MERGE IN MMWR WEEKS ----------------------------------------------------------

# load mmwr dates data
mmwr_dates <- read.csv(".\\Raw\\mmwr-weeks.csv")

# preview data
head(mmwr_dates)

# transform for merge
mmwr_dates <- mmwr_dates %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "week_end"
  ) %>%
  select(-year) %>% 
  arrange(week_end)

# convert week_end to date
mmwr_dates$week_end <- as.Date(mmwr_dates$week_end, "%m/%d/%Y")
mmwr_dates <- mmwr_dates %>% filter(!is.na(week_end))

# merge mmwr_dates to resp_dat_usa
resp_dat_usa <- resp_dat_usa %>% left_join(mmwr_dates)

# preview data
head(resp_dat_usa)

# SAVE DATASET FOR TRAINING ----------------------------------------------------
write.csv(resp_dat_usa, ".\\Datasets\\resp-dat-usa.csv", row.names=F)

