# DESCRIPTION ------------------------------------------------------------------

#' This code transforms full data downloaded from the data source (URL below) to
#' a tidy, smaller dataset for ggplot2 training.
#' 
#' https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data 

# SET-UP -----------------------------------------------------------------------

# load packages

library(openxlsx)
library(tidyverse)

# set directory

setwd("C:/Users/kwn5/OneDrive - CDC/Trainings/2026_EIS-Training_II/")

# EXAMINE DATA -----------------------------------------------------------------

dat <- read.csv("data/COVID-19_Case_Surveillance_Public_Use_Data_20261019.csv")

# preview data
str(dat)

# use case earliest date as onset date
dat_u <- dat %>%
  select(cdc_case_earliest_dt., current_status) %>%
  rename("onset_dt"="cdc_case_earliest_dt.")

# format as date
dat_u$onset_dt <- as.Date(dat_u$onset_dt, format="%Y/%m/%d")

#' subset data to 2023-07-01 to 2023-12-31 and then take 5% of the data to 
#' reduce size

dat_s <- dat_u %>%
  filter(onset_dt >= as.Date("2023-07-01") & onset_dt <= as.Date("2023-12-31")) %>%
  slice_sample(prop=0.05) %>%
  mutate(current_status=case_when(current_status=="Probable Case" ~ "Probable",
                                  current_status=="Laboratory-confirmed case" ~ "Confirmed",
                                  .default=as.character(current_status))) %>%
  arrange(onset_dt)

# WRITE ------------------------------------------------------------------------
write.csv(dat_s, "covid-cases.csv", row.names=F)
