# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating a single-panel and multi-panel line chart

# SET-UP =======================================================================

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 

# read in theme
source("theme_mine.R")

# READ IN DATA =================================================================

resp_dat_usa <- read.csv(".\\Datasets\\resp-dat-usa.csv")

# view summary
str(resp_dat_usa)

# check data types
resp_dat_usa$week_end <- as.Date(resp_dat_usa$week_end, "%Y-%m-%d")

str(resp_dat_usa)

# CREATE BASIC CHART ===========================================================

ctitle <- "The percentage of emergency department (ED) visits with a discharge diagnosis of influenza reached a high of 8.5% during the last week of 2025 before decreasing to 6.3% during the first week of 2026."
csubtitle <- "The percentage of ED visits with a discharge diagnosis of COVID-19 or RSV during the first week of 2026 was 0.9% and 0.5%, respectively."
ftnt <- "Data source: National Syndromic Surveillance Program. Data available at: https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data"

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=75)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

#' str_wrap indicates line breaks with \n but we need to replace this with
#' <br> since our theme customizes most text elements using element_markdown()

#' for text elements not customized with element_markdown(), no need to do this 
#' step (e.g., footnote, which is customized with plot.caption)

ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

ggplot(data=resp_dat_usa, aes(x=week_end, y=percent_visits, group=condition)) +
  # geometry-- 
  geom_line(aes(colour=condition), linewidth=1.25) +
  # scales--
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # titles--
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab(label="Percent of ED visits") +
  # legend--
  guides(colour=guide_legend(title="Condition", position="top")) +
  # theme--
  theme_mine()

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-1.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

# CREATE ADVANCED CHART ========================================================

# create dataset for last point

last_pt <- resp_dat_usa %>%
  group_by(condition) %>%
  # only keep data for last week_end
  filter(week_end==max(week_end)) %>%
  ungroup()

last_pt

# fix overlapping labels for COVID-19 and RSV

last_pt <- last_pt %>%
  mutate(v_adj = case_when(
    condition == "Influenza" ~ percent_visits + 0, #don't adjust
    condition == "COVID-19" ~ percent_visits + 0.6, #move up 0.6 units
    condition == "RSV" ~ percent_visits - 0.3 #move down 0.3 units
  ))

# create dataset with highest value for Influenza group
influ_high <- resp_dat_usa %>% 
  filter(condition=="Influenza") %>%
  filter(percent_visits==max(percent_visits))

# bold condition names and add line symbol
# line symbol is rendered with \u25AC
ctitle <- "The percentage of emergency department (ED) visits with a discharge diagnosis of <b>influenza</b> reached a high of 8.5% during the last week of 2025 before decreasing to 6.3% during the first week of 2026."
csubtitle <- "The percentage of ED visits with a discharge diagnosis of <b>COVID-19</b> or <b>RSV</b> during the first week of 2026 was 0.9% and 0.5%, respectively."

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=80)

ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

# apply condition colors to title -> use <span style='color:...'> for this

# add line symbol to title -> use <span style='font-family:...'> for this because
# theme font may not have glyphs

ctitle_wrap_fmt <- gsub("<b>influenza</b>", "<span style='color: #0A58D6;'><b>influenza</b>
                        <span style='font-family:sans;'>\u25AC</span></span>", 
                        ctitle_wrap)
csubtitle_wrap_fmt <- gsub("<b>COVID-19</b>", "<span style='color: #C04F15;'><b>COVID-19</b>
                           <span style='font-family:sans;'>\u25AC</span></span>", 
                           csubtitle_wrap)
csubtitle_wrap_fmt <- gsub("<b>RSV</b>", "<span style='color: #652B5A;'><b>RSV</b>
                           <span style='font-family:sans;'>\u25AC</span></span>", 
                           csubtitle_wrap_fmt)

ftnt <- "Data source: National Syndromic Surveillance Program. Data available at: https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data"
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

ggplot(data=resp_dat_usa, aes(x=week_end, y=percent_visits, group=condition)) +
  # geometry--
  geom_line(aes(colour=condition), linewidth=1.25) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext_mine(data=last_pt, aes(x=week_end, y=v_adj, 
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition), 
                     hjust=-0.1, fill=NA, label.color=NA, size=anno_size/.pt) + 
  # scales--
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # titles--
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab(label=NULL) +
  # legend--
  guides(colour="none") + #turn off legend
  # coordinates--
  coord_cartesian(clip="off") +
  # annotation--
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", hjust=1.25, family="nunito sans", size=anno_size/.pt) +
  # x: first week_end date
  # y: the y-axis scale max, which is 9
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
           hjust=0, vjust=0.5, color="grey30", family="nunito sans", size=anno_size/.pt) +
  # theme--
  theme_mine() +
  theme(plot.margin=margin(t=10, b=10, l=10, r=75))

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-2.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

# SPLITTING INTO MULTIPLE PANELS ===============================================

# facet_grid() to easily split into separate panels!
# code below has some modifications to clean up formatting after split

# code from before -------------------------------------------------------------

last_pt <- resp_dat_usa %>%
  group_by(condition) %>%
  # only keep data for last week_end
  filter(week_end==max(week_end)) %>%
  ungroup()

last_pt

last_pt <- last_pt %>%
  mutate(v_adj = case_when(
    condition == "Influenza" ~ percent_visits + 0, #don't adjust
    condition == "COVID-19" ~ percent_visits + 0.6, #move up 0.6 units
    condition == "RSV" ~ percent_visits - 0.3 #move down 0.3 units
  ))

influ_high <- resp_dat_usa %>% 
  filter(condition=="Influenza") %>%
  filter(percent_visits==max(percent_visits))

ctitle <- "The percentage of emergency department (ED) visits with a discharge diagnosis of <b>influenza</b> reached a high of 8.5% during the last week of 2025 before decreasing to 6.3% during the first week of 2026."
csubtitle <- "The percentage of ED visits with a discharge diagnosis of <b>COVID-19</b> or <b>RSV</b> during the first week of 2026 was 0.9% and 0.5%, respectively."

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=80)

ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

ftnt <- "Data source: National Syndromic Surveillance Program. Data available at: https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data"
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

ggplot(data=resp_dat_usa, aes(x=week_end, y=percent_visits, group=condition)) +
  # geometry--
  geom_line(aes(colour=condition), linewidth=1.25) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext_mine(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                       label=paste0(round(percent_visits, 1), "%"),
                                       colour=condition), hjust=0.5, nudge_y=1.0,
                     fill="#FFFFFF", label.color=NA, size=anno_size/.pt) + 
  # scales--
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "12 months", date_labels="%b\n%Y", expand=c(0,0)) +
  # titles--
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab(label="Percent of ED visits") +
  # legend--
  guides(colour="none") + #turn off legend
  # coordinates--
  coord_cartesian(clip="off") +
  # annotation--
  geom_point(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition), 
           size=4) +
  geom_text(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition,
                                 label=paste0(round(percent_visits, 1), "%")), 
            hjust=1.25, size=anno_size/.pt) +
  # facet--
  facet_grid(~condition) +
  # theme--
  theme_mine() +
  theme(plot.margin=margin(t=10, b=10, l=10, r=75))

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-3.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
