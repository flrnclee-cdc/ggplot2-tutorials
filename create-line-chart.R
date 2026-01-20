# Set-up =======================================================================

library(openxlsx)
library(tidyverse)
library(ggtext)
library(ggrepel)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training") 

# EXAMPLE A: Trends in ED Visits for COVID-19, Flu, RSV ========================

# LOW LEVEL CUSTOMIZATION ------------------------------------------------------

# Prepare Data -----------------------------------------------------------------

resp_dat <- read.csv(".\\Datasets\\nssp_ed_covid19_flu_rsv.csv")

# preview data 
str(resp_dat)

#' percent_visits
#' * Denominator: all ED patient visits for the specified geography that were 
#'   observed for the given week.
#' * Numerator: ED patient visits for the pathogen of interest for the specified 
#'   geography that were observed for the given week.

# filter to national data
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

# change week_end from chr to date
resp_dat_usa$week_end <- as.Date(resp_dat_usa$week_end)

# merge in mmwr weeks
mmwr_dates <- read.csv(".\\Datasets\\mmwr-weeks.csv")

# clean mmwr_dates first
mmwr_dates <- mmwr_dates %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "week_end"
  ) %>%
  select(-year) %>% 
  arrange(week_end)
mmwr_dates$week_end <- as.Date(mmwr_dates$week_end, "%m/%d/%Y")
mmwr_dates <- mmwr_dates %>% filter(!is.na(week_end))

resp_dat_usa <- resp_dat_usa %>% left_join(mmwr_dates)

write.csv(resp_dat_usa, ".\\Datasets\\Transformed\\resp-dat-usa.csv",
          row.names=F)

# Build Chart ------------------------------------------------------------------

#' set data: resp_dat_usa
#' set aesthetics: x, y
#' assign geometry: line

# Basic chart
ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group = condition)) +
  geom_line() 

# add colors

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition))

# customize colors
                       
ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition)) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664"))

# increase boldness of lines

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664"))

# modify scales

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0))

# improve titles

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name="Percent of ED visits",
                     limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b %Y", 
               expand=c(0,0)) +
  ggtitle("Emergency department visits in the United States")

# modify legend

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name="Percent of ED visits",
                     limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b %Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  ggtitle("Emergency department visits in the United States") 
  
# apply a theme
# I usually like theme_bw() or theme_minimal()

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name="Percent of ED visits",
                     limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b %Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  ggtitle("Emergency department visits in the United States") + 
  theme_minimal()

# modify theme
# place legend at the top

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name="Percent of ED visits",
                     limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b %Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  ggtitle("Emergency department visits in the United States") + 
  theme_minimal() +
  theme(legend.title=element_text(face="bold"), legend.position="top")

# nitpicky edits...

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name="Percent of ED visits",
                     limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  ggtitle("Emergency department visits in the <b>United States</b>") + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    legend.title=element_text(face="bold"), legend.position="top",
        axis.title.x=element_text(margin=margin(t=10)),
        axis.title.y=element_text(margin=margin(r=10)))

# HIGH-LEVEL CUSTOMIZATION -----------------------------------------------------

# make more descriptive y-axis title
# use annotate()

#theme_get()$text$size #get base text size -> 11
#theme_get()$axis.text #get axis text size -> 11*0.8

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle("Emergency department visits in the <b>United States</b>") + 
  coord_cartesian(clip="off") + #allows drawing outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# add last data point

recent_pt <- resp_dat_usa %>%
  group_by(condition) %>%
  filter(week_end==max(week_end)) %>%
  ungroup()

recent_pt

# chart last data points with  matching aesthetics

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle("Emergency department visits in the <b>United States</b>") + 
  coord_cartesian(clip="off") + #allows drawing outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# need to expand right-hand margin to fit labels

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
            aes(label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                colour=condition),
            hjust=-0.1, fill=NA, label.color=NA) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle("Emergency department visits in the <b>United States</b>") + 
  coord_cartesian(clip="off") + #allows drawing outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# theme_get()$plot.margin

# notice labels overlap

recent_pt <- recent_pt %>%
  mutate(v_adj = case_when(
    condition == "influenza" ~ percent_visits + 0,
    condition == "covid" ~ percent_visits + 0.3,
    condition == "rsv" ~ percent_visits - 0.3
  ))

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = guide_legend(title="")) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle("Emergency department visits in the <b>United States</b>") + 
  coord_cartesian(clip="off") + #allows drawing outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# update title

chart_title <- str_wrap("The percentage of emergency department (ED) visits with a discharge 
diagnosis of <b>influenza \u25AC</b> reached a high of 8.5% during the last week of 2025.", width=65)
chart_title <- gsub("\n", "<br>", chart_title)
chart_stitle <- str_wrap("Since then, the percentage has decreased 2.3 percentage points to 
                          6.3% during Week 1 of 2026.", width=65)
chart_stitle <- gsub("\n", "<br>", chart_stitle)

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle(chart_title, subtitle = chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# add more white space

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# adjust title so there is blue!

chart_title <- str_wrap("The percentage of emergency department (ED) visits with a discharge 
diagnosis of <b>influenza \u25AC</b> reached a high of 8.5% during the last week of 2025.", width=65)
chart_title <- gsub("\n", "<br>", chart_title)
# apply formatting after the fact
chart_title <- gsub("<b>influenza \u25AC</b>", "<span style='color: #0A58D6;'><b>influenza \u25AC</b></span>", 
                    chart_title)

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))


# in this format, faceting is really easy to apply to separate out
# into a panel chart

chart_title <- str_wrap("The percentage of emergency department (ED) visits with a discharge 
diagnosis of <b>influenza \u25AC</b> reached a high of 8.5% during the last week of 2025.", width=65)
chart_title <- gsub("\n", "<br>", chart_title)
# apply formatting after the fact
chart_title <- gsub("<b>influenza \u25AC</b>", "<span style='color: #0A58D6;'><b>influenza \u25AC</b></span>", 
                    chart_title)

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  facet_grid(~condition) + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)))

# need to adjust spacing

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of visit", 
           hjust=0, vjust=0.5,
           size = (11*0.8)/.pt,
           color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  facet_grid(~condition) + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)), 
    panel.spacing=unit(1, "lines"))

# fix annotation being repeated

anno_y <- data.frame(
  condition=unique(resp_dat_usa$condition)[1],
  x = min(resp_dat_usa$week_end),
  y = 9,
  label = "% of visit")

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(y= v_adj, label=paste0("<b>", condition, ":</b><br>", round(percent_visits,1), "%"),
                    colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  geom_text(data = anno_y,
            aes(x=x, y=y, label=label),
            hjust=0, vjust=0.5,
            size = (11*0.8)/.pt,
            color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  facet_grid(~condition) + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)), 
    panel.spacing=unit(1, "lines"))

# need to fix labels. Since there are panel titles, we don't need to repeat the group

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(label=paste0(round(percent_visits,1), "%"),
                    colour=condition),
                hjust=0.5, vjust=0, nudge_y=0.3,
                fill="#FFFFFF", label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(name="Week end", date_breaks = "6 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  geom_text(data = anno_y,
            aes(x=x, y=y, label=label),
            hjust=0, vjust=0.5,
            size = (11*0.8)/.pt,
            color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  facet_grid(~condition) + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)), 
    panel.spacing=unit(1, "lines"))

# now let's format the panels a bit more

ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition)) +
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=recent_pt, aes(colour=condition), size=4) +
  geom_richtext(data=recent_pt, 
                aes(label=paste0(round(percent_visits,1), "%"),
                    colour=condition),
                hjust=0.5, vjust=0, nudge_y=0.3,
                fill="#FFFFFF", label.color=NA, size=(11*0.8)/.pt) + 
  scale_color_manual(values=c(covid="#F06F19", influenza="#0A58D6",rsv="#890664")) +
  scale_y_continuous(name=NULL, limits=c(0,9), breaks=c(0:9), expand=expansion(mult=c(0,0.08))) +
  scale_x_date(name="Week end", date_breaks = "12 months", date_labels="%b\n%Y", 
               expand=c(0,0)) +
  guides(colour = "none") +
  geom_text(data = anno_y,
            aes(x=x, y=y, label=label),
            hjust=0, vjust=0.5,
            size = (11*0.8)/.pt,
            color = "grey30") +
  ggtitle(chart_title, subtitle=chart_stitle) + 
  coord_cartesian(clip="off") + #allows drawing http://127.0.0.1:40093/graphics/plot_zoom_png?width=566&height=347outside of plot area
  facet_grid(~condition) + 
  theme_minimal() +
  theme(
    plot.title=element_markdown(lineheight=1.2),
    plot.subtitle=element_markdown(lineheight=1.2, margin=margin(b=20)),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    legend.title=element_text(face="bold"), legend.position="top",
    axis.title.x=element_text(margin=margin(t=10)), 
    panel.spacing=unit(1, "lines"),
    strip.text=element_text(face="bold"))
