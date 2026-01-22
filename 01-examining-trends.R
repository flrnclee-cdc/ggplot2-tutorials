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

# READ IN DATA =================================================================

resp_dat_usa <- read.csv(".\\Datasets\\resp-dat-usa.csv")

# view summary
str(resp_dat_usa)

# check data types
resp_dat_usa$week_end <- as.Date(resp_dat_usa$week_end, "%Y-%m-%d")

# CREATE BASIC CHART ===========================================================

# Assign data to aesthetics ----------------------------------------------------

base <- ggplot(data=resp_dat_usa, 
       aes(x=week_end, y=percent_visits, group=condition))

base

# Assign geometry --------------------------------------------------------------

base + geom_line()

# Increase line thickness - default is too light!

base + geom_line(linewidth=1.5)

# Add colors to lines

base + geom_line(aes(colour=condition), linewidth=1.5)

# Customize colors

base +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A"))

# Add and modify scales --------------------------------------------------------

#' default scales were added based on our data, but we can customize to what we want

base +
  geom_line(aes(colour=condition), linewidth=1.5) +
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  # y-axis is continuous
  # limit: min of 0, max of 9
  # breaks: tick marks for every 1 unit
  # expand to edge of chart
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  # x-axis is date
  # date_breaks: tick marks for every 6 months
  # date_labels: format as "Jan YYYY"
  # expand to edge of chart
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0))

# NOTE: Coordinate system is inferred to be Cartesian.

# Add annotations --------------------------------------------------------------

# Add chart title

#' since chart titles are long, you can store them in variables

ctitle <- "The percentage of emergency department (ED) visits with a discharge diagnosis of influenza reached a high of 8.5% during the last week of 2025 before decreasing to 6.3% during the first week of 2026."
csubtitle <- "The percentage of ED visits with a discharge diagnosis of COVID-19 or RSV during the first week of 2026 was 0.9% and 0.%, respectively."

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title = ctitle, subtitle=csubtitle)

#' the title bleeds out of chart space, but we will deal with this later

# Add footnote

ftnt <- "Data source: National Syndromic Surveillance Program. Data available at: https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data"

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt)

#' footnote also bleeds out of chart space, but we will deal with this later

# Add axis labels

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits")

# Modify legend

#' move legend to top to help readers understand color encoding before chart
#' modify title with proper casing

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND ------------------------------------------------------------------
  # for colour attribute, use a legend
  # for legends, use guide_legend() to customize
  guides(colour=guide_legend(title="Condition", position="top"))

# Apply theme ------------------------------------------------------------------

#' ggplot2 has a number of built-in themes you can apply to your chart
#' https://ggplot2.tidyverse.org/reference/ggtheme.html
#' 
#' let's apply theme_minimal() to improve the look-and-feel 

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND ------------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # THEME -------------------------------------------------------------------
  theme_minimal()

#' themes can be further customized. 
 
# example: legend title should be bold and match the size of legend text.

text_size <- theme_minimal()$text$size
theme_minimal()$legend.text #'rel' num 0.8, which means legend text 80% of $text
# specify a smaller text size for labeling
label_size <- text_size * 0.8
# specify even smaller text for footnote
ftnt_size <- text_size * 0.7

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND ------------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # THEME -------------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size))

# example: apply label_size to axis titles and ftnt_size to footnote

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND ------------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # THEME -------------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size),
        axis.title.y=element_text(size=label_size),
        # hjust=0 is left alignment
        plot.caption=element_text(size=ftnt_size, hjust=0))

# example: add  more white space around axis labels and around plot 

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle, subtitle=csubtitle, caption=ftnt) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND ------------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # THEME -------------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        # hjust=0 is left alignment
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=10))

# Fix text wrapping: Title, subtitle, footnote ---------------------------------

#' str_wrap() helps wrap strings based on a specified character width
#' examine export to determine where break needs to be
#' may need to play around with this a bit

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=75)
ftnt_wrap <- str_wrap(ftnt, width=100, whitespace_only = FALSE)

base +
  # GEOMETRY ----------------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES ------------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # THEME --------------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        # hjust=0 is left alignment
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=10))

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-1.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

# CREATE ADVANCED CHART ========================================================

#' the advanced chart has direct labeling, embedded legend in the title, and
#' a cleaner y-axis label to get us closer to our final chart

# Add last data point ----------------------------------------------------------

# extend right-hand margin to make room for last point labels
# allow things to be drawn outside of coordinate system

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75)) #expanded to 75

# create dataset for last point

last_pt <- resp_dat_usa %>%
  group_by(condition) %>%
  # only keep data for last week_end
  filter(week_end==max(week_end)) %>%
  ungroup()

last_pt

# use last_pt to add layer of points to existing chart

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt) +
  # don't need to specify x and y, since we are using the same ones in base
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

# make dots bigger and match color to condition

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

# Add last data point labels ---------------------------------------------------

# map data in last_pt to text using geom_text

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_text(data=last_pt, aes(label=paste0(condition, ": ", 
                                           round(percent_visits, 1), "%"))) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

# left-align label and match color to condition

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_text(data=last_pt, aes(label=paste0(condition, ": ", 
                                           round(percent_visits, 1), "%"),
                              colour=condition),
            hjust=-0.1) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

#' using geom_richtext() gives you greater control over the formatting of
#' the label to match our sketch using HTML

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(label=paste0("<b>", condition, "</b>:<br>", 
                                           round(percent_visits, 1), "%"),
                              colour=condition),
            hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

# fix overlapping labels for COVID-19 and RSV

last_pt <- last_pt %>%
  mutate(v_adj = case_when(
    condition == "Influenza" ~ percent_visits + 0, #don't adjust
    condition == "COVID-19" ~ percent_visits + 0.3, #move up 0.3 units
    condition == "RSV" ~ percent_visits - 0.3 #move down 0.3 units
  ))

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
    label=paste0("<b>", condition, "</b>:<br>", 
                 round(percent_visits, 1), "%"),
    colour=condition),
    hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(legend.title=element_text(face="bold", size=label_size),
        axis.title.x=element_text(size=label_size, margin=margin(t=10)),
        axis.title.y=element_text(size=label_size, margin=margin(r=10)),
        plot.caption=element_text(size=ftnt_size, hjust=0),
        plot.margin=margin(t=10, b=10, l=10, r=75))

# Embed legend to title --------------------------------------------------------

#' to add formatting to our title and subtitle (e.g., bold condition names, apply 
#' condition colors), we should override theme default of plot.title=element_text() 
#' to use element_markdown() instead
#' 
#' element_markdown() will render HTML/CSS for custom formatting

# bold condition names and add line symbol
# line symbol is rendered with \u25AC
ctitle <- "The percentage of emergency department (ED) visits with a discharge diagnosis of <b>influenza\u25AC</b> reached a high of 8.5% during the last week of 2025 before decreasing to 6.3% during the first week of 2026."
csubtitle <- "The percentage of ED visits with a discharge diagnosis of <b>COVID-19\u25AC</b> or <b>RSV\u25AC</b> during the first week of 2026 was 0.9% and 0.%, respectively."

ctitle_wrap <- str_wrap(ctitle, width=65)
csubtitle_wrap <- str_wrap(csubtitle, width=80)

ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour=guide_legend(title="Condition", position="top")) +
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# apply condition colors to title -> use <span style='color:...'> for this
# remove redundant legend

ctitle_wrap_fmt <- gsub("<b>influenza\u25AC</b>", "<span style='color: #0A58D6;'><b>influenza\u25AC</b></span>", 
                        ctitle_wrap)
csubtitle_wrap_fmt <- gsub("<b>COVID-19\u25AC</b>", "<span style='color: #C04F15;'><b>COVID-19\u25AC</b></span>", 
                      csubtitle_wrap)
csubtitle_wrap_fmt <- gsub("<b>RSV\u25AC</b>", "<span style='color: #652B5A;'><b>RSV\u25AC</b></span>", 
                           csubtitle_wrap_fmt)

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# Add 8.5% marker and data label -----------------------------------------------

#' for one-off labeling, use annotate()

# create dataset with highest value for Influenza group
influ_high <- resp_dat_usa %>% 
  filter(condition=="Influenza") %>%
  filter(percent_visits==max(percent_visits))

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION -----------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
           size=4) +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%")) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# adjust position, color, and size of label

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION -----------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
    # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# Re-format with more descriptive y-axis label ---------------------------------

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  # x: first week_end date
  # y: the y-axis scale max, which is 9
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# adjust position, size, and color of label

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
           hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(),
    plot.subtitle=element_markdown(),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# Minor design updates ---------------------------------------------------------

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
         hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-2.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

# SPLITTING INTO MULTIPLE PANELS ===============================================

# facet_grid() to easily split into separate panels!

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
         hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75))

#'increase space between panels to prevent overlap of labels

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(x=week_end, y=v_adj,  # re-specify x and y
                                  label=paste0("<b>", condition, "</b>:<br>", 
                                               round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=-0.1, fill=NA, label.color=NA, size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
           hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=75),
    panel.spacing=unit(1.25, "lines"))

# remove condition from labels, since panel is labeled
# adjust position above point by nudging 

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(label=paste0(round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=0.5, nudge_y=0.9, 
                # add a background color and outline so label is easy to see
                fill="#FFFFFF", size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b %Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
           hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=10, b=10, l=10, r=20),
    panel.spacing=unit(1.25, "lines"))

# update x-axis labeling to avoid overlap

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(label=paste0(round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=0.5, nudge_y=0.9, 
                fill="#FFFFFF", size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "6 months", date_labels="%b\n%Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # ANNOTATION ---------------------------------------------------------
  annotate("point", x=influ_high$week_end, y=influ_high$percent_visits,
         size=4, colour="#0A58D6") +
  annotate("text", x=influ_high$week_end, y=influ_high$percent_visits,
           label=paste0(round(influ_high$percent_visits, 1), "%"),
           colour="#0A58D6", size=label_size/.pt, hjust=1.25) +
  annotate("text", x=min(resp_dat_usa$week_end), y=9, label="% of ED visits",
           hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=15, b=10, l=10, r=20),
    panel.spacing=unit(1.25, "lines"))

# remove redundant annotations by using geom_text
# annotations are repeated across panels

# create dataset to use for y-axis label on first panel only
anno_y_axis <- data.frame(
  # first panel
  condition=unique(resp_dat_usa$condition)[1],
  x = min(resp_dat_usa$week_end),
  y = 9,
  label = "% of ED visits")

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(label=paste0(round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=0.5, nudge_y=0.9, 
                fill="#FFFFFF", size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "12 months", date_labels="%b\n%Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # ANNOTATION ---------------------------------------------------------
  geom_text(data=anno_y_axis, aes(x=x, y=y, label=label),
            hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  geom_point(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition), 
             size=4) +
  geom_text(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition,
            label=paste0(round(percent_visits, 1), "%")),
            size=label_size/.pt, hjust=1.25) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=15, b=10, l=10, r=20),
    panel.spacing=unit(1.25, "lines"))

# Minor design updates ---------------------------------------------------------

base +
  # GEOMETRY ----------------------------------------------------------
  geom_line(aes(colour=condition), linewidth=1.5) +
  geom_point(data=last_pt, aes(colour=condition), size=4) +
  # format label as "condition: %"
  geom_richtext(data=last_pt, aes(label=paste0(round(percent_visits, 1), "%"),
                                  colour=condition),
                hjust=0.5, nudge_y=0.9, 
                fill="#FFFFFF", size=label_size/.pt) + 
  # SCALES ------------------------------------------------------------
  scale_color_manual(values=c("COVID-19"="#C04F15", "Influenza"="#0A58D6","RSV"="#652B5A")) +
  scale_y_continuous(limits=c(0,9), breaks=c(0:9), expand=c(0,0)) +
  scale_x_date(date_breaks = "12 months", limits=c(min(resp_dat_usa$week_end), 
                                                   max(resp_dat_usa$week_end)+months(3)),
                        date_labels="%b\n%Y", expand=c(0,0)) +
  # TITLES -------------------------------------------------------------
  labs(title=ctitle_wrap_fmt, subtitle=csubtitle_wrap_fmt, caption=ftnt_wrap) +
  xlab(label="Week end") +
  ylab("Percent of ED visits") +
  # LEGEND -------------------------------------------------------------
  guides(colour="none") + #turn off legend
  # COORDINATES --------------------------------------------------------
  coord_cartesian(clip="off") +
  # FACET --------------------------------------------------------------
  facet_grid(~condition) +
  # ANNOTATION ---------------------------------------------------------
  geom_text(data=anno_y_axis, aes(x=x, y=y, label=label),
          hjust=0, vjust=0.5, size=label_size/.pt, color="grey30") +
  geom_point(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition), 
             size=4) +
  geom_text(data=influ_high, aes(x=week_end, y=percent_visits, colour=condition,
                                 label=paste0(round(percent_visits, 1), "%")),
            size=label_size/.pt, hjust=1.25) +
  # THEME --------------------------------------------------------------
  theme_minimal() +
  theme(
    plot.title=element_markdown(face="bold", lineheight=1.1, size=text_size*1.1),
    plot.subtitle=element_markdown(margin=margin(b=20), lineheight=1.1, size=text_size*0.9),
    legend.title=element_text(face="bold", size=label_size),
    axis.title.x=element_text(size=label_size, margin=margin(t=10)),
    axis.title.y=element_text(size=label_size, margin=margin(r=10)),
    plot.caption=element_text(size=ftnt_size, hjust=0),
    plot.margin=margin(t=15, b=10, l=10, r=20),
    panel.spacing=unit(1.25, "lines"), 
    strip.text=element_text(face="bold", margin=margin(b=10)))

# Save chart -------------------------------------------------------------------

ggsave(
  filename="resp_ed_visit_trend-3.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)
