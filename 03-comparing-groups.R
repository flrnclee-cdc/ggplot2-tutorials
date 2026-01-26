# DESCRIPTION ==================================================================

#' TEXT

# SET-UP =======================================================================

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 

# READ IN DATA =================================================================

tob_dat <- read.csv(".\\Datasets\\teen-tobacco-use.csv")

# view summary
str(tob_dat)

tob_dat$year <- as.character(tob_dat$year)

ggplot(data=tob_dat, aes(x=product, y=percent, fill=year)) +
  geom_bar(stat="identity", position="dodge")

#' sort by 2018 value

tob_dat <- tob_dat %>% 
  group_by(product) %>%
  mutate(pct18=percent[year==2018]) %>%
  ungroup() %>%
  arrange(desc(year), desc(pct18)) %>%
  select(-pct18)

#' turn product into factor based on this ordering
 
tob_dat$product <- factor(tob_dat$product, levels=unique(tob_dat$product))

ggplot(data=tob_dat, aes(x=product, y=percent, fill=year)) +
  geom_bar(stat="identity", position="dodge")

ggplot(data=tob_dat, aes(x=product, y=percent, fill=year)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(percent, "%")), position=position_dodge(width=0.9),
            hjust=-0.1) +
  scale_fill_manual(values=c("2011"="#A3CEED", "2018"="#1D6295")) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous() +
  xlab("") +
  ylab("Percent") +
  guides(fill=guide_legend(title="", position="top")) +
  coord_flip()


source("theme_mine.R")

ggplot(data=tob_dat, aes(x=product, y=percent, fill=year)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(percent, "%")), position=position_dodge(width=0.9),
            hjust=-0.1, size=label_size/.pt) +
  scale_fill_manual(values=c("2011"="#A3CEED", "2018"="#1D6295")) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous() +
  xlab("") +
  ylab("Percent") +
  guides(fill=guide_legend(title="", position="top")) +
  coord_flip() +
  theme_mine()

ctitle <- "Between 2011 and 2018, e-cigarette use among high school students increased by 19 percentage points."
csubtitle <- "Current use of other tobacco products decreased or did not change."
ftnt <- "Gentzke AS, Creamer M, Cullen KA, et al. Vital Signs: Tobacco Product Use Among Middle and High School Students — United States, 2011–2018. MMWR Morb Mortal Wkly Rep 2019;68:157–164. DOI: http://dx.doi.org/10.15585/mmwr.mm6806e1."

ctitle_wrap <- str_wrap(ctitle, width=50)
csubtitle_wrap <- str_wrap(csubtitle, width=70)
ftnt_wrap <- str_wrap(ftnt, width=80, whitespace_only = FALSE)
ctitle_wrap <- gsub("\n", "<br>", ctitle_wrap)
csubtitle_wrap <- gsub("\n", "<br>", csubtitle_wrap)

ggplot(data=tob_dat, aes(x=product, y=percent, fill=year)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(percent, "%")), position=position_dodge(width=0.9),
            hjust=-0.1, size=label_size/.pt) +
  scale_fill_manual(values=c("2011"="#A3CEED", "2018"="#1D6295")) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels=NULL) +
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) + 
  xlab("") +
  ylab("Percent") +
  guides(fill=guide_legend(title="", position="top")) +
  coord_flip() +
  theme_mine() 

ggsave(
  filename="hs-tobacco-use-1.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

tob_dat_2 <- tob_dat %>%
  pivot_wider(names_from=year, values_from=percent)

ggplot(data=tob_dat_2, aes(y=product)) +
  geom_segment(aes(x=`2011`, xend=`2018`, yend=product),
               colour="#000000", size=2) +
  geom_point(data=tob_dat, aes(x=percent, y=product, colour=year), size=5) +
  scale_colour_manual(values=c("2011"="#A3CEED", "2018"="#1D6295")) +
  scale_x_continuous(labels=NULL) +
  scale_y_discrete() +
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) + 
  ylab("") +
  xlab("Percent") +
  guides(colour=guide_legend(title="", position="top")) +
  theme_mine() 
  

tob_dat <- tob_dat %>%
  group_by(product) %>%
  mutate(is_left = percent==min(percent)) %>%
  ungroup()

ggplot(data=tob_dat_2, aes(y=product)) +
  geom_segment(aes(x=`2011`, xend=`2018`, yend=product),
               colour="#000000", size=2) +
  geom_point(data=tob_dat, aes(x=percent, y=product, colour=year), size=5) +
  geom_text(data=tob_dat, aes(x=percent, y=product, 
                              label=paste0(percent, "%"), 
                              colour=year), size=label_size/.pt,
            hjust=ifelse(tob_dat$is_left, 1.3, -0.)) +
  scale_colour_manual(values=c("2011"="#A3CEED", "2018"="#1D6295")) +
  scale_x_continuous(labels=NULL, expand=expansion(mult = c(0.15, 0.2))) +
  scale_y_discrete(limits=rev) +
  labs(title=ctitle_wrap, subtitle=csubtitle_wrap, caption=ftnt_wrap) + 
  ylab("") +
  xlab("Percent") +
  guides(colour=guide_legend(title="", position="top")) +
  theme_mine() +
  theme(panel.grid=element_blank())

ggsave(
  filename="hs-tobacco-use-2.png",
  path=".\\Charts",
  width=6,height=5,
  dpi="print" #save as print quality
)

