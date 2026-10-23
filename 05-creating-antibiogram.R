# DESCRIPTION ==================================================================

#' Step-by-step tutorial on creating epi curve 

# SET-UP =======================================================================

# load packages

library(openxlsx)
library(tidyverse)
library(ggtext)
library(showtext)

# set directory 

getwd() #shows current directory
setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials") 


setwd("C:\\Users\\kwn5\\OneDrive - CDC\\Trainings\\2026_EIS-Training\\ggplot2-tutorials")

library(tidyverse)
library(ggtext)

# ---- Sample antibiogram data (long format) ----
# Same fictional dataset as the mockup: 8 organisms x 12 antibiotics

antibiotics <- tribble(
  ~ab_id, ~ab_name,          ~ab_class,
  "amp",  "Ampicillin",      "Penicillin",
  "amc",  "Amp-Sulbactam",   "Penicillin",
  "czo",  "Cefazolin",       "Cephalosporin",
  "caz",  "Ceftazidime",     "Cephalosporin",
  "cro",  "Ceftriaxone",     "Cephalosporin",
  "fep",  "Cefepime",        "Cephalosporin",
  "mem",  "Meropenem",       "Carbapenem",
  "cip",  "Ciprofloxacin",   "Fluoroquinolone",
  "gen",  "Gentamicin",      "Aminoglycoside",
  "sxt",  "Bactrim",         "Folate inhibitor",
  "van",  "Vancomycin",      "Glycopeptide",
  "lzd",  "Linezolid",       "Oxazolidinone"
)

organisms <- tribble(
  ~org_id,   ~org_name,          ~n,  ~gram,
  "ecoli",   "E. coli",          412, "negative",
  "kpneu",   "K. pneumoniae",    187, "negative",
  "paer",    "P. aeruginosa",    143, "negative",
  "ecloac",  "E. cloacae",       24,  "negative",
  "saur",    "S. aureus (all)",  356, "positive",
  "mrsa",    "MRSA (subset)",    149, "positive",
  "efaec",   "E. faecalis",      98,  "positive",
  "efaem",   "E. faecium",       19,  "positive"
)

# susceptibility values; NA = not routinely tested/reported
values <- tribble(
  ~org_id,  ~ab_id, ~pct_susc,
  "ecoli",  "amp", 52,  "ecoli", "amc", 68,  "ecoli", "czo", 71,
  "ecoli",  "caz", 91,  "ecoli", "cro", 90,  "ecoli", "fep", 94,
  "ecoli",  "mem", 100, "ecoli", "cip", 78,  "ecoli", "gen", 92,
  "ecoli",  "sxt", 74,  "ecoli", "van", NA,  "ecoli", "lzd", NA,
  
  "kpneu",  "amp", NA,  "kpneu", "amc", 72,  "kpneu", "czo", 80,
  "kpneu",  "caz", 89,  "kpneu", "cro", 88,  "kpneu", "fep", 93,
  "kpneu",  "mem", 97,  "kpneu", "cip", 85,  "kpneu", "gen", 90,
  "kpneu",  "sxt", 81,  "kpneu", "van", NA,  "kpneu", "lzd", NA,
  
  "paer",   "amp", NA,  "paer",  "amc", NA,  "paer",  "czo", NA,
  "paer",   "caz", 83,  "paer",  "cro", NA,  "paer",  "fep", 87,
  "paer",   "mem", 85,  "paer",  "cip", 72,  "paer",  "gen", 88,
  "paer",   "sxt", NA,  "paer",  "van", NA,  "paer",  "lzd", NA,
  
  "ecloac", "amp", NA,  "ecloac","amc", 45,  "ecloac","czo", NA,
  "ecloac", "caz", 78,  "ecloac","cro", 76,  "ecloac","fep", 95,
  "ecloac", "mem", 98,  "ecloac","cip", 88,  "ecloac","gen", 91,
  "ecloac", "sxt", 83,  "ecloac","van", NA,  "ecloac","lzd", NA,
  
  "saur",   "amp", NA,  "saur",  "amc", NA,  "saur",  "czo", 58,
  "saur",   "caz", NA,  "saur",  "cro", NA,  "saur",  "fep", NA,
  "saur",   "mem", NA,  "saur",  "cip", 66,  "saur",  "gen", 95,
  "saur",   "sxt", 96,  "saur",  "van", 100, "saur",  "lzd", 100,
  
  "mrsa",   "amp", NA,  "mrsa",  "amc", NA,  "mrsa",  "czo", 0,
  "mrsa",   "caz", NA,  "mrsa",  "cro", NA,  "mrsa",  "fep", NA,
  "mrsa",   "mem", NA,  "mrsa",  "cip", 38,  "mrsa",  "gen", 91,
  "mrsa",   "sxt", 94,  "mrsa",  "van", 100, "mrsa",  "lzd", 100,
  
  "efaec",  "amp", 96,  "efaec", "amc", NA,  "efaec", "czo", NA,
  "efaec",  "caz", NA,  "efaec", "cro", NA,  "efaec", "fep", NA,
  "efaec",  "mem", NA,  "efaec", "cip", 62,  "efaec", "gen", NA,
  "efaec",  "sxt", NA,  "efaec", "van", 94,  "efaec", "lzd", 99,
  
  "efaem",  "amp", 18,  "efaem", "amc", NA,  "efaem", "czo", NA,
  "efaem",  "caz", NA,  "efaem", "cro", NA,  "efaem", "fep", NA,
  "efaem",  "mem", NA,  "efaem", "cip", 12,  "efaem", "gen", NA,
  "efaem",  "sxt", NA,  "efaem", "van", 56,  "efaem", "lzd", 98
)

# ---- Join into one long tidy dataframe ----
df <- values %>%
  left_join(organisms, by = "org_id") %>%
  left_join(antibiotics, by = "ab_id") %>%
  mutate(
    low_n = n < 30,
    org_name = factor(org_name, levels = rev(organisms$org_name)),   # preserve gram-neg/gram-pos block order
    ab_name  = factor(ab_name, levels = antibiotics$ab_name)         # preserve antibiotic class order
  )

write.csv(".//Datasets//")



# ---- Plot ----
source("theme_mine.R")
ggplot(df, aes(x = ab_name, y = org_name, fill = pct_susc)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text_mine(
    aes(label = ifelse(is.na(pct_susc), "—",
                       paste0(pct_susc, ifelse(low_n, "*", "")))),
    color = ifelse(is.na(df$pct_susc), "grey50",
                   ifelse(df$pct_susc >= 90 | df$pct_susc <= 39, "white", "grey20")),
    size = anno_size/.pt, fontface = "bold"
  ) +
  scale_fill_stepsn(
    colors = c("#dc2626", "#fb923c", "#fde047", "#bef264", "#4ade80", "#15803d"),
    breaks = c(40, 60, 70, 80, 90),
    limits = c(0, 100),
    na.value = "grey90",
    name = "% Susceptible",
    guide = guide_coloursteps(
      show.limits = TRUE,       # ensures end labels aren't dropped
    )
  ) +
  labs(
    title = "Hospital Antibiogram — Sample Dataset",
    subtitle = "Cumulative % susceptibility, Jan–Dec 2025 (fictional/illustrative data)\n\\* n < 30 isolates — interpret with caution (CLSI)",
    x = NULL, y = NULL
  ) +
  theme_mine() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
  )

ggsave(
  filename="antibiogram.png",
  path=".\\Charts",
  width=10,height=7,
  dpi="print" #save as print quality
)
