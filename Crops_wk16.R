# TidyTuesday | 2023-04-18 | Neolithic Founder Crops
# Data Source: The Neolithic Founder Crops in Southwest Asia: Research Compendium 

# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(scales)
library(ggsvg)

# Load data
tuesdata <- tidytuesdayR::tt_load('2023-04-18')
tuesdata <- tidytuesdayR::tt_load(2023, week = 16)

# Check NAs
map(crops, ~ sum(is.na(.)))

# Summarise founder crops
f_crops <- crops %>% 
  filter(!is.na(founder_crop)) %>% 
  group_by(founder_crop) %>% 
  summarise(N = n()) %>% 
  arrange(desc(N))
f_crops

# svg path
url <- "https://www.svgrepo.com/show/212060/wheat-barley.svg"
svg <- paste(readLines(url), collapse = "\n")

# Plot
theme_set(theme_classic(base_size = 14) )
ggplot(f_crops, aes(x = founder_crop, y = N)) +
  geom_segment(aes(x = reorder(founder_crop, N), xend = founder_crop, y = 0, yend = N)) +
  geom_point_svg(aes(x = founder_crop , y = N), svg = svg, size = 13) +
  geom_text(aes(label = N), size = 4, hjust = 0, nudge_y = 9) +
  coord_flip() +
  scale_x_discrete(name = NULL, labels = str_to_title) +
  labs(title = "Distribution of the originally claimed founder crops",
       subtitle = "BP time scale: 12984 - 5050",
       caption = "\n\n#TidyTuesday 2023 week 16 | Viz: Bernadett Piros | Source: The Neolithic Founder Crops in Southwest Asia: Research Compendium") +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 6, hjust = 0.5),
        text = element_text(family = "Garamond"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(25, 25, 10, 25),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) 

# Save png
ggsave(paste0("crops", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320)