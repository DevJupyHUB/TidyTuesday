# TidyTuesday | 2023-05-16 | Tornados
# Data Source: NOAA's National Weather Service Storm Prediction Center

# Load libraries
library(tidytuesdayR)
library(tidyverse)

# Load data
tuesdata <- tidytuesdayR::tt_load('2023-05-16')
tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

tornados <- tuesdata$tornados

# Load map
us_map <- map_data("state")

# Create new df with month as factor
tor_df <- tornados %>% 
  mutate(month = as.factor(mo),
         month = as.factor(format(date,"%b")))

# Plot
ggplot() +
  geom_polygon(data = us_map,
               mapping = aes(long,
                             lat,
                             group = group),
               fill = "#D0C9C0",
               color = "black", 
               linewidth = 0.1) +
  geom_point(data = tor_df,
             aes(slon, slat),
             shape = ".",
             colour = "#5F7161") +
  facet_wrap(~ factor(month,
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) +
  coord_cartesian(xlim = c(-125, -66),
                  ylim = c(23, 50)) +
  labs(x = NULL,
       y = NULL,
       title = "US tornados per month across 1950 - 2022",
       caption = "\n\n#TidyTuesday 2023 week 20 | Viz: Bernadett Piros | Source: NOAA's National Weather Service Storm Prediction Center") +
  theme(panel.background = element_rect(fill = "#E0E7F1"),
        plot.background = element_rect(fill = "#E0E7F1"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(color = "#5F7161", face = "bold"),
        plot.title = element_text(face = "bold", color = "#5F7161", hjust = 0.5),
        plot.caption = element_text(size = 6, color = "#5F7161", hjust = 0.5))

# Save plot
ggsave("tornados.png", width = 8, height = 5, dpi = 600)