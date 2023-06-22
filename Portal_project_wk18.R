# TidyTuesday wk 18 | 2023-05-02 | The Portal project
# Data Source: Portal Project Data

# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(scales)
library(ggsci)
library(ggtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2023-05-02')
tuesdata <- tidytuesdayR::tt_load(2023, week = 18)

plots <- tuesdata$plots
species <- tuesdata$species
surveys <- tuesdata$surveys

# Join tables
df <- surveys %>% left_join(species, 
                            by=c('species'='species'))

# Creating a new df for bold geom line 
bailey <- df %>% 
  filter(commonname == "Bailey's pocket mouse") %>% 
  group_by(year) %>% 
  count(commonname, scientificname, sort = TRUE)


# Creating a new df for normal geom lines
kangaroo <- df %>% 
  filter(str_detect(scientificname,'Dipodomys')) %>% 
  group_by(year) %>% 
  count(commonname, scientificname, sort = TRUE)


# Label text
lbl <- "In the 1970s, researchers removed kangaroo rats from an ecosystem in the \nChihuahuan Desert, and for many years, no other small rodents moved in to fill \nthe niche they left. After twenty years the experimental removal, Bailey's \npocket mouse moved into the area.(Wikipedia)"

# Plot 
p <- ggplot() +
  geom_line(data = bailey, mapping = aes(x = year, y = n, color = commonname), linewidth = 2) +
  geom_line(data = kangaroo, mapping = aes(x = year, y = n, color = commonname)) +
  scale_color_nejm() +
  annotate(geom = "text", x = 1990, y = 585, label = lbl,
           size = 2, color = "#BC3C29FF", fontface = "bold", family = "Tw Cen MT", lineheight = .9) +
  annotate(geom = "segment", x = 1998, y = 565, xend = 2002, yend = 450,
           arrow = arrow(length = unit(2, "mm")), color = "#BC3C29FF") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_markdown(size = 11, hjust = 0.5 , face = "bold", family = "Cooper"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 6, hjust = 0.5),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text = element_text(size = 8, face = "bold"),
        axis.line = element_line(linewidth = 0.2),
        axis.ticks.x = element_line(linewidth = 0.9),
        panel.grid = element_blank(),
        legend.position = c(.99, .95),
        legend.justification = c("right", "top"),
        legend.title = element_blank(),
        legend.text = element_text(size = 7, face = "bold")) +
  labs(title = "<span style = 'color:#BC3C29FF;'>Bailey's pocket mouse</span> and kangaroo rats in Chihuahuan Desert,
       Arizona 1978 - 2022",
       x = NULL, y = NULL,
       caption = "\n\n#TidyTuesday 20023 week 18 | Viz: Bernadett Piros | Source: Portal poject data")
p

# Save the plot
ggsave("pportal.png", width = 8, height = 5, dpi = 600)

