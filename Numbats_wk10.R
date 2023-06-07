# TidyTuesday | 2023-03-07 | Numbats in Australia
# Data Source: Atlas of Living Australia 

# Load libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(ozmaps)
library(patchwork)
library(wesanderson)
library(paletteer)
library(patchwork)
library(scales)

# Load data
tuesdata <- tidytuesdayR::tt_load('2023-03-07')

# Clean column names
numbats <- tuesdata$numbats
numbats %>% 
  clean_names()

# Convert dyrandra column's NAs 
numbats$dryandra <- ifelse(is.na(numbats$dryandra), "Unknown", numbats$dryandra)
numbats$dryandra

# Summarise data
dryandra <- numbats %>% 
  group_by(dryandra) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
dryandra

# Create new df
data <- data.frame(category = c("True", "False","Unknown"),
                   count = c(125, 597, 83),
                   fraction = c(16, 74, 10))
data 

# Compute the cumulative percentages 
data$ymax <- cumsum(data$fraction)
data$ymax

# Compute ymin values
data$ymin <- c(0, head(data$ymax, n=-1))
data$ymin

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2
data$labelPosition

# Customise label
data$label <- paste0(data$category, "\n  ", data$fraction, "%")
data$label

# Plot donut
donut <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=label), size=3, fontface = "bold", colour = "white") +
  scale_fill_paletteer_d("ochRe::jumping_frog") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(caption = "\n\n#TidyTuesday 2023 week 10 | Viz: Bernadett Piros \n Source: Atlas of Living Australia") +
  annotate(geom = 'text', x = 2, y = 0, size = 3, 
           label = "In Dryandra \n Woodland \n National Park?") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e5d9c2", colour = "#e5d9c2"),
        plot.background = element_rect(fill = "#e5d9c2", colour = "#e5d9c2"),
        plot.caption = element_text(size = 6, hjust = 0.5))

# Load map
ozmap <-ozmap(x = "country")

# Create plot caption
p_caption <- " Occurrence records between 1856 Dec – 2023 Feb. Myrmecobius \n fasciatus rufus is presumed to be extinct. Myrmecobius fasciatus, \n is classified as an endangered species."

# Plot map
map <- ggplot()  + 
  geom_sf(data = ozmap, colour = "black", fill = "#9db753") +
  geom_point(data = numbats,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 color = scientificName)) +
  scale_color_paletteer_d("wesanderson::BottleRocket1") +
  labs(x = NULL, y = NULL, title = "Numbat spotting", caption =  p_caption) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(t = 0)),
        plot.caption = element_text(size = 7, hjust = 0.5),
        plot.margin = margin(1, 0, 0.5, 0, "cm"),
        plot.background = element_rect(fill = "#e5d9c2", colour = "#e5d9c2"),
        panel.background = element_rect(fill = "#e5d9c2", colour = "#e5d9c2"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.key.size = unit(0.7, 'cm'),
        legend.text = element_text(size = 7),
        text = element_text(family = "Arial Rounded MT")) 

# Join plots
map+donut

# Save plot
ggsave(paste0("numbats", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320)







