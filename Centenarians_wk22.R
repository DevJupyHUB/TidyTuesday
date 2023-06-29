# TidyTuesday | 2023-05-30 | Verified Oldest People
# Data Source: frankiethull: Centenarians

library(tidytuesdayR)
library(tidyverse)
library(ggflags) 
library(countrycode) 
library(tidytext) 
library(scales) 
library(ggrepel)
library(gghighlight)
library(patchwork)
library(glue)
library(ggchicklet)

# load data

tuesdata <- tidytuesdayR::tt_load('2023-05-30')
tuesdata <- tidytuesdayR::tt_load(2023, week = 22)

centenarians <- tuesdata$centenarians

# check countries

centenarians %>%
  distinct(place_of_death_or_residence)

# check if countries are in the right format

countrycode::guess_field(centenarians$place_of_death_or_residence, min_similarity= 80)

# translate countries into iso2c format

centenarians$code <- countrycode(centenarians$place_of_death_or_residence, "country.name", "iso2c")

# sorting out the french overseas territory codes

centenarians <- centenarians %>%
  mutate(code = ifelse(place_of_death_or_residence == "France (French Guiana)", "GF", code)) %>%
  mutate(code = ifelse(place_of_death_or_residence == "France (Saint Barthélemy)", "BL", code))

# create a new df for eu data

eu <- centenarians %>% 
  filter(place_of_death_or_residence %in% c("France", "Spain", "Italy", "Poland", "Netherlands", 
                                            "Germany", "Romania", "Portugal", "Norway")) %>% 
  mutate(name = as.factor(name)) %>% 
  mutate(code = tolower(code)) 

# create a color vector

gencol <- ifelse(eu$gender == "female", "pink", "lightblue")

# create plots

p1 <- ggplot(eu, aes(x = reorder(name, age), y = age)) +
  geom_chicklet(fill = ifelse(eu$still_alive %in% c("alive"),"#CD1818", gencol),
                width = 0.45) +
  geom_flag(aes(x = name, y = age, country = code)) +
  coord_flip() +
  labs(title = "Verfied oldest people in Europe",
       subtitle = "Oldest known european living person is highlighted by red") +
  theme(legend.position = "none",
        axis.text = element_text(face = "bold", color = "white"),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#002366", color = "#002366"),
        panel.background = element_rect(fill = "#002366", color = "#002366"),
        panel.grid = element_blank(),
        text = element_text(family = "Parchment"),
        plot.title = element_text(face = "bold",color = "white", hjust = 0.5),
        plot.subtitle = element_text(face = "bold",color = "white", hjust = 0.5)) 


p2 <- ggplot(eu, aes(x = reorder(place_of_death_or_residence, age, FUN = sum), y = age, group = gender, fill = gencol)) +
  geom_chicklet(width = 0.45) +
  scale_fill_manual(values = c("lightblue", "pink"),
                    labels= c("Male","Female"),
                    guide = guide_legend(reverse = TRUE)) +
  coord_flip() +
  labs(title = "Number of oldest people by country") +
  annotate(geom = "text",
           x = 4,
           y = 625,
           label = "There may be something \nin the Mediterranean diet",
           color = "#CD1818",
           fontface = "bold",
           size = 4) +
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold", color = "white"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#002366", color = "#002366"),
        panel.background = element_rect(fill = "#002366", color = "#002366"),
        panel.grid = element_blank(),
        text = element_text(family = "Parchment"),
        plot.title = element_text(face = "bold",color = "white", hjust = 0.5)) 

p3 <- eu %>%
  mutate(rounded_age = round(age, 1)) %>% 
  mutate(code = tolower(code)) %>%
  ggplot(aes(birth_date, rounded_age)) +
  geom_flag(aes(country = code), size = 4) +
  facet_wrap(~ gender, scales = "free_x",
             labeller = labeller(.default = stringr::str_to_title)) +
  labs(x = "Birth date",
       y = NULL,
       title = "European oldest people by gender",
       caption = "\n\n#TidyTuesday 2023 week 22 | Viz: Bernadett Piros | Source: frankiethull: Centenarians") +
  theme(legend.position = "none",
        text = element_text(family = "Parchment"),
        axis.text = element_text(face = "bold",color = "white"),
        axis.title = element_text(color = "white", size = 8),
        plot.background = element_rect(fill = "#002366", color = "#002366"),
        panel.background = element_rect(fill = "#002366", color = "white"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", color = "white", hjust = 0.5),
        plot.caption = element_text(face = "bold", color = "white", size= 6),
        
        strip.background = element_rect(fill = "#002366", color = "white"),
        strip.text = element_text(face = "bold",color = "white"))

# create a plot layout

p1+(p2/p3)

# save plot

ggsave("centenarians.png", width = 16, height = 10, dpi = 600)



