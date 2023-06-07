# TidyTuesday | 2023-03-14| European Drug Development
# Data Source: European Medicines Agency

# Load libraries
library(tidyverse)
library(patchwork)
library(scico)

# Load data
tuesdata <- tidytuesdayR::tt_load('2023-03-14')
drugs <- tuesdata$drugs

# Summarise biggest holders
biggest_holders_by_category <- drugs %>%
  group_by(category) %>% 
  mutate(marketing_authorisation_holder_company_name = fct_lump(marketing_authorisation_holder_company_name, n = 5)) %>%
  count(marketing_authorisation_holder_company_name, sort = TRUE) %>% 
  drop_na(marketing_authorisation_holder_company_name) %>% 
  filter(marketing_authorisation_holder_company_name != "Other") %>% 
  arrange(category)

# Create plot
p_drugs <- ggplot(biggest_holders_by_category,
                  aes(x = n,
                      y = fct_infreq(fct_na_value_to_level(marketing_authorisation_holder_company_name)),  
                      fill = category)) + 
  geom_col(width = 0.75) + 
  scale_fill_scico_d(palette = "lisbon") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 6, color = "#F2F2F2", hjust = 0.5),
        axis.text = element_text(size = 8, face = "bold", color = "#F2F2F2"),
        axis.line = element_line(linewidth = 0.2, color = "#F2F2F2"),
        axis.ticks.x = element_line(linewidth = 0.9, color = "#F2F2F2"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8, face = "bold", color = "#F2F2F2"),
        plot.margin = unit(c(1.5, 1.5, 1, 1.5), "cm"),
        plot.background = element_rect(color = NA, fill = "seagreen"),
        text = element_text(family = "Courier New")) +
  labs(title = "The biggest drug authorization holders by category",
       x = NULL, y = NULL, caption = "\n\n#TidyTuesday 2023 week 11 | Viz: Bernadett Piros \n Source: European Medicines Agency") 

# Save plot
ggsave(paste0("drugs", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320)