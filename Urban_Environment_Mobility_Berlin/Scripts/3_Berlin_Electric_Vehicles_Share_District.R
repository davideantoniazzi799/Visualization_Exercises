# Libraries ----
library(tidyverse)
library(readr)
library(lubridate)
library(data.table)
library(forcats)

# File path ----
vehicles_Berlin_2025_path <- "Data/Electric_vehicles_Berlin/ea_elektrofahrzeuge_kfzbestand_bez2025_WGS84.csv"

# Importing dataset ----
df_ev_berlin_2025 <- read_csv(vehicles_Berlin_2025_path,
                                locale = locale(encoding = "UTF-8"),
                                skip = 0)

glimpse(df_ev_berlin_2025)

# Cleaning dataset ----
df_ev_berlin_2025 <- df_ev_berlin_2025 %>% 
  select(id_bezirk, 
         bezirk, 
         jahr,
         anteilelektrogesamt,
         anteilkeinelektrogesamt,
         anteil_kfz_sonstige_proz)

# Only needed columns regarding share of EV and other vehicles
new_columns <- c(ID = "id_bezirk", 
                 District = "bezirk",
                 Year = "jahr", 
                 EV_Share = "anteilelektrogesamt", 
                 Not_Ev_Share = "anteilkeinelektrogesamt", 
                 Other_share = "anteil_kfz_sonstige_proz")

df_ev_berlin_2025 <- rename(df_ev_berlin_2025, all_of(new_columns))

# Calculating average EV share in Berlin ----
ev_share_berlin_avg_2025 <- round(mean(df_ev_berlin_2025$EV_Share), 2)

# Setting the theme for the plot ----
theme_set(theme_bw() + 
            theme(panel.grid = element_blank()))

# Plotting: Lollipop Graph of EV Share in Berlin districts ----
ev_plot <- df_ev_berlin_2025 %>%
  mutate(District = reorder(District, -EV_Share)) %>%
  ggplot(aes(x = District, y = EV_Share)) +
  geom_segment(aes(x=District, 
                   y=ev_share_berlin_avg_2025, 
                   xend = District, 
                   yend = EV_Share), 
               colour = "grey") +
  geom_point(size = 4, 
             colour = "#D95F02") +
  geom_hline(yintercept = ev_share_berlin_avg_2025,
             colour = "grey20",
             linewidth = 0.8,
             linetype = 2) + # dashed
  geom_text(aes(label = round(EV_Share, 1),
                vjust = ifelse(EV_Share >= ev_share_berlin_avg_2025, -0.85, 1.7)),
            size = 3.5) +
  scale_y_continuous(limits = c(3, 9),
                     breaks = 3:9,
                     labels = 3:9) +
  labs(title = "Electric Vehicle Share across Berlin Districts (2025)",
       subtitle = bquote("Berlin-wide mean in 2025:"~ .(ev_share_berlin_avg_2025) ~ 
                           "% | Dashed line indicates the city-wide average"),
       x = "Berlin Districts",
       y = "Electric Vehicle Share [%]",
       caption = paste(
         "Source: Source: Amt für Statistik Berlin-Brandenburg/Elektrofahrzeuge |",
         "Electric Vehicles includes Battery Electric and Plug-In Hybrid Vehicles | Y-axis truncated to improve visibility of differences across districts",
         "\nData accessed: 22 September 2026 | Author: Davide Antoniazzi")) +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 13,
                                    face = "bold"),
        axis.title.y = element_text(size = 13,
                                    face = "bold"),
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  colour = "black",
                                  hjust = 0),
        plot.subtitle = element_text(size = 14,
                                     face = "plain",
                                     color = "grey30",
                                     hjust = 0),
        plot.caption = element_text(size = 9,
                                    colour = "grey50",
                                    hjust = 0))

ev_plot

# Saving in PDF format
ggsave("Images/Electric_Vehicles_Berlin_2025_Lollipop.pdf",
       ev_plot,
       width = 13,
       height = 10,
       units = "in")
