# Libraries ----
library(tidyverse)
library(readr)
library(lubridate)
library(data.table)
library(stringr)
library(forcats)
library(sf)
library(patchwork)
library(ggspatial)

# File path ----
trees_Berlin_2025_path <- "Data/Trees_Distribution_Berlin_2025/baumbestand_strassenbaeume_WGS84.csv"
berlin_districts_map_path <- "Data/Trees_Distribution_Berlin_2025/alkis_bezirke_bezirksgrenzen_WGS84.geojson"

# Importing dataset ----
df_trees_berlin_2025 <- read_csv(trees_Berlin_2025_path,
                              locale = locale(encoding = "UTF-8"),
                              skip = 0)

glimpse(df_trees_berlin_2025)

berlin_districts_map <- st_read(berlin_districts_map_path)

glimpse(berlin_districts_map)

# Cleaning dataset ----
df_trees_berlin_2025 <- df_trees_berlin_2025 %>%
  select(gisid,
         art_bot,
         gattung,
         art_gruppe,
         strnr,
         strname,
         bezirk,
         lat,
         lon)

# Renaming columns
new_columns <- c(Gis_ID = "gisid",
                 Botanic_Name = "art_bot",
                 Genus = "gattung",
                 Botanic_Group = "art_gruppe",
                 Street_Number = "strnr",
                 Street_Name = "strname",
                 District = "bezirk")

df_trees_berlin_2025 <- rename(df_trees_berlin_2025, all_of(new_columns))

# Checking NAs
df_trees_berlin_2025 %>%
  summarise(across(everything(), ~ sum(is.na(.))))

df_trees_berlin_2025 <- df_trees_berlin_2025 %>%
  filter(!if_any(c(Botanic_Name, Genus, District), is.na)) #eliminating NAs in Botanic_Name, Genus, and District

# Calculating area of each district in Berlin ----
berlin_districts_map <- berlin_districts_map %>%
  mutate(Area_m2 = as.numeric(st_area(geometry)))

# Calculating amount of street trees for each district in Berlin 2025 ----
df_trees_per_district <- df_trees_berlin_2025 %>%
  group_by(District) %>%
  summarise(Tree_Count = n(), .groups = 'drop')

# Joining df_trees_per_district with berlin_districts_map and calculating density of street trees for each district ----
berlin_districts_map <- berlin_districts_map %>%
  left_join(df_trees_per_district, 
            by = c("namgem" = "District")) %>%
  mutate(Tree_Density_m2 = Tree_Count / Area_m2,
         Tree_Density_km2 = Tree_Count / (Area_m2 / 1000000))

# Calculating number of street trees by Genus in Berlin ----
df_trees_amount_berlin <- df_trees_berlin_2025 %>%
  mutate(Genus_Lumped = fct_lump_n(as.factor(Genus), 
                                   n = 9, 
                                   other_level = "Other")) %>%
  count(Genus_Lumped, name = "Tree_Amount") %>%
  mutate(Genus_Lumped = fct_reorder(Genus_Lumped, Tree_Amount),
         Genus_Lumped = fct_relevel(Genus_Lumped, "Other", after = 0))

# Creating the lollipop plot ----
plot_lollipop <- ggplot(df_trees_amount_berlin, aes(x = Tree_Amount, y = Genus_Lumped)) +
  geom_segment(aes(x = 0,
                   y = Genus_Lumped,
                   xend = Tree_Amount, 
                   yend = Genus_Lumped), 
               color = "#8d745e", 
               linewidth = 0.9) +
  geom_point(color = "#4a3b32", 
             size = 3.5) +
  geom_text(aes(label = scales::comma(Tree_Amount)), 
            hjust = -0.3, 
            size = 3.5, 
            fontface = "bold",
            color = "#33261c") +
  labs(title = "Most Common Street-Tree Genera in Berlin",
       subtitle = "Top 9 most frequent genera; remaining taxa aggregated under 'Other'",
       x = "Absolute Abundance (Count)",
       y = "Genus") +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11, 
                                   face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10), 
                                    size = 10),
        axis.title.y = element_text(margin = margin(r = 10), 
                                    size = 10),
        plot.title = element_text(face = "bold", 
                                  size = 12))

# Choropleth map of density of street trees in Berlin 2025 per District ----
plot_map <- ggplot(data = berlin_districts_map) +
  geom_sf(aes(fill = Tree_Density_km2), 
          color = "white", 
          linewidth = 0.2) +
  scale_fill_distiller(palette = "YlGn", 
                       direction = 1,
                       limits = c(200, 900),
                       breaks = seq(200, 900, length.out = 6),
                       labels = scales::comma,
                       guide = guide_colorbar(barwidth = 20, barheight = 1)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.3, 
                   text_size = 8,
                   style = "ticks") +
  labs(title = "Spatial Density of Street Trees by Administrative District",
       subtitle = bquote("Number of street trees per km"^2*" of district area"),
       fill = bquote("Trees/km"^2*"")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", 
                                  size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(t = 15))

# Joining the two maps/visualizations ----
combined_plot <- plot_lollipop | plot_map

final_plot <- combined_plot + plot_annotation(
  title = "Quantitative and Spatial Assessment of Berlin Street Trees (2025)",
  subtitle = "Street-tree abundance by genus and spatial density across Berlin's administrative districts",
  caption = paste(
    "Source: Senatsverwaltung für Mobilität, Verkehr, Klimaschutz und Umwelt Berlin | ",
    "Data accessed: 22 September 2026 | Author: Davide Antoniazzi"),
  theme = theme(plot.title = element_text(face = "bold",
                                          color = "grey10",
                                          size = 15, 
                                          hjust = 0.5),
                plot.subtitle = element_text(size = 11, 
                                             hjust = 0.5, 
                                             face = "italic",
                                             color = "grey30"),
                plot.caption = element_text(size = 8,
                                            colour = "grey50",
                                            hjust = 0,
                                            face = "italic")))

final_plot

# Saving
ggsave("Images/Berlin_Street_Trees_Analysis.pdf", 
       plot = final_plot,
       width = 14,
       height = 7, 
       dpi = 300)
