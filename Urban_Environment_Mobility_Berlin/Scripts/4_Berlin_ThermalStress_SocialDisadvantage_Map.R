# In this script the following acronym are applied:
# TS or ts = Thermal Stress
# SD or sd = Social Disadvantage

# Libraries ----
library(tidyverse)
library(readr)
library(lubridate)
library(data.table)
library(forcats)
library(sf)
library(ggplot2)
library(biscale)
library(cowplot)
library(ggspatial)

# File path ----
social_disadvantage_Berlin_2324_path <- "Data/ThermalStress_SocialVulnerability_Berlin/social_disadvantage_ua_umweltgerechtigkeit2023_e_sozial2023_WGS84.csv"
thermal_stress_Berlin_2324_path <- "Data/ThermalStress_SocialVulnerability_Berlin/thermal_stress_ua_umweltgerechtigkeit2023_d_bioklima2023_WGS84.csv"

# Importing dataset ----
df_sd_berlin_2324 <- read_csv(social_disadvantage_Berlin_2324_path,
                              locale = locale(encoding = "UTF-8"),
                              skip = 0)

glimpse(df_sd_berlin_2324)

df_ts_berlin_2324 <- read_csv(thermal_stress_Berlin_2324_path,
                              locale = locale(encoding = "UTF-8"),
                              skip = 0)

glimpse(df_ts_berlin_2324)

# Cleaning dataset ----
# PLR stands for "Property-Related Land Use" 
# Here, I will refer to them as "Territorial Units for Urban Micro-Planning",
# which corresponds to column "Territorial_Units" or "TU"
new_columns_ts <- c(ID_TU = "plr_id",
                    Territorial_Unit = "plr_name",
                    Thermal_Stress_Category = "kategorie")

df_ts_berlin_2324 <- rename(df_ts_berlin_2324, all_of(new_columns_ts))

new_columns_sd <- c(ID_TU = "plr_id",
                    Territorial_Unit = "plr_name",
                    Social_Disadvantage_Category = "kategorie")

df_sd_berlin_2324 <- rename(df_sd_berlin_2324, all_of(new_columns_sd))

# Eliminating "Status-Index"
df_sd_berlin_2324 <- df_sd_berlin_2324 %>%
  mutate(Social_Disadvantage_Category = str_remove(Social_Disadvantage_Category, "Status-Index"))

# Turning - into NA
df_sd_berlin_2324 <- df_sd_berlin_2324 %>%
  mutate(Social_Disadvantage_Category = na_if(Social_Disadvantage_Category, "-"))

# Translating from German to English
df_sd_berlin_2324 <- df_sd_berlin_2324 %>%
  mutate(
    Social_Disadvantage_Category = str_trim(Social_Disadvantage_Category),
    Social_Disadvantage_Category = case_when(
    Social_Disadvantage_Category == "hoher" ~ "high",
    Social_Disadvantage_Category == "mittlerer" ~ "medium",
    Social_Disadvantage_Category == "niedriger/sehr niedriger" ~ "low",
    .default = Social_Disadvantage_Category #to keep NA and other values
  ))
         
df_ts_berlin_2324 <- df_ts_berlin_2324 %>%
  mutate(
    Thermal_Stress_Category = str_trim(Thermal_Stress_Category),
    Thermal_Stress_Category = case_when(
      Thermal_Stress_Category == "hoch" ~ "high",
      Thermal_Stress_Category == "mittel" ~ "medium",
      Thermal_Stress_Category == "gering" ~ "low",
      .default = Thermal_Stress_Category #to keep NA and other values
    ))

# Joining Thermal Stress and Social Disadvantage datasets ----
df_all <- left_join(df_sd_berlin_2324, 
                    df_ts_berlin_2324, 
                    by = c("ID_TU", "Territorial_Unit"))

# Uploading the Berlin map (at a Territorial Units for Urban Micro-Planning level) ----
data_geojson_path <- "Data/ThermalStress_SocialVulnerability_Berlin/lor_2021_a_lor_plr_2021_WGS84.geojson"
berlin_geometries <- st_read(data_geojson_path)
plot(st_geometry(berlin_geometries))
colnames(berlin_geometries)

# Joining Berlin map data with dataset ----
berlin_map_data <- berlin_geometries %>%
  left_join(df_all, by = c("plr_id" = "ID_TU"))

# Creating Bivariate choropleth of Berlin comparing Thermal Stress and Social Disadvantage ----
#1) Converting Thermal Stress and Social Disadvantage in factors
berlin_map_data <- berlin_map_data %>%
  mutate(Social_Disadvantage_Category = factor(Social_Disadvantage_Category, 
                                               levels = c("low", "medium", "high"), 
                                               ordered = TRUE),
         Thermal_Stress_Category = factor(Thermal_Stress_Category, 
                                          levels = c("low", "medium", "high"), 
                                          ordered = TRUE))

#2) Creating bivariate column
data_bivariate <- bi_class(berlin_map_data, 
                           x = Social_Disadvantage_Category, 
                           y = Thermal_Stress_Category, 
                           dim = 3)

#3) Base map with ggplot2
berlin_map <- ggplot() +
  geom_sf(data = data_bivariate, 
          aes(fill = bi_class), 
          color = "white", 
          size = 0.05, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink2", 
                dim = 3,
                flip_axes = TRUE,
                na.value = "#E6DFB8") + 
  annotation_scale(location = "bl",
                   pad_x = unit(6.5, "cm"),
                   pad_y = unit(0.5, "cm"),
                   width_hint = 0.25,
                   unit_category = "metric",    
                   style = "ticks") +
  bi_theme() +
  labs(title = "Thermal Stress and Social Disadvantage across Berlin (2023/24)",
       subtitle = "Joint classification of neighbourhoods by thermal stress and social disadvantage",
       caption = paste(
         "Areas are classified into low, medium and high thermal stress and social disadvantage categories |",
         "Light yellow represents missing data",
         "\nSource: Senatsverwaltung für Mobilität, Verkehr, Klimaschutz und Umwelt Berlin |",
         "Indicators definition in the Data_Source.txt file",
         "\nData accessed: 22 September 2026 | Author: Davide Antoniazzi")) +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 0), 
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  colour = "black",
                                  hjust = 0),
        plot.subtitle = element_text(size = 14,
                                     face = "plain",
                                     color = "grey30",
                                     hjust = 0,
                                     margin = margin(t = 5, 
                                                     b = 10, 
                                                     l = 0, 
                                                     r = 0)),
        plot.caption = element_text(size = 9,
                                    colour = "grey50",
                                    hjust = 0))

#4) Creating 3x3 color legend
legend_bivariate <- bi_legend(pal = "GrPink2",
                              dim = 3,
                              xlab = "Social Disadvantage",
                              ylab = "Thermal Stress",
                              size = 9,
                              flip_axes = TRUE) +
  theme(axis.title.x = element_text(size = 10,
                                    face = "bold",
                                    colour = "grey20",
                                    hjust = 0,
                                    margin = margin(t = 8)),
        axis.title.y = element_text(size = 10,
                                    face = "bold",          
                                    colour = "grey20",      
                                    hjust = 0,
                                    margin = margin(r = 8)))

#5) Combining map and legend
final_map <- ggdraw() +
  draw_plot(berlin_map, 0, 0, 1, 1) +
  draw_plot(legend_bivariate, 0.72, 0.62, 0.25, 0.25)

print(final_map)

#Exporting as PDF
ggsave("Images/ThermalStress_SocialDisadvantage_Berlin_Bivariate_Map.pdf", 
       plot = final_map, 
       width = 10, 
       height = 8, 
       dpi = 300)
