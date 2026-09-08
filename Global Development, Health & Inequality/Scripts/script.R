# SCRIPT CONTENTS
# 1) Libraries
# 2) Data import + cleaning
# 3) FIRST VISUALIZATION: Infant (Under 5) Mortality Line Chart
# 4) SECOND VISUALIZATION: Life Expectancy Distribution with Ridgeline Plot 
# 5) THIRD VISUALIZATION: GDP per capita-Life Expectancy Scatter Plot
# 6) FOURTH VISUALIZATION: Wordwide Electricity Access Map

# 1) Libraries
library(tidyverse)
library(data.table)
library(countrycode)
library(stringr)
library(ggrepel)
library(scales)
library(ggridges)
library(ggExtra)
library(sf)
library(rnaturalearth)

options(scipen = 999)

# 2) Data import + cleaning
new_columns <- c("Country"="Country Name",
                 "Code"="Country Code",
                 "Series"="Series Name",
                 "Series_Code"="Series Code")

# Mortality under 5 dataset import
mortality_rate <- data.table::fread('Data/Mortality_Younger_5/Mortality_younger_5.csv',
                        drop='1989 [YR1989]')
mortality_rate <- mortality_rate[1:11,]
mortality_rate <- mortality_rate %>% 
  rename_with(~ str_extract(., "^\\d{4}"), .cols = contains("[")) %>%
  arrange(`Country Name`)
mortality_rate <- rename(mortality_rate, all_of(new_columns))

# Life_Expectancy dataset import
life_expectancy <- data.table::fread('Data/Life_Expectancy/Life_Expectancy.csv')
life_expectancy <- life_expectancy[1:194,]
life_expectancy <- life_expectancy %>% 
  rename_with(~ str_extract(., "^\\d{4}"), .cols = contains("["))
life_expectancy <- rename(life_expectancy, all_of(new_columns))
life_expectancy <- life_expectancy %>%
  mutate(`1960`= as.numeric(`1960`),
         `1980`= as.numeric(`1980`))

# GDP per capita and Life Expectancy dataset import
GDP_capita_Life_Expec <- data.table::fread('Data/GDPCapita_LifeExpectancy/GDPCapita_LifeExpectancy.csv')
GDP_capita_Life_Expec <- GDP_capita_Life_Expec[1:579,]
GDP_capita_Life_Expec <- GDP_capita_Life_Expec %>% 
  rename_with(~ str_extract(., "^\\d{4}"), .cols = contains("["))
GDP_capita_Life_Expec <- rename(GDP_capita_Life_Expec, all_of(new_columns))
GDP_capita_Life_Expec <- GDP_capita_Life_Expec %>%
  mutate(`2023`= as.numeric(`2023`),
         Region = countrycode(Code, 
                              origin = "iso3c", 
                              destination = "region"),
         Region = if_else(Country == "Kosovo" & is.na(Region), 
                          "Europe & Central Asia", 
                          Region),
         Region = if_else(Country == "Puerto Rico (US)" & is.na(Region), 
                          "Latin America & Caribbean", 
                          Region))

# Electricity Access dataset import
Elect_Acc <- data.table::fread('Data/Access_Electricity/Electricity_Access_Data.csv')
Elect_Acc <- Elect_Acc[1:194,]
Elect_Acc <- Elect_Acc %>% 
  rename_with(~ str_extract(., "^\\d{4}"), .cols = contains("["))
Elect_Acc <- rename(Elect_Acc, all_of(new_columns))
Elect_Acc <- Elect_Acc %>% 
  rename(Year_2023 = `2023`) %>% 
  mutate(Year_2023 = as.numeric(Year_2023)) %>%
  filter(!grepl("Kosovo", Country)) # no data for Kosovo


# 3) FIRST VISUALIZATION: Infant (Under 5) Mortality Line Chart
# Long format
mortality_rate_long <- mortality_rate %>%
  pivot_longer(cols = `1990`:`2024`,
               names_to = "Year",
               values_to = "Mortality_Rate") %>%
  mutate(Year = as.numeric(Year))

# Plotting
p1 <- mortality_rate_long %>%
  ggplot(aes(Year, Mortality_Rate))+
  geom_line(linewidth=0.7,
            lineend = "round")+
  facet_wrap(~~str_wrap(Country, width = 30), ncol = 4)+
  labs(title=expression("Under-5 Mortality Rate by World Region (1990 - 2024)"),
       subtitle ="Deaths per 1,000 live births",
       x=NULL,
       y="Mortality Rate",
       caption = "Source available in the Notes.txt file")+
  theme_classic()+
  theme(plot.title = element_text(size=24, face="bold"),
        plot.subtitle = element_text(size=15, 
                                     color = "grey30"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        plot.caption.position = "plot",
        plot.caption = element_text(size=9,
                                    colour = "grey50",
                                    hjust=0),
        strip.text = element_text(size = 11, 
                                    face = "bold"),
        strip.background = element_rect(fill = "grey95",
                                        colour = NA),
        panel.grid.major = element_line(color = "grey88",
                                        linewidth = 0.25),
        panel.grid.minor = element_blank())

p1

#ggsave('Images/Under5_Mortality_Rate.pdf',
#  plot = p1,
#  width = 13,
#  height = 8,
#  dpi = 300,
#  bg = "white")

# 4) SECOND VISUALIZATION: Life Expectancy Distribution with Ridgeline Plot
#Long format + Region
life_expectancy_long <- life_expectancy %>%
  pivot_longer(cols = `1960`:`2023`,
               names_to = "Year",
               values_to = "Life_Expectancy") %>%
  mutate(Region = countrycode(Code, 
                              origin = "iso3c", 
                              destination = "region"),
         Region = if_else(Country == "Kosovo" & is.na(Region), 
                          "Europe & Central Asia", 
                          Region),
         Region = if_else(Country == "Puerto Rico (US)" & is.na(Region), 
                          "Latin America & Caribbean", 
                          Region))

# Plotting
p2 <- ggplot(life_expectancy_long, 
       aes(x = Life_Expectancy, y = Year, fill = Region)) +
  geom_density_ridges(alpha = 0.6,
                      color = "gray20",
                      scale = 1.4, 
                      rel_min_height = 0.01) +
  scale_fill_viridis_d(option = "viridis") + #Colour Map
  labs(title = "Life Expectancy Distribution Over Time by World Region (1960-2023)",
       subtitle = "Comparison of Years 1960-1980-2000-2023",
       x = "Life Expectancy at Birth (Years)",
       y = NULL,
       caption = "Source available in the Notes.txt file",
       fill = NULL) +
  theme_ridges() + #Specific theme 
  theme(plot.title = element_text(size=24, face="bold"),
        plot.subtitle = element_text(size=15, 
                                     color = "grey30"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        plot.caption = element_text(size=9,
                                    colour = "grey50",
                                    hjust=0),
        strip.text = element_text(size = 11, 
                                  face = "bold"),
        strip.background = element_rect(fill = "grey95",
                                        colour = NA),
        panel.grid.major = element_line(color = "grey88",
                                        linewidth = 0.25),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 11))+
  guides(fill = guide_legend(nrow = 1))

p2

#ggsave('Images/Life_Expectancy_RidgePlot.pdf',
#  plot = p2,
#  width = 13,
#  height = 8,
#  dpi = 300,
#  bg = "white")
                
# 5) THIRD VISUALIZATION: GDP per capita-Life Expectancy Scatter Plot
# Wide format
GDP_capita_Life_Expec_wide <- GDP_capita_Life_Expec %>%
  select(Country, Code, Region, Series, `2023`) %>%
  pivot_wider(names_from = Series,
              values_from = `2023`) %>%
  rename(gdp_pc = `GDP per capita (constant 2015 US$)`,
         life_exp = `Life expectancy at birth, total (years)`,
         pop_tot = `Population, total`)

# Plotting
pop_breaks <- c(1e6,5e6,25e6,100e6,500e6,1e9)

p3 <- GDP_capita_Life_Expec_wide %>% 
  ggplot(aes(gdp_pc, life_exp, size = pop_tot, fill = Region))+
  geom_point(alpha=0.65,
             shape=21,
             stroke=0.5,
             color="grey20")+
  geom_smooth(aes(group = 1),
              method = "lm",
              se = TRUE,
              color = "grey25",
              fill = "grey75",
              linewidth = 0.8,
              show.legend = FALSE)+
  scale_x_log10(labels = scales::label_dollar())+
  scale_size_continuous(range = c(2, 11),
                        breaks = rev(pop_breaks),
                        name = "pop_tot",
                        labels = function(x) paste0(round(x / 1e6, 0), "M"))+
  scale_fill_manual(values = c(
    "South Asia" = "#2CA02C",
    "Europe & Central Asia" = "#1F77B4",
    "Middle East & North Africa" = "#FF7F0E",
    "Sub-Saharan Africa" = "#8C564B",
    "Latin America & Caribbean" = "#D62728",
    "East Asia & Pacific"="#9467BD",
    "North America"="#7F7F7F"),
    name="Region")+
  labs(title="GDP Per Capita and Life Expectancy (2023)",
       subtitle = "Countries are sized by population and coloured by region",
       x="GDP per capita [US-$ in 2015 prices, log-scale]",
       y="Life Expectancy [Years]",
       caption="Source available in the Notes.txt file")+
  theme_classic()+
  theme(plot.title = element_text(size=20, 
                                  face="bold"),
        plot.subtitle = element_text(size = 13, 
                                     color = "grey30"),
        axis.text = element_text(size=11),
        axis.title = element_text(size = 13, 
                                  face = "bold"),
        plot.caption = element_text(size=9,
                                    colour = "grey50",
                                    hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_text(size = 10, 
                                    face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "inside",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        panel.grid.major = element_line(color = "grey85",
                                        linewidth = 0.3),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(order = 1,
                             ncol = 2,
                             override.aes = list(size = 3),
                             position = "inside",
                             theme = theme(legend.position.inside = c(0.18, 0.76))),
         size = guide_legend(order = 2,
                             title = "Population",
                             position = "inside",
                             theme = theme(legend.position.inside = c(0.88, 0.22))))

#Marginal Density
p3_marginal <- ggMarginal(p3, 
                          type = "density", 
                          fill = "grey80", 
                          color = "grey50",
                          size = 4)

#ggsave('Images/GDPcapita_LifeExpectancy_Marginal.pdf',
#  plot = p3_marginal,
#  width = 13,
#  height = 8,
#  dpi = 300,
#  bg = "white")


# 6) FOURTH VISUALIZATION: Worldwide Electricity Access Map
# Borders as sf object
world_borders <- ne_countries(scale = "medium",
                             returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  mutate(iso_a3 = case_when(name == "Norway" ~ "NOR",
                            name == "France" ~ "FRA",
                            TRUE ~ iso_a3))

# Combining borders with Access to Electricity dataset
Access_Elect_map <- world_borders %>%
  select(iso_a3, name, geometry) %>%
  left_join(Elect_Acc, by = c("iso_a3" = "Code"))

# Plotting
p4 <- ggplot(data=Access_Elect_map)+
  geom_sf(aes(fill = Year_2023), 
          color = "white",
          size = 0.1)+
  coord_sf(crs = "+proj=robin")+
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "#e0e0e0",
    labels = scales::label_percent(scale = 1),
    name = "Access to Electricity")+
  theme_minimal()+
  labs(title = "Access to Electricity Worldwide (2023)",
       subtitle = "Share of the Population with Access to Electricity",
       caption = "Source available in the Notes.txt file | Grey countries indicate missing data")+
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5),
    plot.subtitle = element_text(size = 11, 
                                 hjust = 0.5, 
                                 color = "gray30"),
    plot.caption = element_text(size=9,
                                colour = "grey50",
                                hjust = 0),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.background = element_rect(fill = "#f7fbff", 
                                    color = NA))

p4

#ggsave('Images/AccessElectricity_Map.pdf',
#  plot = p4,
#  width = 13,
#  height = 8,
#  dpi = 300,
#  bg = "white")
