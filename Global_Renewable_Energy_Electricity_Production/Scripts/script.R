# SCRIPT CONTENTS
# 1) Libraries
# 2) Data import + cleaning
# 3) FIRST VISUALIZATION: Electricity production share from renewables
# 4) SECOND VISUALIZATION: Global Share Electricity Source Production
# 5) THIRD VISUALIZATION: G20 Dumbell Renewables Chart
# 6) FOURTH VISUALIZATION: Scatter Renewable Share - GDP per capita

# 1) Libraries
library(tidyverse)
library(forcats)
library(ggrepel)
library(scales)
library(ggthemes)
library(stringr) #to quick modify text
library(ggalt)
library(gridExtra)
library(grid)
library(countrycode)

# 2) Data import + cleaning
# Source download website: 
#https://github.com/owid/energy-data?tab=readme-ov-file (CSV format)
df <- read.csv('Data/owid-energy-data.csv')

df %>% View()

excluded <- c("Africa", "American Samoa", "Antarctica", "Aruba", "Asia", 
              "British Virgin Islands", "Cayman Islands", "Cook Islands", 
              "Czechoslovakia", "Faroe Islands", "French Polynesia", 
              "East Germany", "Europe", "European Union (27)", "Falkland Islands",
              "French Guiana", "Gibraltar", "Guadeloupe", "Guam", "Greenland", 
              "Hong Kong", "Macao", "Martinique", "Montserrat", "New Caledonia", 
              "Netherlands Antilles", "North America", "Niue", 
              "Northern Mariana Islands", "Oceania", "Reunion", "Saint Helena", 
              "Saint Pierre and Miquelon", "Serbia and Montenegro", "South America",
              "Turks and Caicos Islands", "USSR", "United States Virgin Islands",
              "West Germany", "Western Sahara", "World", "Yugoslavia")

g20_countries <- c(
  "United States", "China", "Japan", "Germany", "United Kingdom", 
  "France", "Italy", "India", "Brazil", "Russia",
  "Canada", "Mexico", "Argentina", "Australia", "Indonesia",
  "Saudi Arabia", "South Korea", "Turkey", "South Africa")

filt_df <- df %>%
  filter(!str_detect(country, "Ember|EI|EIA|Shift|income"),
         !country %in% excluded,
         !country %in% c("Curacao", "Micronesia (country)",
                         "Tuvalu")) # No data

# 3) FIRST VISUALIZATION: Electricity production share from renewables
renewable_share_df <- filt_df %>%
  filter(year >= 1990)%>%
  select(country, 
         iso_code, 
         year, 
         renewables_share_elec) %>%
  #To remove NA values
  drop_na(renewables_share_elec)

# Grouping the countries
renewable_share_grouped <- renewable_share_df %>%
  mutate(country_lump = fct_other(
    country, 
    keep = g20_countries, 
    other_level = "Other"))

renewable_share_plot <- renewable_share_grouped %>%
  group_by(country_lump, year) %>%
  summarise(renewables_share_elec_plot = mean(renewables_share_elec), 
            .groups = "drop")

# Plotting
p1 <- renewable_share_plot %>%
  ggplot(aes(year, renewables_share_elec_plot))+
  geom_line(linewidth=0.9,
            lineend = "round",
            color = "#2E8B57")+
  facet_wrap(~country_lump,
             ncol = 5)+
  labs(title = expression("Renewable Share of Electricity Generation by Country (1990-2025)"),
       subtitle = paste(
         "Values represent the share of total electricity generation.",
         "'Other' represents the average of all remaining independent countries.*",
         sep = "\n"),
       x="Year",
       y="%",
       caption = paste("Source: Our World in Data (Energy dataset)",
                       "*Curacao, Micronesia, and Tuvalu were excluded because no data are available.",
                       sep = "\n"))+
  theme_bw()+
  theme(plot.title = element_text(size=24, 
                                  face="bold"),
        plot.subtitle = element_text(size=14, 
                                     hjust = 0),
        axis.text = element_text(size=11),
        axis.title = element_text(size=13),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10),
        strip.text.x = element_text(size = 13),
        panel.grid.major = element_line(linewidth = .2),
        panel.grid.minor = element_line(linewidth = .2),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_rect(fill="grey95"))

p1

#ggsave('Images/renewable_electricity_share.pdf',
#       plot = p1,
#       width = 13,
#       height = 8,
#       dpi = 300,
#       bg = "white")

# 4) SECOND VISUALIZATION: Global Share Electricity Source Production
perc_elec_sources <- df %>%
  filter(country == "World") %>%
  filter(year > 1984) %>%
  select(country, 
         year,
         coal_share_elec,
         gas_share_elec,
         oil_share_elec,
         hydro_share_elec,
         nuclear_share_elec,
         wind_share_elec,
         solar_share_elec,
         other_renewables_share_elec) %>%
  rename(Coal = coal_share_elec,
         Gas = gas_share_elec,
         Oil = oil_share_elec,
         Hydro = hydro_share_elec,
         Nuclear = nuclear_share_elec,
         Wind = wind_share_elec,
         Solar = solar_share_elec,
         Other_Renewables = other_renewables_share_elec) %>%
  pivot_longer(
    cols = c(Coal, 
             Gas,
             Oil,
             Hydro, 
             Nuclear,
             Wind,
             Solar,
             Other_Renewables),
    names_to = "Source",
    values_to = "Share_electricity_generated")

perc_elec_sources %>% View()

# Plotting
perc_elec_sources <- perc_elec_sources %>%
  mutate(Source = factor(Source, levels = c(
    "Other_Renewables",
    "Solar",
    "Wind",
    "Hydro",
    "Nuclear",
    "Oil",
    "Gas",
    "Coal")))

sources_fill <- c(
  "Coal" = "#2B2B2B", 
  "Gas" = "#E69F00", 
  "Oil" = "#665243", 
  "Hydro" = "#1F77B4",
  "Nuclear" = "#9467BD",
  "Wind" = "#41B6C4",
  "Solar" = "#FFD700",
  "Other_Renewables" = "#2CA02C")

p2 <- perc_elec_sources %>% 
  group_by(year, Source) %>%
  ggplot(aes(x = year, 
             y = Share_electricity_generated, 
             fill = Source)) +
  geom_area(position = "stack",
            alpha = 0.9)+
  scale_fill_manual(values = sources_fill,
                    labels = function(x) str_replace_all(x, "_", " "))+
  scale_x_continuous(
    limits = c(1985, 2025),
    breaks = seq(1985, 2025, by = 5))+
  labs(title=expression("Global Share of Electricity Generation by Source (1985-2025)"),
       x= NULL,
       y= "Share of Global Electricity Generation [%]",
       caption = expression("Source: Our World in Data (Energy dataset)"))+
  theme_classic()+
  theme(plot.title = element_text(size=24, 
                                  face="bold",
                                  margin = margin(b = 15)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14,
                                    margin = margin(r = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10,
                                    hjust = 0),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.8,"cm"),
        legend.spacing.x = unit(0.3,"cm"))+
  guides(fill = guide_legend(nrow = 1, 
                             byrow = TRUE,
                             reverse = TRUE,
                             keywidth = unit(1.2,"cm"),
                             keyheight = unit(0.5,"cm")))

p2

#ggsave('Images/Electricity_production_sources.pdf',
#       plot = p2,
#       width = 13,
#      height = 8,
#       dpi = 300,
#       bg = "white")


# 5) THIRD VISUALIZATION: G20 Dumbell Renewables Chart
g20_countries_df <- df %>%
  filter(country %in% g20_countries | country == "Africa" | country == "European Union (27)") %>%
  filter(year == 1995 | year == 2023) %>%
  select(country, 
         year, 
         renewables_share_elec) %>%
  mutate(renewables_share_elec = round(renewables_share_elec, 2))

# Wide format data
wide_g20_countries_df <- pivot_wider(g20_countries_df, 
                         names_from = year, 
                         values_from = c(renewables_share_elec),
                         names_glue = "{.value}_{year}")

wide_g20_countries_df <- wide_g20_countries_df %>%
  mutate(country = fct_reorder(country, renewables_share_elec_2023))

# Plotting
p3 <- wide_g20_countries_df %>%
  ggplot(aes(y = country)) +
  geom_dumbbell(aes(x = renewables_share_elec_1995,
                    xend = renewables_share_elec_2023),
                colour = "#BDBDBD",
                colour_x = "#7A7A7A",
                colour_xend = "#2C7FB8",
                size = 1.2,
                size_x = 4,
                size_xend = 4) +
  # Invisible points used only to create the legend
  geom_point(aes(x = renewables_share_elec_1995,
                 colour = "1995"),
             size = 3.5) +
  geom_point(aes(x = renewables_share_elec_2023,
                 colour = "2023"),
             size = 3.5) +
  scale_colour_manual(name = NULL, values = c("1995" = "#7A7A7A",
                                                "2023" = "#2C7FB8")) +
  labs(title = "Renewable Electricity Generation in G20 Countries",
       subtitle = "Change in the share of electricity generated from renewables, 1995-2023",
       x = "Electricity Generated by Renewables [%]",
       y = NULL,
       caption = "Source: Our World in Data, Energy dataset. Africa and European Union (27) included.") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(size = 12,
                                face = "bold",
                                margin = margin(t = 10)),
    axis.text.x = element_text(size = 10,
                               colour = "grey40"),
    axis.text.y = element_text(size = 11,
                               colour = "grey20"),
    plot.title = element_text(size=20, 
                              face="bold"),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 9,
                                colour = "grey50",
                                hjust = 0))

p3

#ggsave('Images/Renewable_dumbbell_g20.pdf',
#       plot = p3,
#       width = 13,
#       height = 8,
#       dpi = 300,
#       bg = "white")

# 6) FOURTH VISUALIZATION: Scatter Renewable Share - GDP per capita
renewable_gdp_capita_df <- filt_df %>%
  filter(year == 2022)%>%
  select(country, 
         iso_code,
         year, 
         population,
         gdp,
         renewables_share_elec) %>%
  drop_na(gdp, renewables_share_elec) %>%
  mutate(renewables_share_elec = round(renewables_share_elec, 2),
         gdp_per_capita = round(gdp/population, 2),
         continent = countrycode(iso_code, 
                                 origin = "iso3c", 
                                 destination = "continent"))

pop_breaks <- c(1e6,5e6,25e6,100e6,500e6,1e9)

p4 <- renewable_gdp_capita_df %>%
  ggplot(aes(gdp_per_capita, renewables_share_elec,
             size = population,
             fill = continent))+
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
                        name = "Population",
                        labels = function(x) paste0(round(x / 1e6, 0), "M"))+
  scale_fill_manual(values = c(
    "Africa" = "#545252",
    "Americas" = "#E76F51",
    "Asia" = "#E9C46A",
    "Europe" = "#86B886",
    "Oceania" = "#2275A8"),
    name="Continent")+
  labs(title="Per Capita GDP and Electricity Share Generated by Renewables (2022)",
       subtitle = "Countries are sized by population and coloured by continent",
       x="GDP per capita [international-$ in 2011 prices, log-scale]",
       y="Electricity Share Generated by Renewables [%]",
       caption=paste("Source: Our World in Data (Energy dataset)",
                     "26 Countries Excluded due to GDP 2022 Data Unavailable",
                     sep = "\n"))+
  geom_text_repel(data = subset(renewable_gdp_capita_df,
                                country %in% c("United States","China",
                                               "India","Germany",
                                               "Brazil","Russia")),
                  aes(label = iso_code),
                  size = 3,
                  color="black")+
  theme_classic()+
  theme(plot.title = element_text(size=20, 
                                  face="bold"),
        plot.subtitle = element_text(size = 13, color = "grey30"),
        axis.text = element_text(size=11),
        axis.title = element_text(size = 13, 
                                  face = "bold"),
        plot.caption = element_text(size=9,
                                    colour = "grey50",
                                    hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_text(size = 12, 
                                    face = "bold"),
        legend.text = element_text(size=11),
        legend.position = "right",
        panel.grid.major = element_line(color = "grey85",
                                        linewidth = 0.3),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(order = 1,
                             override.aes = list(size = 4)),
         size = guide_legend(order = 2,
                             title = "Population"))

p4

#ggsave('Images/Scatter_Renewable_Share_GDPcapita.pdf',
#       plot = p4,
#       width = 13,
#       height = 8,
#       dpi = 300,
#       bg = "white")