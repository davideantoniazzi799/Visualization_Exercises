# SCRIPT CONTENTS
# 1) Libraries
# 2) Data import + cleaning
# 3) FIRST VISUALIZATION: Per Capita CO2 Source Emissions
# 4) SECOND VISUALIZATION: INDEXED GDP AND GREENHOUSE GASES EMISSIONS
# 5) THIRD VISUALIZATION: BUBBLE CHART CO2-GDP PER CAPITA
# 6) FOURTH VISUALIZATION: DUMBBELL CHART ENERGY AND GHG PER CAPITA


# 1) Libraries
library(tidyverse)
library(ggrepel)
library(scales)
library(countrycode)

# 2) Data import + cleaning
df <- read.csv('Data/owid-co2-data.csv')

df %>% View()

filt_df <- df %>%
  filter(!str_detect(country, "(GCP)")) %>%
  filter(!str_detect(country, "Europe")) %>%
  filter(!str_detect(country, "countries")) %>%
  filter(!str_detect(country, "Asia")) %>%
  filter(!country %in% c("Africa", "Antarctica", "Anguilla", "Aruba", 
                         "Bermuda", "Bonaire Sint Eustatius and Saba", 
                         "British Virgin Islands", "Christmas Island", 
                         "Cook Islands", "Faroe Islands", "French Polynesia",
                         "Greenland", "Hong Kong", "International aviation", 
                         "International shipping", "Kuwaiti Oil Fires", 
                         "Macao", "Montserrat", "New Caledonia", 
                         "North America", "North America (excl. USA)", "Niue",
                         "OECD (Jones et al.)", "Oceania", "Ryukyu Islands",
                         "Saint Helena", "Saint Pierre and Miquelon",
                         "Sint Maarten (Dutch part)", "South America",
                         "Turks and Caicos Islands", "Wallis and Futuna"))

filt_df <- filt_df %>%
  mutate(continent = countrycode(iso_code, origin = "iso3c", destination = "continent"))


# 3) FIRST VISUALIZATION: Per Capita CO2 Source Emissions
# Pivot longer table
material_co2_per_capita <- df %>%
  filter(country=='World',
         year > 1799) %>%
  select(year, 
         cement_co2_per_capita,
         coal_co2_per_capita,
         flaring_co2_per_capita,
         gas_co2_per_capita, 
         oil_co2_per_capita,
         other_co2_per_capita) %>%
  rename(Cement = cement_co2_per_capita,
         Coal = coal_co2_per_capita,
         Flaring = flaring_co2_per_capita,
         Gas = gas_co2_per_capita,
         Oil = oil_co2_per_capita,
         Other = other_co2_per_capita)%>%
  pivot_longer(
    cols = c(Cement, 
             Coal,
             Flaring,
             Gas, Oil,
             Other),
    names_to = "Source",
    values_to = "co2_per_capita")

# To hide NA values
start_years <- material_co2_per_capita %>%
  group_by(Source) %>%
  summarise(first_year = min(year[!is.na(co2_per_capita)]))

material_co2_per_capita <- material_co2_per_capita %>%
  left_join(start_years, by = "Source") %>%
  filter(year >= first_year)

# Transform 0z in Other in NA
material_co2_per_capita <- material_co2_per_capita %>%
  group_by(Source) %>%
  mutate(co2_plot = ifelse(cumsum(co2_per_capita > 0) == 0, NA, co2_per_capita)) %>%
  ungroup()

# Plotting
p1 <- material_co2_per_capita %>%
  ggplot(aes(year, co2_plot,
             color=Source))+
  geom_line(linewidth=0.7,
            lineend = "round")+
  facet_wrap(~Source, scales = "free")+
  scale_color_manual(values = c(
    "Cement" = "#A9AFBA", 
    "Coal" = "#242829", 
    "Flaring" = "#C49104", 
    "Gas" = "#4672B3",
    "Oil" = "#399BAD",
    "Other" = "#912511"))+
  labs(title=expression("Worldwide Per Capita Source CO"[2] * " Emissions"),
       subtitle = "Each panel uses its own axis scales to better highlight the temporal evolution of each emission source.",
       x="Year",
       y="Tonnes per person",
       caption = expression("Source: Our World in Data (CO"[2] * " dataset)"))+
  theme_bw()+
  theme(plot.title = element_text(size=24, face="bold"),
        plot.subtitle = element_text(size=16),
        axis.text = element_text(size=11),
        axis.title = element_text(size=13),
        plot.caption.position = "plot",
        plot.caption = element_text(size=10),
        strip.text.x = element_text(size = 13),
        panel.grid.major = element_line(linewidth = .2),
        panel.grid.minor = element_line(linewidth = .2))+
  guides(color="none")

p1

#ggsave('Images/co2_per_capita_sources.pdf',
#  plot = p1,
#  width = 13,
#  height = 8,
#  dpi = 300,
#  bg = "white")


# 4) SECOND VISUALIZATION: INDEXED GDP AND GREENHOUSE GASES EMISSIONS
"The analysis includes sovereign countries only. 
Overseas territories and special administrative regions were excluded 
to ensure comparability across national observations."

gdp_emissions_df <- filt_df %>%
  select(country, year, iso_code, gdp, total_ghg) 

# Selecting top 10 countries based on average GDP in the data
gdp_emissions_df %>%
  drop_na(gdp) %>%
  group_by(country)%>%
  summarise('Avg_GDP' = mean(gdp)) %>%
  arrange(desc(Avg_GDP))

top10 <- c("China", "United States", "Russia", "Japan", "India",
           "Germany", "United Kingdom", "Brazil", "France", "Italy")

# Defining countries that are not in the average GDP top 10
gdp_emissions_df2 <- gdp_emissions_df %>%
  filter(year >= 1997 & year < 2023) %>%
  mutate(group = ifelse(country %in% top10,
                        country,
                        "Rest of World"))

# Calculating gdp and ghg indexes for top 10 average GDP countries
top10_data <- gdp_emissions_df2 %>%
  filter(group != "Rest of World") %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(gdp_index = (gdp / first(na.omit(gdp))) * 100,
         ghg_index = (total_ghg / first(na.omit(total_ghg))) * 100) %>%
  ungroup()

# Calculating gdp and ghg indexes for Rest of the World
rest_world <- gdp_emissions_df2 %>%
  filter(group == "Rest of World") %>%
  group_by(year) %>%
  summarise(
    gdp = sum(gdp, na.rm = TRUE),
    total_ghg = sum(total_ghg, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(year) %>%
  mutate(gdp_index = (gdp / first(gdp)) * 100,
         ghg_index = (total_ghg / first(total_ghg)) * 100,
         country = "Rest of World")

# Calculating gdp and ghg indexes for World
world <- filt_df %>%
  select(country, year, iso_code, gdp, total_ghg) %>%
  filter(year >= 1997, year < 2023) %>%
  group_by(year) %>%
  summarise(
    gdp = sum(gdp, na.rm = TRUE),
    total_ghg = sum(total_ghg, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(year) %>%
  mutate(gdp_index = gdp / first(gdp) * 100,
         ghg_index = total_ghg / first(total_ghg) * 100,
         country = "World")
  
# Final dataset with top 10, Rest of the World and World
final_df <- bind_rows(
  top10_data %>% select(country, year, gdp_index, ghg_index),
  rest_world %>% select(country, year, gdp_index, ghg_index),
  world %>% select(country, year, gdp_index, ghg_index))

# Plotting
final_df <- final_df %>%
  mutate(country = factor(country,levels = c("Brazil","China","France",
                                             "Germany","India", "Italy","Japan",
                                             "Russia","United Kingdom",
                                             "United States","Rest of World",
                                             "World")))

labels_df <- final_df %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  mutate(gdp_label = paste0("GDP ",
                            ifelse(gdp_index - 100 >= 0, "+", ""),
                            round(gdp_index - 100),
                            "%"),
         ghg_label = paste0("GHG ",
                            ifelse(ghg_index - 100 >= 0, "+", ""),
                            round(ghg_index - 100),
                            "%")) %>%
  ungroup()


p2<- ggplot(data = final_df)+
  geom_line(aes(x = year, y = gdp_index, color = "GDP"), 
            linewidth = 0.8)+
  geom_line(aes(x = year, y = ghg_index, color = "GHG"), 
            linewidth = 0.5)+
  facet_wrap(~country)+
  scale_color_manual(
    values = c(GDP = "#2C7FB8",
               GHG = "#D95F02" ))+
  labs(title=expression("Indexed GDP and Greenhouse Gas Emissions (1997-2022)"),
       subtitle = expression(atop("Top 10 average GDP countries, Rest of World and World",
                                  "Base year = 1997 - GDP: 2011 international-$ - GHG: CO"[2] * "e (100-year)")),
       x="Year",
       y="Index",
       caption = expression("Source: Our World in Data (CO"[2] * " dataset)"))+
  geom_text(data = labels_df,aes(year, gdp_index,
                                 label = gdp_label,
                                 color = "GDP"),
            hjust = -0.1,
            fontface = "bold",
            size = 2.8,
            show.legend = FALSE)+
  geom_text(data = labels_df,
            aes(year, ghg_index,
                label = ghg_label,
                color = "GHG"),
            hjust = -0.1,
            fontface = "bold",
            size = 2.8,
            show.legend = FALSE)+
  expand_limits(x = 2025)+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.25)))+
  theme_bw()+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size=12),
        axis.text = element_text(size=10),
        axis.title = element_text(size=13),
        plot.caption = element_text(size=10),
        plot.caption.position = "plot",
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 35, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        panel.grid.major = element_line(linewidth = .2),
        panel.grid.minor = element_line(linewidth = .2))

p2

#ggsave('Images/countries_GDP_ghg_index.pdf',
#       plot = p2,
#       width = 13,
#       height = 8,
#       dpi = 300,
#       bg = "white")


# 5) THIRD VISUALIZATION: BUBBLE CHART CO2-GDP PER CAPITA 
co2_gdp_per_capita <- filt_df %>%
  select(country, year, iso_code, population, gdp, 
         co2_including_luc_per_capita, continent) %>%
  filter(!country == "World") %>%
  filter(population >= 5000000) %>%
  filter(year==2022) %>%
  mutate(gdp_per_capita = gdp/population)

pop_breaks <- c(5e6,25e6,100e6,500e6,1e9)

p3 <- co2_gdp_per_capita %>%
  ggplot(aes(gdp_per_capita, co2_including_luc_per_capita,
             size = population,
             fill = continent))+
  geom_point(alpha=.7,
             shape=21,
             stroke=.4,
             color="black",
             aes(fill=continent))+
  scale_x_log10(labels = scales::label_dollar())+
  scale_size_continuous(range = c(2, 11),
                        breaks = rev(pop_breaks),
                        name = "Population",
                        labels = function(x) paste0(round(x / 1e6, 0), "M"))+
  scale_y_continuous(labels = scales::label_number(suffix = " t"))+
  scale_fill_manual(values = c(
    "Africa" = "#000000", 
    "Americas" = "#DF0024", 
    "Asia" = "#F4C300", 
    "Europe" = "#009F3D",
    "Oceania" = "#0085C7"),
    name="Continent")+
  labs(title=expression("Per Capita GDP and CO"[2]*" Emissions (2022)"),
       subtitle = "Dimension based on Population* and Color on Continent",
       x="GDP per capita (international-$ in 2011 prices, log-scale)",
       y=expression("CO"[2]*" per capita"),
       caption=paste("Source: Our World in Data (CO2 dataset)",
                     "* Only countries with a population of at least 5 million inhabitants were included.",
                     sep = "\n"))+
  geom_text_repel(data = subset(co2_gdp_per_capita,
                                country %in% c("United States","China",
                                               "India","Germany",
                                               "Brazil","Russia")),
                  aes(label = iso_code),
                  size = 3,
                  color="black")+
  theme_minimal()+
  theme(plot.title = element_text(size=20, face="bold"),
        plot.subtitle = element_text(size=12),
        axis.text = element_text(size=10),
        axis.title = element_text(size=13),
        plot.caption = element_text(size=10),
        plot.caption.position = "plot",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.position = "right",
        panel.grid.major = element_line(linewidth=.2),
        panel.grid.minor = element_line(linewidth=.2))+
  guides(fill = guide_legend(override.aes = list(size = 4)))

p3

#ggsave('Images/GDP_co2_per_capita.pdf',
#       plot = p3,
#       width = 13,
#       height = 8,
#       dpi = 300,
#       bg = "white")


# 6) FOURTH VISUALIZATION: DUMBBELL CHART ENERGY AND GHG PER CAPITA 
install.packages("remotes")
remotes::install_github("hrbrmstr/ggalt")
library(ggalt)
library(gridExtra)
library(grid)

df_plot4 <- df %>%
  filter(grepl("income countries", country)) %>%
  filter(year == 1997 | year == 2022) %>% 
  select(country, year, energy_per_capita, co2_including_luc_per_capita, 
         methane_per_capita, nitrous_oxide_per_capita) %>%
  mutate(energy_per_capita = round(energy_per_capita, 2),
         co2_including_luc_per_capita = round(co2_including_luc_per_capita,2),
         methane_per_capita = round(methane_per_capita, 2),
         nitrous_oxide_per_capita = round(nitrous_oxide_per_capita, 2))

# Wide format data
wide_data <- pivot_wider(df_plot4, 
                         names_from = year, 
                         values_from = c(energy_per_capita, co2_including_luc_per_capita, 
                                         methane_per_capita, nitrous_oxide_per_capita),
                         names_glue = "{.value}_{year}") %>%
  mutate(country = factor(country,levels = rev(c("High-income countries",
                                                 "Upper-middle-income countries",
                                                 "Lower-middle-income countries",
                                                 "Low-income countries"))))


# Plotting
# Plotting function
create_dumbbell <- function(data, variable, x_label, show_y = TRUE) {
  ggplot(data, aes(y = country)) +
    geom_dumbbell(aes(x = .data[[paste0(variable, "_1997")]],
                      xend = .data[[paste0(variable, "_2022")]]),
                  colour = "grey75",
                  colour_x = "#e41a1c",
                  colour_xend = "#377eb8",
                  size = 1.5,
                  size_x = 4.5,
                  size_xend = 4.5) +
    labs(x = x_label,
         y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      axis.title.x = element_text(size = 15),
      axis.text.x = element_text(size = 12),
      panel.border = element_rect(colour="grey85", fill=NA),
      plot.margin = margin(t = 12,r = 18,b = 12,l = 18),
      # check axis Y
      axis.text.y = if(show_y) {
        element_text(size = 13, face = "bold")
      } else {
        element_blank()
      },
      axis.ticks.y = if(show_y) {
        element_line()
      } else {
        element_blank()
      })
}

# Variables list
variables <- list(
  energy = list(var = "energy_per_capita",
                label = "Energy per Capita [kWh/person]"),
  co2 = list(var = "co2_including_luc_per_capita",
             label = expression("CO"[2]*" per Capita [t/person]")),
  methane = list(var = "methane_per_capita",
                 label = "Methane per Capita"),
  nitrous = list(var = "nitrous_oxide_per_capita",
                 label = "Nitrous Oxide per Capita"))

# Generating the four charts
plots <- lapply(names(variables), function(x) {
  create_dumbbell(
    data = wide_data,
    variable = variables[[x]]$var,
    x_label = variables[[x]]$label,
    show_y = x %in% c("energy", "methane"))
})

names(plots) <- names(variables)

# Function to get the legend from a plot
pick_legend <- function(dumbbell) {
  step1 <- ggplotGrob(dumbbell)
  legends <- check_grob <- step1$grobs[[which(sapply(step1$grobs, 
                                                     function(x) x$name) == "guide-box")]]
  return(legends)
}

# Chart only for the legend + Selecting legend
legend_plot <- ggplot(data.frame(year = c("1997","2022"),
                                 x = c(1,2),
                                 y = c(1,1)),
                      aes(x=x,y=y,color=year))+
  geom_point(size=5)+
  scale_color_manual(
    values=c("1997"="#e41a1c",
             "2022"="#377eb8"),
    name=NULL)+
  theme_void()+
  theme(legend.position="top",
        legend.text=element_text(size=14))

general_legend <- pick_legend(legend_plot)

# Title, Subtitle, and Notes as separate objects
titles <- textGrob(
  "Changes in Per-Capita Energy Use and Greenhouse Gas Emissions by Income Group (1997-2022)",
  x = 0,
  hjust = 0,
  gp = gpar(fontsize = 22, fontface = "bold"))

subtitles <- textGrob(
  "Source: Our World in Data (CO2 dataset)",
  x = 0,
  hjust = 0,
  gp = gpar(fontsize = 14, 
            fontface = "plain", 
            col = "gray40"))

notes <- textGrob(
  "Notes:\n- CO2 values include land-use change emissions.\n- Methane and nitrous oxide are expressed in tonnes of CO2-equivalent per capita.", 
  x = 0, 
  hjust = 0,
  gp = gpar(fontsize = 10, fontface = "italic", col = "gray50"))

# Title and subtitle as unique block
title_subtitle_block <- arrangeGrob(titles, subtitles, 
                                    ncol = 1,
                                    heights = c(0.7, 0.3))

# Unique window plot + Saving
p4 <- grid.arrange(
  title_subtitle_block,
  general_legend,
  arrangeGrob(
    plots$energy,
    plots$co2,
    plots$methane,
    plots$nitrous,
    ncol = 2),
  notes,
  heights = c(.09,.06,.73,.12))

#ggsave('Images/Dumbbell_Energy_GHG.pdf',
#       plot = p4,
#       width = 15,
#       height = 12,
#       dpi = 300,
#       bg = "white")
