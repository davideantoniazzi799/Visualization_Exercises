# Libraries ----
library(tidyverse)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)
library(purrr) # To map the function on the file list
library(calendR)

# Function to import all datasets ----
clean_no2_file <- function(file_path) {
  read_delim(
    file_path,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    skip = 0 #to keep first row with station names
    ) %>%
    rename(Date = 1) %>% #first column name "Date"
    slice(-(1:3)) %>% #deleting row 2-3-4
    #From wide to long
    pivot_longer(cols = -Date,
                 names_to = "Station",
                 values_to = "NO2_Value") %>%
    mutate(NO2_Value = as.numeric(NO2_Value),
           Date = dmy(Date))
}

# File path definition ----
no2_2015_2025_path <- "Data/NO2_Berlin_2000_2025/3_ber_no2_20151231-20251231.csv"
no2_2005_2015_path <- "Data/NO2_Berlin_2000_2025/2_ber_no2_20051231-20151230.csv"
no2_2000_2005_path <- "Data/NO2_Berlin_2000_2025/1_ber_no2_20000101-20051230.csv"

# Making a list of the file paths ----
file_paths <- c(no2_2000_2005_path, no2_2005_2015_path, no2_2015_2025_path)

# Applying the function to all datasets and join them to a unique final dataset ----
no2_total_df <- map_df(file_paths, clean_no2_file)

glimpse(no2_total_df)

# Checking NA values among stations ----
no2_total_df %>%
  group_by(Station) %>%
  summarize(Number_NA = sum(is.na(NO2_Value)),
            Percent_NA = mean(is.na(NO2_Value))*100)%>%
  arrange(desc(Percent_NA)) %>% View()

# Computing daily average NO2 in Berlin (each day from 2000 to 2025) ----
no2_city_daily_avg <- no2_total_df %>%
  group_by(Date) %>%
  summarize(City_Average_NO2 = mean(NO2_Value, na.rm = TRUE),
            Active_Stations = sum(!is.na(NO2_Value)) #Counts numbers of stations that contributed on that day
            )

# Creating calendar plot of daily average NO2 in Berlin in 2025 ----
# Days sequence in 2025 (365 days)
days_2025_complete <- tibble(Date = seq(as.Date("2025-01-01"), 
                                        as.Date("2025-12-31"), 
                                        by = "day"))

# Join data with 2025 days
data_2025 <- days_2025_complete %>%
  left_join(no2_city_daily_avg, by = "Date") %>%
  arrange(Date)

annual_mean_2025_NO2 <- round(mean(data_2025$City_Average_NO2, 
                                   na.rm = TRUE), 2)

# Creating thermal calendar
Sys.setlocale("LC_TIME", "English") 

NO2_2025_plot <- calendR(year = 2025,
        special.days = data_2025$City_Average_NO2,
        gradient = TRUE, #activating heat map
        col = "white", #squares border color
        months.size = 12,
        months.col = "grey10",
        legend.pos = "right",
        title = expression("Daily mean NO"[2] ~ "concentration in Berlin (2025)"),
        subtitle = bquote("City-wide daily average concentration in Berlin (µg/m"^3*") | Annual NO"[2] ~ "mean in 2025:" ~ .(annual_mean_2025_NO2)),
        title.size = 24,
        subtitle.size = 15,
        subtitle.col = "grey30")

# Adding details
NO2_2025_plot <- NO2_2025_plot +
  scale_fill_gradient(low = "#edf8e9", 
                      high = "#006d2c", 
                      na.value = "grey90",
                      name = bquote("NO"[2] ~ "(µg/m"^3*")"),
                      breaks = c(5, 10, 20, 30, 40, 50)) +
  labs(caption = paste(
    "Source: Berlin Air Quality Monitoring Network (luftdaten.berlin.de) |",
    "Daily city-wide averages calculated from monitoring stations |",
    "Grey represents missing data",
    "\nData accessed: 22 September 2026 | Author: Davide Antoniazzi")) +
  theme(plot.title = element_text(face = "bold",
                                  colour = "black"),
        plot.subtitle = element_text(face = "plain"),
        plot.caption = element_text(size = 9,
                                    colour = "grey50",
                                    hjust = 0),
        legend.title = element_text(size = 11, 
                                    face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(0.5, "cm"))

NO2_2025_plot

# Saving PDF format
ggsave("Images/NO2_calendar_2025.pdf",
       NO2_2025_plot,
       width = 14,
       height = 10,
       units = "in",
       dpi = 300)

# Plot of evolution of daily average NO2 in Berlin from 2000 to 2005 ----
# Preparing needed temporal columns
no2_ggplot_ready <- no2_city_daily_avg %>%
  mutate(Year = factor(year(Date)),
         Week = as.numeric(format(Date, "%U")), #calculating week number (1-53)
         Day_of_week = wday(Date, week_start = 1))

no2_Berlin_avg_2000_2025 <-round(mean(no2_total_df$NO2_Value,
                                      na.rm = TRUE), 2)

# Plotting
no2_2000_2025_avg_Berlin_plot <- ggplot(no2_ggplot_ready, 
       aes(x = Week, y = Day_of_week, 
           fill = City_Average_NO2)) +
  geom_tile(color = "white", 
            size = 0.1) + #Single calendar tiles
  facet_wrap(~ Year, 
             ncol = 1, 
             strip.position = "right") + #Year below year
  scale_fill_viridis_c(name = bquote("NO"[2] ~ "(µg/m"^3*")"), 
                       option = "cividis", 
                       na.value = "gray90",
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) + 
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 53)) + 
  scale_y_continuous(breaks = 1:7,
                     labels = 1:7,
                     trans = "reverse") +
  labs(title = expression("Historical Evolution of Daily NO"[2] ~ "Concentration in Berlin (2000-2025)"),
       subtitle = bquote("City-wide calendar plot of daily NO"[2] ~ "average (µg/m"^3*") | Overall mean:" ~ .(no2_Berlin_avg_2000_2025)),
       x = "Week of the Year",
       y = "Day of week (1 = Monday, 7 = Sunday)",
       caption = paste(
         "Source: Berlin Air Quality Monitoring Network (luftdaten.berlin.de) | Grey represents missing data",
         "\nEach tile represents the city-wide daily mean NO2 concentration calculated from monitoring stations",
         "\nData accessed: 22 September 2026 | Author: Davide Antoniazzi")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0, #straight year label
                                    size = 9,
                                    face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8,
                                    face = "bold"),
        axis.title.y = element_text(size = 8,
                                    face = "bold"),
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  colour = "black",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 13,
                                     face = "plain",
                                     color = "grey30",
                                     hjust = 0.5),
        plot.caption = element_text(size = 9,
                                    colour = "grey50",
                                    hjust = 0))

no2_2000_2025_avg_Berlin_plot

# Saving in PDF format
ggsave("Images/NO2_Berlin_daily_avg_2000_2025.pdf",
       no2_2000_2025_avg_Berlin_plot,
       width = 10,
       height = 14,
       units = "in")
