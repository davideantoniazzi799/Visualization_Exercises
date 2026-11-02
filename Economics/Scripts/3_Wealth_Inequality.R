# Libraries ----
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(scales)
library(ggalt)
library(gridExtra)

# File path definition ----
top5_share_file_path <- "Data/Wealth_Inequality/Top5_share.xlsx"
top10_share_file_path <- "Data/Wealth_Inequality/Top10_share.xlsx"
bottom50_share_file_path <- "Data/Wealth_Inequality/Bottom50_share.xlsx"

# Data import ----
#1) Custom function for automated ECB data import
import_ecb_share_data <- function(file_path, value_col_name) {
  #1.1) Country names: importing row 7 only (countries from column C onwards)
  row_countries <- read_excel(file_path, 
                              skip = 6, 
                              n_max = 1, 
                              col_names = FALSE)
  #Extracting country names from column C
  country_names <- as.character(row_countries[1, 3:ncol(row_countries)])
  #1.2) Importing dataset
  ecb_data <- read_excel(file_path, skip = 14)
  #1.3) Assigning country names to the right column
  colnames(ecb_data) <- c("DATE", "TIME_PERIOD", country_names)
  #1.4) Long format dataset transformation
  df_long <- ecb_data %>% 
    rename(Date = DATE, Quarter = TIME_PERIOD) %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_longer(cols = -c(Date, Quarter),
                 names_to = "Geographical_Area",
                 values_to = value_col_name)
  
  return(df_long)
}

#2)Data import execution
#---Net Wealth of Households Top 5 percent share---
top5_share_df <- import_ecb_share_data(top5_share_file_path, 
                                       "Net_Wealth_Top_5_Percent_Share")
glimpse(top5_share_df)

#---Net Wealth of Households Top 10 percent share---
top10_share_df <- import_ecb_share_data(top10_share_file_path, 
                                        "Net_Wealth_Top_10_Percent_Share")
glimpse(top10_share_df)

#---Net Wealth of Households Bottom 50 percent share---
bottom50_share_df <- import_ecb_share_data(bottom50_share_file_path, 
                                           "Net_Wealth_Bottom_50_Percent_Share")
glimpse(bottom50_share_df)

# Custom function to filter and delete NAs in the datasets ----
filter_dataset <- function(dataset) {
  
  df_filter <- dataset %>%
    #Filtering datasets to keep 2015Q4 and 2025Q4 only for each country
    filter(Quarter == "2015Q4" | Quarter == "2025Q4") %>%
    #Deleting every NA
    drop_na()
  
  return(df_filter)
}
#---Net Wealth of Households Top 5 percent share filter---
top5_share_df_filter <- filter_dataset(top5_share_df)
View(top5_share_df_filter)
#---Net Wealth of Households Top 10 percent share filter---
top10_share_df_filter <- filter_dataset(top10_share_df)
View(top10_share_df_filter)
#---Net Wealth of Households Bottom 50 percent share filter---
bottom50_share_df_filter <- filter_dataset(bottom50_share_df)
View(bottom50_share_df_filter)

# Custom function to delete countries with only one observation ----
delete_unique_countries <- function(dataset) {
  df_filter <- dataset %>%
    group_by(Geographical_Area) %>%
    filter(n()>1) %>%
    ungroup()
  
  return(df_filter)
}
#---Net Wealth of Households Top 5 percent share filter---
top5_share_df_filter <- delete_unique_countries(top5_share_df_filter)
#Countries deleted:
# Croatia (HR)     
# Latvia (LV)      
# Lithuania (LT)   
# Netherlands (NL) 
#---Net Wealth of Households Top 10 percent share filter---
top10_share_df_filter <- delete_unique_countries(top10_share_df_filter)
#Countries deleted:
# Croatia (HR)     
# Latvia (LV)      
# Lithuania (LT)   
# Netherlands (NL) 
#---Net Wealth of Households Bottom 50 percent share filter---
bottom50_share_df_filter <- delete_unique_countries(bottom50_share_df_filter)
#Countries deleted:
# Croatia (HR)     
# Latvia (LV)      
# Lithuania (LT)

# Wide format data custom function for the dumbbell charts ----
from_long_to_wide <- function(long_dataset, value_col_name) {
  wide_dataset <- long_dataset %>%
    filter(Date %in% as.Date(c("2015-12-31", "2025-12-31"))) %>%
    select(-Quarter) %>%
    pivot_wider(names_from = "Date",
                values_from = value_col_name,
                names_prefix = value_col_name)
}

#top5_share_df_filter wide
top5_share_df_filter_wide <- from_long_to_wide(top5_share_df_filter,
                                               "Net_Wealth_Top_5_Percent_Share")
#top10_share_df_filter wide
top10_share_df_filter_wide <- from_long_to_wide(top10_share_df_filter,
                                                "Net_Wealth_Top_10_Percent_Share")
#bottom50_share_df_filter wide
bottom50_share_df_filter_wide <- from_long_to_wide(bottom50_share_df_filter,
                                                   "Net_Wealth_Bottom_50_Percent_Share")

# Making the dumbbell chart of Net Wealth of Households Top 5 percent share ----
top5_dumbbell <- top5_share_df_filter_wide %>%
  mutate(Geographical_Area = reorder(Geographical_Area, `Net_Wealth_Top_5_Percent_Share2015-12-31`)) %>%
  ggplot(aes(y = Geographical_Area)) +
  geom_dumbbell(aes(x = `Net_Wealth_Top_5_Percent_Share2015-12-31`,
                    xend = `Net_Wealth_Top_5_Percent_Share2025-12-31`),
                colour = "#D1CFC7",
                colour_x = "#1F4E5B",
                colour_xend = "#C47D2B",
                size = 1,
                size_x = 4.5,
                size_xend = 4.5) +
  #Invisible points used only to create the legend
  geom_point(aes(x = `Net_Wealth_Top_5_Percent_Share2015-12-31`,
                 colour = "4th Quarter 2015"),
             size = 4.5) +
  geom_point(aes(x = `Net_Wealth_Top_5_Percent_Share2025-12-31`,
                 colour = "4th Quarter 2025"),
             size = 4.5) +
  scale_colour_manual(name = NULL, values = c("4th Quarter 2015" = "#1F4E5B",
                                              "4th Quarter 2025" = "#C47D2B")) +
  labs(title = "Net Wealth Share of the Top 5% Richest Households",
       subtitle = "Change in the share of net wealth owned by the top 5% richest households in Eurozone countries (2015Q4 vs 2025Q4)",
       x = "Net Wealth Share (%)",
       y = NULL,
       caption = paste(
         "Source: European Central Bank (ECB), Series: DWA.Q.I9.S14._Z._Z.NWA.T5.PT.S.N\n",
         "Note: Croatia, Latvia, Lithuania, and the Netherlands are excluded due to missing values.\n",
         "Author: Davide Antoniazzi")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#FFFDF6", 
                                   colour = NA),
    panel.background = element_rect(fill = "#FFFDF6", 
                                    colour = NA),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11,
                               face = "bold",
                               colour = "#111111"),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#E6E4DC", 
                                       linewidth = 0.6),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 11,
                                face = "bold",
                                colour = "#0F2537",
                                margin = margin(t = 12)),
    axis.text.x = element_text(size = 10,
                               colour = "#555555"),
    axis.text.y = element_text(size = 11,
                               colour = "#111111",
                               face = "plain"),
    plot.title = element_text(size=16, 
                              face="bold",
                              colour = "#0F2537",
                              margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11,
                                 colour = "#4A4A4A",
                                 margin = margin(b = 15)),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8.5,
                                colour = "#666666",
                                hjust = 0,
                                lineheight = 1.2))

top5_dumbbell

ggsave('Images/4_Top5_NetWealth_dumbbell_EuroArea.pdf',
       plot = top5_dumbbell,
       width = 13,
       height = 8,
       dpi = 300,
       bg = "white")

# Making the dumbbell chart of Net Wealth of Households Top 10 percent share ----
top10_dumbbell <- top10_share_df_filter_wide %>%
  mutate(Geographical_Area = reorder(Geographical_Area, `Net_Wealth_Top_10_Percent_Share2015-12-31`)) %>%
  ggplot(aes(y = Geographical_Area)) +
  geom_dumbbell(aes(x = `Net_Wealth_Top_10_Percent_Share2015-12-31`,
                    xend = `Net_Wealth_Top_10_Percent_Share2025-12-31`),
                colour = "#D1CFC7",
                colour_x = "#1F4E5B",
                colour_xend = "#C47D2B",
                size = 1,
                size_x = 4.5,
                size_xend = 4.5) +
  #Invisible points used only to create the legend
  geom_point(aes(x = `Net_Wealth_Top_10_Percent_Share2015-12-31`,
                 colour = "4th Quarter 2015"),
             size = 4.5) +
  geom_point(aes(x = `Net_Wealth_Top_10_Percent_Share2025-12-31`,
                 colour = "4th Quarter 2025"),
             size = 4.5) +
  scale_colour_manual(name = NULL, values = c("4th Quarter 2015" = "#1F4E5B",
                                              "4th Quarter 2025" = "#C47D2B")) +
  labs(title = "Net Wealth Share of the Top 10% Richest Households",
       subtitle = "Change in the share of net wealth owned by the top 10% richest households in Eurozone countries (2015Q4 vs 2025Q4)",
       x = "Net Wealth Share (%)",
       y = NULL,
       caption = paste(
         "Source: European Central Bank (ECB), Series: DWA.Q.I9.S14._Z._Z.NWA.T10.PT.S.N\n",
         "Note: Croatia, Latvia, Lithuania, and the Netherlands are excluded due to missing values.\n",
         "Author: Davide Antoniazzi")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#FFFDF6", 
                                   colour = NA),
    panel.background = element_rect(fill = "#FFFDF6", 
                                    colour = NA),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11,
                               face = "bold",
                               colour = "#111111"),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#E6E4DC", 
                                      linewidth = 0.6),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 11,
                                face = "bold",
                                colour = "#0F2537",
                                margin = margin(t = 12)),
    axis.text.x = element_text(size = 10,
                               colour = "#555555"),
    axis.text.y = element_text(size = 11,
                               colour = "#111111",
                               face = "plain"),
    plot.title = element_text(size=16, 
                              face="bold",
                              colour = "#0F2537",
                              margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11,
                                 colour = "#4A4A4A",
                                 margin = margin(b = 15)),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8.5,
                                colour = "#666666",
                                hjust = 0,
                                lineheight = 1.2))

top10_dumbbell

ggsave('Images/5_Top10_NetWealth_dumbbell_EuroArea.pdf',
       plot = top10_dumbbell,
       width = 13,
       height = 8,
       dpi = 300,
       bg = "white")

# Making the dumbbell chart of Net Wealth of Households Bottom 50 percent share ----
bottom50_dumbbell <- bottom50_share_df_filter_wide %>%
  mutate(Geographical_Area = if_else(
    Geographical_Area == "Euro area 21 (fixed composition) as of 1 January 2026 (I10)",
    "Euro Area (EA21)", Geographical_Area)) %>%
  mutate(Geographical_Area = reorder(Geographical_Area, `Net_Wealth_Bottom_50_Percent_Share2015-12-31`)) %>%
  ggplot(aes(y = Geographical_Area)) +
  geom_dumbbell(aes(x = `Net_Wealth_Bottom_50_Percent_Share2015-12-31`,
                    xend = `Net_Wealth_Bottom_50_Percent_Share2025-12-31`),
                colour = "#D1CFC7",
                colour_x = "#1F4E5B",
                colour_xend = "#C47D2B",
                size = 1,
                size_x = 4.5,
                size_xend = 4.5) +
  #Invisible points used only to create the legend
  geom_point(aes(x = `Net_Wealth_Bottom_50_Percent_Share2015-12-31`,
                 colour = "4th Quarter 2015"),
             size = 4.5) +
  geom_point(aes(x = `Net_Wealth_Bottom_50_Percent_Share2025-12-31`,
                 colour = "4th Quarter 2025"),
             size = 4.5) +
  scale_colour_manual(name = NULL, values = c("4th Quarter 2015" = "#1F4E5B",
                                              "4th Quarter 2025" = "#C47D2B")) +
  labs(title = "Net Wealth Share of the Bottom 50% Households",
       subtitle = "Change in the share of net wealth owned by the lower half of the population in Eurozone countries (2015Q4 vs 2025Q4)",
       x = "Net Wealth Share (%)",
       y = NULL,
       caption = paste(
         "Source: European Central Bank (ECB), Series: DWA.Q.I10.S14._Z._Z.NWA.B50.PT.S.N\n",
         "Note: Croatia, Latvia, and Lithuania are excluded due to missing values.\n",
         "Author: Davide Antoniazzi")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#FFFDF6", 
                                   colour = NA),
    panel.background = element_rect(fill = "#FFFDF6", 
                                    colour = NA),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11,
                               face = "bold",
                               colour = "#111111"),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#E6E4DC", 
                                      linewidth = 0.6),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 11,
                                face = "bold",
                                colour = "#0F2537",
                                margin = margin(t = 12)),
    axis.text.x = element_text(size = 10,
                               colour = "#555555"),
    axis.text.y = element_text(size = 11,
                               colour = "#111111",
                               face = "plain"),
    plot.title = element_text(size=16, 
                              face="bold",
                              colour = "#0F2537",
                              margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11,
                                 colour = "#4A4A4A",
                                 margin = margin(b = 15)),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8.5,
                                colour = "#666666",
                                hjust = 0,
                                lineheight = 1.2))

bottom50_dumbbell

ggsave('Images/6_Bottom50_NetWealth_dumbbell_EuroArea.pdf',
       plot = bottom50_dumbbell,
       width = 13,
       height = 8,
       dpi = 300,
       bg = "white")