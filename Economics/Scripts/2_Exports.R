# Libraries ----
library(tidyverse)
library(readr)
library(lubridate)
library(data.table)
library(plotly)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(scales) #to format number scale
library(htmlwidgets) #to export html

# File path ----
exports_file_path <- "Data/Exports/hs22_country_product_year_2.csv"
product_classification_file_path <- "Data/Exports/product_hs22.csv"

# Importing dataset ----
df_exports <- read_csv(exports_file_path,
                       skip = 0)
glimpse(df_exports)

df_product_classification <- df_exports <- read_csv(product_classification_file_path,
                                                    skip = 0)
glimpse(df_exports)

# Dataset cleaning ----
df_exports_ita_ger_2023 <- df_exports %>%
  filter(country_iso3_code == "DEU" | country_iso3_code == "ITA") %>%
  filter(year == 2023)
#There are no NAs that matter to this analysis

# Join of 'df_exports_ita_ger_2023' with 'df_product_classification' ----
df_exports_complete <- df_exports_ita_ger_2023 %>%
  left_join(df_product_classification, by = c("product_id", "product_hs22_code")) %>%
  select(country_id, country_iso3_code, product_id, product_hs22_code,
         product_name, product_name_short, year, export_value)

# Creating 'Sector_Category' column ----
df_exports_complete <- df_exports_complete %>% mutate(
  product_hs22_code = as.numeric(product_hs22_code)) %>%
  mutate(
  Sector_Category = case_when(
    product_hs22_code >= 1 & product_hs22_code <= 24 ~ "Agriculture & Foodstuffs",
    product_hs22_code >= 25 & product_hs22_code <= 40 ~ "Minerals & Chemicals",
    product_hs22_code >= 41 & product_hs22_code <= 49 ~ "Raw Materials & Paper",
    product_hs22_code >= 50 & product_hs22_code <= 67 ~ "Textiles & Footwear",
    product_hs22_code >= 68 & product_hs22_code <= 83 ~ "Stone, Glass & Metals",
    product_hs22_code >= 84 & product_hs22_code <= 85 ~ "Machinery & Electronics",
    product_hs22_code >= 86 & product_hs22_code <= 97 ~ "Transport & Equipment",
    TRUE ~ "Not Specified"))

# Making the Interactive Tree Map for German and Italian Product exports in 2023 ----
#1) SCIENTIFIC DATA PREPARATION & CUSTOM ECONOMIC PALETTE
unique_sectors <- c("Machinery & Electronics",
                    "Transport & Equipment",
                    "Minerals & Chemicals",
                    "Stone, Glass & Metals",
                    "Raw Materials & Paper",
                    "Textiles & Footwear",
                    "Agriculture & Foodstuffs",
                    "Not Specified")

#Defining the palette
palette_colors <- c("Machinery & Electronics" = "#1f77b4",
                    "Transport & Equipment"   = "#008080",
                    "Minerals & Chemicals"    = "#6a3d9a",
                    "Stone, Glass & Metals"   = "#708090",
                    "Raw Materials & Paper"   = "#b57c43",
                    "Textiles & Footwear"     = "#8da0cb",
                    "Agriculture & Foodstuffs" = "#2ca02c",
                    "Not Specified"           = "#555555")

#2) FUNCTION TO GENERATE CLEAN HIERARCHICAL RECORDS
#Data = dataframe to use as input
#Country_code = specific iso3 code of the country
#Country_name = specific name of the input country 
prepare_country_data <- function(data, country_code, country_name) {
  df_country <- data %>%
    filter(country_iso3_code == country_code) %>%
    mutate(Sector_Category = ifelse(is.na(Sector_Category) | Sector_Category == "", 
                                    "Not Specified", 
                                    Sector_Category)) %>%
    mutate(export_compressed = label_number(prefix = "$", #compressed export values
                                            accuracy = 1, 
                                            scale_cut = cut_short_scale())(export_value))
  
  df_sectors <- df_country %>%
    group_by(Sector_Category) %>%
    summarise(export_value = sum(export_value), .groups = "drop") %>%
    mutate(id = paste(country_name, 
                      Sector_Category, 
                      sep = " - "),
           label = Sector_Category,
           parent = country_name,
           color = palette_colors[Sector_Category],
           hover_text = paste("Sector:", Sector_Category, 
                              "<br>Total Value:", label_number(prefix = "$", 
                                                               accuracy = 1, 
                                                               scale_cut = cut_short_scale())(export_value)))
  
  df_products <- df_country %>%
    mutate(id = paste(country_name, 
                      Sector_Category, 
                      product_name_short, sep = " - "),
           label = paste0(product_name_short, "<br>", export_compressed),
           parent = paste(country_name, 
                          Sector_Category, 
                          sep = " - "),
           color = palette_colors[Sector_Category],
           hover_text = paste("Product:", product_name_short, 
                              "<br>Sector:", Sector_Category, 
                              "<br>Value:", export_compressed))
  
  df_root <- data.frame(id = country_name, 
                        label = country_name, 
                        parent = "", 
                        export_value = sum(df_country$export_value), 
                        color = "gray90",
                        hover_text = paste("Country:", country_name))
  
  bind_rows(df_root, df_sectors, df_products) %>%
    #Adding a column to identify the country at the row level
    mutate(country_group = country_name)
}

#3) GENERATE DATA
df_germany <- prepare_country_data(df_exports_complete, "DEU", "GERMANY")
df_italy <- prepare_country_data(df_exports_complete, "ITA", "ITALY")

#Combining the datasets into a single corrected block
df_total_treemap <- bind_rows(df_germany, df_italy)

#4) GENERATE BOTH TREEMAPS IN A SINGLE PLOTLY CALL
final_map <- plot_ly() %>%
  #First Treemap: GERMANY (Positioned in the upper domain of the grid)
  add_trace(data = filter(df_total_treemap, 
                          country_group == "GERMANY"),
            ids = ~id, 
            labels = ~label, 
            parents = ~parent, 
            values = ~export_value,
            type = "treemap", 
            branchvalues = "total", 
            textinfo = "label", 
            hoverinfo = "text", 
            hovertext = ~hover_text,
            marker = list(colors = ~color, 
                          line = list(color = "white", 
                                      width = 1.5)),
            domain = list(x = c(0, 1), 
                          y = c(0.52, 1)) #Upper row (from the middle of the screen upwards)
  ) %>%
  #Second Treemap: ITALY (Positioned in the lower section of the grid)
  add_trace(data = filter(df_total_treemap, 
                          country_group == "ITALY"),
            ids = ~id, 
            labels = ~label, 
            parents = ~parent, 
            values = ~export_value,
            type = "treemap", 
            branchvalues = "total", 
            textinfo = "label", 
            hoverinfo = "text", 
            hovertext = ~hover_text,
            marker = list(colors = ~color, 
                          line = list(color = "white", 
                                      width = 1.5)),
            domain = list(x = c(0, 1), y = c(0, 0.48)) #Lower row (from zero to the middle of the screen)
  ) %>%
  #5) LAYOUT, SCIENTIFIC TITLES & CAPTION
  layout(title = list(
    text = "<b>Comparative Analysis of Export Structures (2023): Germany vs. Italy</b><br><sup>Macroeconomic distribution of trade values by product category and sector according to the WCO Harmonized System (HS22)</sup>",
    x = 0.02, #Shifted slightly to the right of the edge
    y = 0.91, #Pushes the title block down, closer to the treemaps
    font = list(size = 22,
                color = "#111111")),
    #Calibration of general margins
    margin = list(l = 25, r = 20, 
                  t = 115, b = 45),
    #Methodological note at the bottom left
    annotations = list(list(x = 0.01,
                            y = -0.06,
                            text = "Source: Harvard Growth Lab (2026), Harvard Dataverse. Methodology: Sectoral aggregation based on the World Customs Organization (WCO) HS Nomenclature 2022 Edition. Author: Antoniazzi Davide",
                            showarrow = FALSE,
                            xref = "paper", yref = "paper",
                            xanchor = "left",
                            font = list(size = 10, 
                                        color = "gray40", 
                                        italic = TRUE))))

#View the map
final_map

#6) EXPORT AS HTML
saveWidget(final_map, 
           "Images/3_export_comparison_deu_ita.html", 
           selfcontained = TRUE)
