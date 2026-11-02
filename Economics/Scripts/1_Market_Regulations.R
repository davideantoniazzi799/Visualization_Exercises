# Libraries ----
library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)
library(gtExtras)
library(scales)
library(rlang)

# File path definition ----
market_reg_file_path <- "Data/Market_Regulations/PMR_Econwide_Indicator_2023-24_and_2018.xlsx"

# Importing the data ----
market_reg_df <- read_excel(market_reg_file_path,
                            sheet = "PMR_Econwide_2023-24",
                            skip = 3,
                            n_max = 61) %>%
  select(1:29) %>%
  select(-c(3:5)) %>% #deleting columns
  slice(-(1:2)) %>%
  slice(-c(39, 58)) #deleting rows

# Data cleaning ----
#Changing columns name
new_columns <- c(Code = "ISO Alpha-3 code",
                 Country = "Economies",
                 PMR_2023 = "PMR\r\n(2023 methodology)",
                 DISI = "Distortions Induced by State Involvement",
                 BDFE = "Barriers to Domestic and Foreign Entry",
                 DIPO = "Distortions Induced by Public Ownership...9",
                 IBO = "Involvement in Business Operations...10",
                 RIE = "Regulations Impact Evaluation",
                 ARB = "Administrative and Regulatory Burden",
                 BSNS = "Barriers in Service & Network sectors...13",
                 BTT = "Barriers to Trade and Investment...14",
                 QSPO = "Distortions Induced by Public Ownership...15",
                 GOV_SOEs = "...16",
                 RPCR = "Involvement in Business Operations...17",
                 IBONS = "...18",
                 IBOSS = "...19",
                 PP = "...20",
                 AIC = "Regulations\r\nImpact\r\nEvaluation",
                 Int_Stakeholder = "...22",
                 ARLLCPE = "Administrative And Regulatory Burden",
                 CSARB = "...24",
                 BESS = "Barriers in Service & Network sectors...25",
                 BENS = "...26",
                 Barr_FDI = "Barriers to Trade and Investment...27",
                 Barr_TF = "...28",
                 Tarif_Barr = "...29")

market_reg_df <- rename(market_reg_df, all_of(new_columns))

#Mutating data type
market_reg_df <- market_reg_df %>%
  mutate(across(c(PMR_2023, QSPO, GOV_SOEs, RPCR,
                  IBONS, IBOSS, PP, AIC, Int_Stakeholder, ARLLCPE,
                  CSARB, BESS, BENS, Barr_FDI, 
                  Barr_TF, Tarif_Barr), as.numeric))

#Correction of country names
market_reg_df <- market_reg_df %>%
  mutate(Country = case_when(
    Country == "Israel1" ~ "Israel",
    Country == "Mexico2" ~ "Mexico",
    Country == "Cyprus3,4" ~ "Cyprus",
    Country == "Chinese Taipei12" ~ "Chinese Taipei",
    TRUE ~ Country)) %>%
  mutate(Code = case_when(
    Code == "OECD" ~ "OECD Average",
    TRUE ~ Code))

glimpse(market_reg_df)
#There is one NA: Chinese Taipei Barr_FDI

# Dividing the dataset in High_Medium and Low Level Indicators datasets ----
high_medium_df <- market_reg_df %>%
  select(1:11)
glimpse(high_medium_df)

low_df <- market_reg_df %>%
  select(c(1:3, 12:26))

# Making the table for High and Medium Level Indicators ----
#1)CREATING BASE MAP WITHOUT COLORED CELLS
tab_high_med_indicators <- high_medium_df %>%
  gt() %>%
  cols_hide(columns = Country) %>%
  tab_header(title = "2023 OECD Product Market Regulation Performance",
             subtitle = md("Country-level Analysis of Product Market Regulation (PMR) and its High and Medium Level Indicators by OECD<sup>*</sup>")) %>%
  fmt_number(columns = where(is.numeric),
             decimals = 2) %>%
  cols_label(Code="Country Code",
             PMR_2023="PMR",
             DISI="DISI",
             BDFE = "BDFE",
             DIPO = "DIPO",
             IBO = "IBO",
             RIE = "RIE",
             ARB = "ARB",
             BSNS = "BSNS",
             BTT = "BTT") %>%
  cols_align(align = "center",
             columns = everything()) %>%
  #High and Medium Level Indicators line
  tab_spanner(label = "High Level Indicators",
              columns = c(DISI, BDFE)) %>%
  tab_spanner(label = "Medium Level Indicators",
              columns = c(DIPO, IBO, RIE, ARB, BSNS, BTT)) %>%
  tab_style(style = cell_text(weight = "bold", 
                              transform = "uppercase", 
                              size = pct(85)),
            locations = cells_column_spanners()) %>%
  tab_footnote(footnote = "Color intensity represents relative performance within each column for the five best (green) and worst (red) countries | Consult 'Data_Source.txt' for indicators' construction",
               locations = cells_column_labels(columns = c(everything(), -Code))) %>%
  tab_footnote(footnote = "Average of DISI and BDFE",
               location = cells_column_labels(columns = PMR_2023)) %>%
  tab_footnote(footnote = "DISI = Distortions Induced by State Involvement | Average of DIPO, IBO, and RIE",
               location = cells_column_labels(columns = DISI)) %>%
  tab_footnote(footnote = "BDFE = Barriers to Domestic and Foreign Entry | Average of ARB, BSNS, and BTT",
               location = cells_column_labels(columns = BDFE)) %>%
  tab_footnote(footnote = "DIPO = Distortions Induced by Public Ownership",
               location = cells_column_labels(columns = DIPO)) %>%
  tab_footnote(footnote = "IBO = Involvement in Business Operations",
               location = cells_column_labels(columns = IBO)) %>%
  tab_footnote(footnote = "RIE = Regulations Impact Evaluation",
               location = cells_column_labels(columns = RIE)) %>%
  tab_footnote(footnote = "ARB = Administrative and Regulatory Burden",
               location = cells_column_labels(columns = ARB)) %>%
  tab_footnote(footnote = "BSNS = Barriers in Service & Network Sectors",
               location = cells_column_labels(columns = BSNS)) %>%
  tab_footnote(footnote = "BTT = Barriers to Trade and Investment",
               location = cells_column_labels(columns = BTT)) %>%
  gt_theme_espn()

#2)COLORING CELLS THROUGH FOR CYCLE
#List of columns to color
columns_to_color <- c("PMR_2023", "DISI", "BDFE", "DIPO", 
                      "IBO", "RIE", "ARB", "BSNS", "BTT")

for (col in columns_to_color) {
  col_sym <- rlang::sym(col) #Turns the string into a readable symbol for gt()
  tab_high_med_indicators <- tab_high_med_indicators %>%
    #Red scale color
    data_color(columns = !!col_sym, #Interprets the column name as a variable
               rows = rank(-!!col_sym, ties.method = "first") <= 5,
               fn = scales::col_numeric(palette = "Reds", domain = NULL)) %>%
    #Inverted Green scale color
    data_color(columns = !!col_sym,
               rows = rank(!!col_sym, ties.method = "first") <= 5,
               fn = scales::col_numeric(palette = rev(scales::brewer_pal(palette = "Greens")(9)), domain = NULL))
}

#3)COMPLETING THE TABLE WITH LAST DETAILS
tab_high_med_indicators <- tab_high_med_indicators %>%
  #Separating lines
  tab_style(style = cell_borders(sides = "right", 
                                 color = "#94a3b8", 
                                 weight = px(2)), 
            locations = list(cells_body(columns = PMR_2023), 
                             cells_column_labels(columns = PMR_2023))) %>%
  tab_style(style = cell_borders(sides = "right", 
                                 color = "#94a3b8", 
                                 weight = px(2)), 
            locations = list(cells_body(columns = BDFE), 
                             cells_column_labels(columns = BDFE))) %>%
  #Line that separates OECD and non-OECD countries
  tab_style(style = cell_borders(sides = "bottom", 
                                 color = "#1e293b", 
                                 weight = px(2.5), 
                                 style = "solid"),
            locations = cells_body(columns = everything(),
                                   rows = 38 #After 38th row (USA)
            )) %>%
  #Customizing 'OECD Average' line
  tab_style(style = list(cell_fill(color = "#e2e8f0"), 
                         cell_text(weight = "bold", 
                                   color = "#1e293b")), 
            locations = cells_body(columns = everything(), 
                                   rows = Code == "OECD Average")) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom"), 
                                 color = "#94a3b8", 
                                 weight = px(1.5)), 
            locations = cells_body(columns = everything(), 
                                   rows = Code == "OECD Average")) %>%
  tab_footnote(footnote = "The OECD Average is calculated exclusively using data from the first 38 OECD member countries listed above (until USA).",
               locations = cells_body(columns = Code, 
                                      rows = Code == "OECD Average")) %>%
  #Data Source and Author footnote
  tab_source_note(source_note = md("**[*]Data Source**: OECD Product Market Regulation (PMR) Database (2023-2024 assessment) | Author: Davide Antoniazzi")) %>%
  #To keep colors when exporting
  opt_css(css = "@media print { * { -webkit-print-color-adjust: exact !important; print-color-adjust: exact !important; } }")

tab_high_med_indicators

#Exporting the table in 'Images' folder
gtsave(data = tab_high_med_indicators, 
       filename = "Images/1_PMR_Performance_2023_High_Medium_Indicators.pdf")

# Making the table for Low Level Indicators ----
#1)CREATING BASE MAP WITHOUT COLORED CELLS
tab_low_indicators <- low_df %>%
  gt() %>%
  cols_hide(columns = Country) %>%
  tab_header(title = "2023 OECD Product Market Regulation Low Level Indicators Performance",
             subtitle = md("Country-level Analysis of Product Market Regulation (PMR) and its Low Level Indicators by OECD<sup>*</sup>")) %>%
  fmt_number(columns = where(is.numeric),
             decimals = 2) %>%
  cols_label(Code="Country Code",
             PMR_2023="PMR",
             QSPO="QSPO",
             GOV_SOEs = "Gov. SOEs",
             RPCR = "RPCR",
             IBONS = "IBONS",
             IBOSS = "IBOSS",
             PP = "PP",
             AIC = "AIC",
             Int_Stakeholder = "Int. Stakeholders",
             ARLLCPE = "ARLLCPE",
             CSARB = "CSARB",
             BESS = "BESS",
             BENS = "BENS",
             Barr_FDI = "Barr. FDI",
             Barr_TF = "Barr. TF",
             Tarif_Barr = "Tariff Barr.") %>%
  cols_align(align = "center",
             columns = everything()) %>%
  #Low Indicators Line
  tab_spanner(label = "Low Level Indicators",
              columns = c(QSPO, GOV_SOEs, RPCR, IBONS, IBOSS, PP, AIC, 
                          Int_Stakeholder, ARLLCPE, CSARB, BESS, BENS, 
                          Barr_FDI, Barr_TF, Tarif_Barr)) %>%
  tab_style(style = cell_text(weight = "bold", 
                              transform = "uppercase", 
                              size = pct(85)),
            locations = cells_column_spanners()) %>%
  tab_footnote(footnote = "Color intensity represents relative performance within each column for the five best (green) and worst (red) countries | Consult 'Data_Source.txt' for indicators' construction",
               locations = cells_column_labels(c(everything(), -Code))) %>%
  tab_footnote(footnote = "QSPO = Quality and Scope of Public Ownership",
               location = cells_column_labels(columns = QSPO)) %>%
  tab_footnote(footnote = "Gov. SOEs = Governance of SOEs",
               location = cells_column_labels(columns = GOV_SOEs)) %>%
  tab_footnote(footnote = "RPCR = Retail Price Controls and Regulation",
               location = cells_column_labels(columns = RPCR)) %>%
  tab_footnote(footnote = "IBONS = Involvement in Business Operations in Network Sectors",
               location = cells_column_labels(columns = IBONS)) %>%
  tab_footnote(footnote = "IBOSS = Involvement in Business Operations in Service Sectors",
               location = cells_column_labels(columns = IBOSS)) %>%
  tab_footnote(footnote = "PP = Public Procurement",
               location = cells_column_labels(columns = PP)) %>%
  tab_footnote(footnote = "AIC = Assessment of Impact on Competition",
               location = cells_column_labels(columns = AIC)) %>%
  tab_footnote(footnote = "Int. Stakeholder = Interaction with Stakeholders",
               location = cells_column_labels(columns = Int_Stakeholder)) %>%
  tab_footnote(footnote = "ARLLCPE = Administrative Requirements for Limited Liability Companies and Personally-owned Enterprises",
               location = cells_column_labels(columns = ARLLCPE)) %>%
  tab_footnote(footnote = "CSARB = Communication and Simplification of  Administrative and Regulatory Burden",
               location = cells_column_labels(columns = CSARB)) %>%
  tab_footnote(footnote = "BESS = Barriers to Entry in Service Sectors",
               location = cells_column_labels(columns = BESS)) %>%
  tab_footnote(footnote = "BENS = Barriers to Entry in Network Sectors",
               location = cells_column_labels(columns = BENS)) %>%
  tab_footnote(footnote = "Barr. FDI = Barriers to FDI",
               location = cells_column_labels(columns = Barr_FDI)) %>%
  tab_footnote(footnote = "Barr. TF = Barriers to Trade Facilitation",
               location = cells_column_labels(columns = Barr_TF)) %>%
  tab_footnote(footnote = "Tarif. Barr. = Tariff Barriers",
               location = cells_column_labels(columns = Tarif_Barr)) %>%
  gt_theme_espn()

#2)COLORING CELLS THROUGH FOR CYCLE
#List of columns to color
columns_to_color <- c("PMR_2023", "QSPO", "GOV_SOEs", "RPCR", 
                      "IBONS", "IBOSS", "PP", "AIC", "Int_Stakeholder",
                      "ARLLCPE", "CSARB", "BESS", "BENS", "Barr_FDI",
                      "Barr_TF", "Tarif_Barr")

for (col in columns_to_color) {
  col_sym <- rlang::sym(col) #Turns the string into a readable symbol for gt()
  tab_low_indicators <- tab_low_indicators %>%
    #Red scale color
    data_color(columns = !!col_sym, #Reads the column name as a variable
               rows = rank(-!!col_sym, ties.method = "first") <= 5,
               fn = scales::col_numeric(palette = "Reds", domain = NULL)) %>%
    #Inverted Green scale color
    data_color(columns = !!col_sym,
               rows = rank(!!col_sym, ties.method = "first") <= 5,
               fn = scales::col_numeric(palette = rev(scales::brewer_pal(palette = "Greens")(9)), domain = NULL))
}

#3)COMPLETING THE TABLE WITH LAST DETAILS
tab_low_indicators <- tab_low_indicators %>%
  #Separating line between PMR and rest of the indicators
  tab_style(style = cell_borders(sides = "right",
                                 color = "#94a3b8",
                                 weight = px(2), 
                                 style = "solid"),
            locations = list(cells_body(columns = PMR_2023),
                             cells_column_labels(columns = PMR_2023))) %>%
  #Line that separates OECD and non-OECD countries
  tab_style(style = cell_borders(sides = "bottom", 
                                 color = "#1e293b", 
                                 weight = px(2.5), 
                                 style = "solid"),
    locations = cells_body(columns = everything(),
                           rows = 38 #After 38th row (USA)
                           )) %>%
  #Customizing 'OECD Average' line
  tab_style(style = list(cell_fill(color = "#e2e8f0"), 
                         cell_text(weight = "bold", color = "#1e293b")),
            locations = cells_body(columns = everything(),
                                   rows = Code == "OECD Average")) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom"),
                                 color = "#94a3b8",
                                 weight = px(1.5)),
            locations = cells_body(columns = everything(),
                                   rows = Code == "OECD Average")) %>%
  tab_footnote(footnote = "The OECD Average is calculated exclusively using data from the first 38 OECD member countries listed above (until USA).",
               locations = cells_body(columns = Code, 
                                      rows = Code == "OECD Average")) %>%
  #Data Source and Author footnote
  tab_source_note(source_note = md("**[*]Data Source**: OECD Product Market Regulation (PMR) Database (2023-2024 assessment) | Author: Davide Antoniazzi")) %>%
  #To keep colors when exporting
  opt_css(css = "@media print {
  @page {
        size: A4 landscape;
        margin: 1cm;
      } * { 
  -webkit-print-color-adjust: exact !important; 
          print-color-adjust: exact !important; } 
  table {
        width: 100% !important;
        font-size: 11px !important;
      }}")

tab_low_indicators

#Exporting the table in 'Images' folder
gtsave(data = tab_low_indicators, 
       filename = "Images/2_PMR_Performance_2023_Low_Indicators.pdf",
       zoom = 1.5)
