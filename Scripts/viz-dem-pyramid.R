# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   67e9a69f 
# LICENSE:  MIT
# DATE:     2024-10-03
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(janitor)
  library(glue)
  library(scales)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "67e9a69f"
  ano <- "2024"
  source("Scripts/utilities.R")
  
# LOAD DATA ------------------------------------------------------------------
  
  df <- read_csv("Dataout/ine_demproj_misau.csv")

# MUNGE -------------------------------------------------------------------
  

  df1 <- prep_pyramid_df(data = df, 
                         group_level = "pais") |> 
    categorize_age_brackets()
  
  
  plot <- viz_pyramid_df(df1)
  
  plot

  
  
  
  
  
  
  
  group_and_summarize_valor <- function(data, group_var = c("idade", "sexo"), periodo_filter = 2024) {
    library(dplyr)
    
    # Check if the group_var provided is valid
    if(!group_var %in% c("idade", "sexo")) {
      stop("group_var must be either 'idade' or 'sexo'")
    }
    
    # Filter the data by the given periodo
    filtered_data <- data %>%
      filter(periodo == periodo_filter)
    
    # Group and summarize
    summary <- filtered_data %>%
      group_by(across(all_of(group_var))) %>%
      summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
    
    return(summary)
  }
  

  
  df1 <- group_and_summarize_valor(df, group_var = "sexo") 

  
  
  
  
  
  
  
  
  
  
  
  
  group_and_summarize_valor <- function(data, group_var = c("idade", "sexo", "grupos"), periodo_filter = 2024) {
    
    # Check if the group_var provided is valid
    if(!group_var %in% c("idade", "sexo", "grupos")) {
      stop("group_var must be either 'idade', 'sexo', or 'grupos'")
    }
    
    # Filter the data by the given periodo
    filtered_data <- data %>%
      filter(periodo == periodo_filter)
    
    # Apply custom grouping if group_var is "grupos"
    if (group_var == "grupos") {
      filtered_data <- filtered_data %>%
        mutate(
          grupos = case_when(
            idade == 0 ~ "<12 meses",
            idade >= 1 & idade <= 5 ~ "12-59m",
            idade >= 15 & idade <= 24 ~ "15-24a",
            TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
          )
        ) %>%
        filter(!is.na(grupos))  # Remove rows where grupos is NA
    }
    
    if (group_var == "idade") {
      filtered_data <- filtered_data %>%
        mutate(
          grupos = case_when(
            idade == 0 ~ "<12 meses",
            idade >= 1 & idade <= 4 ~ "1-4 anos",
            idade >= 5 & idade <= 14 ~ "5-14 anos", 
            idade >= 15 & idade <= 49 ~ "15-49 anos", 
            idade >= 50 & idade <= 64 ~ "50-64 anos", 
            idade >= 65 ~ ">65 anos", 
            TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
          )
        ) %>%
        filter(!is.na(grupos))  # Remove rows where grupos is NA
    }
    
    
    if (group_var == "sexo") {
      filtered_data <- filtered_data %>%
        mutate(
          grupos = case_when(
            idade == "Masculino" ~ "Masculino",
            idade == "Feminino" ~ "Feminino",
            TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
          )
        ) %>%
        filter(!is.na(grupos))  # Remove rows where grupos is NA
    }
    
    # Group and summarize
    summary <- filtered_data %>%
      group_by(across(all_of(group_var))) %>%
      summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
    
    return(summary)
    
  }
  
  
  
  df1 <- group_and_summarize_valor(df, group_var = "idade") 

  
  
  
  
  
  
  group_and_summarize_valor <- function(data, group_var = c("idade", "sexo", "grupos"), periodo_filter = 2024) {
    library(dplyr)
    
    # Check if the group_var provided is valid
    if(!group_var %in% c("idade", "sexo", "grupos")) {
      stop("group_var must be either 'idade', 'sexo', or 'grupos'")
    }
    
    # Filter the data by the given periodo
    filtered_data <- data %>%
      filter(periodo == periodo_filter)
    
    # Handle custom grouping if group_var is "grupos"
    if (group_var == "grupos") {
      summary <- filtered_data %>%
        summarise(
          # Summarize for idade == 0
          `<12m` = sum(valor[idade == 0], na.rm = TRUE),
          # Summarize for idade between 1 and 5
          `12-59m` = sum(valor[idade >= 1 & idade <= 5], na.rm = TRUE),
          # Summarize for idade between 15 and 24
          `15-24a` = sum(valor[idade >= 15 & idade <= 24], na.rm = TRUE),
          # Summarize for idade between 15 and 49 and sexo is Feminino
          `MIF` = sum(valor[idade >= 15 & idade <= 49 & sexo == "Feminino"], na.rm = TRUE)
        )
    } else {
      # Group and summarize based on the selected group_var
      summary <- filtered_data %>%
        group_by(across(all_of(group_var))) %>%
        summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
    }
    
    return(summary)
  }

  result <- group_and_summarize_valor(data = df, group_var = "grupos")
  print(result)  
  