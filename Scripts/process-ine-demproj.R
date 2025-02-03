# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   749cb784 
# LICENSE:  MIT
# DATE:     2024-10-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(janitor)
  library(glue)
  library(openxlsx)
  library(readxl)
  library(fs)
  library(googlesheets4)
  library(glamr)
  load_secrets()
  
  source("Scripts/utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  input_folder <- "Data/population"
  input_sheets <- c(as.character(c(2023:2050)))
  
  ref_id <- "749cb784"
  
# LOAD DATA ------------------------------------------------------------------
  
  map_psnu <- read_sheet(as_sheets_id("13mufLaIKsTdgBx5FKPuoHeqt4UMrfx4eZgaEjDW9qJQ"), 
                         sheet = "ine_demproj_psnu")
  
  file_inventory <- dir(input_folder,
                        full.name = TRUE,
                        pattern = "*.xlsx")
  
# MUNGE -------------------------------------------------------------------
  
  df <- map(file_inventory, function(file) {
    
    # Process each sheet for the current file
    file_data <- map(input_sheets, function(sheet) {
      extract_demproj(file, sheet)
    })
    
    # Combine data from all sheets for the current file
    bind_rows(file_data)
  }) |> 
    
    # Combine data from all files
    bind_rows()
  
  
  
  df_final <- df |> 
    clean_demproj(output_type = "MISAU")
  
  # create value indicating if output file is for MISAU or PEPFAR
  if ("snu" %in% colnames(df_final)) {
    
    output_file <- "pepfar"
    
  } else {
    
    output_file <- "misau"
    
  }
  
  
  # DATAOUT -------------------------------------------------------------------
  
  write_csv(df_final, 
            glue("Dataout/ine_demproj_{output_file}.csv")
  )
  