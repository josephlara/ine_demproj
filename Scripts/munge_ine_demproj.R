# PROJECT:  INE Demographic Projection Munging
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   357505b4 
# LICENSE:  MIT
# DATE:     2024-08-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(janitor)
library(openxlsx)
library(readxl)
library(fs)
library(googlesheets4)
library(glamr)
load_secrets()

# GLOBAL VARIABLES --------------------------------------------------------

INPUT_FOLDER <- "Data/population"
INPUT_TABS <- c(as.character(c(2017:2050)))

# FUNCTIONS-------------------------------------------------------------------

source("Scripts/utilities.R")

# MUNGE -------------------------------------------------------------------

map_psnu <- read_sheet(as_sheets_id("13mufLaIKsTdgBx5FKPuoHeqt4UMrfx4eZgaEjDW9qJQ"), 
                       sheet = "ine_demproj_psnu")

all_population_input_files <- dir(INPUT_FOLDER,
                                  full.name = TRUE,
                                  pattern = "*.xlsx")

all_population_df <- map(all_population_input_files, function(file) {
  # Process each sheet for the current file
  file_data <- map(INPUT_TABS, function(sheet) {
    extract_demproj(file, sheet)
  })
  # Combine data from all sheets for the current file
  bind_rows(file_data)
}) %>%
  # Combine data from all files
  bind_rows()

# MUNGE -------------------------------------------------------------------

all_pop_final_df <- all_population_df |> 
  clean_demproj(output_type = "MISAU")

# create value indicating if output file is for MISAU or PEPFAR
if ("snu" %in% colnames(all_pop_final_df)) {
  
  output_file <- "pepfar"
  
} else {
  
  output_file <- "misau"
  
}

# DATAOUT -------------------------------------------------------------------

write_csv(x = all_pop_final_df, 
          file = glue::glue("Dataout/ine_demproj_{output_file}.csv")
)

