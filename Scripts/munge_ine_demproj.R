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

source("Scripts/utilities.R")

input_tabs <- c(as.character(c(2017:2050)))

# input_files <- dir("Data/", pattern = "*.xlsx", full.names = TRUE)
path_niassa <- "Data/niassa.xlsx"
path_cabo_delgado <- "Data/cabo_delgado.xlsx"
path_nampula <- "Data/nampula.xlsx"
path_zambezia <- "Data/zambezia.xlsx"
path_tete <- "Data/tete.xlsx"
path_manica <- "Data/manica.xlsx"
path_sofala <- "Data/sofala.xlsx"
path_inhambane <- "Data/inhambane.xlsx"
path_gaza <- "Data/gaza.xlsx"
path_maputo_province <- "Data/maputo_province.xlsx"
path_maputo_cidade <- "Data/maputo_cidade.xlsx"

# load googlesheet containing psnu renaming map
map_psnu <- read_sheet(as_sheets_id("13mufLaIKsTdgBx5FKPuoHeqt4UMrfx4eZgaEjDW9qJQ"), 
                       sheet = "ine_demproj_psnu")


# MUNGE -------------------------------------------------------------------


df_niassa <- map(.x = input_tabs,
                 .f = ~ extract_demproj(path_niassa, .x)) %>% 
  reduce(rbind)


df_cabo_delgado <- map(.x = input_tabs,
                       .f = ~ extract_demproj(path_cabo_delgado, .x)) %>% 
  reduce(rbind)


df_nampula <- map(.x = input_tabs,
                  .f = ~ extract_demproj(path_nampula, .x)) %>% 
  reduce(rbind)


df_zambezia <- map(.x = input_tabs,
                   .f = ~ extract_demproj(path_zambezia, .x)) %>% 
  reduce(rbind)

df_tete <- map(.x = input_tabs,
               .f = ~ extract_demproj(path_tete, .x)) %>% 
  reduce(rbind)


df_manica <- map(.x = input_tabs,
                 .f = ~ extract_demproj(path_manica, .x)) %>% 
  reduce(rbind)


df_sofala <- map(.x = input_tabs,
                 .f = ~ extract_demproj(path_sofala, .x)) |> 
  reduce(rbind)


df_inhambane <- map(.x = input_tabs,
                    .f = ~ extract_demproj(path_inhambane, .x)) %>% 
  reduce(rbind)


df_gaza <- map(.x = input_tabs,
               .f = ~ extract_demproj(path_gaza, .x)) %>% 
  reduce(rbind)


df_maputo_province <- map(.x = input_tabs,
                          .f = ~ extract_demproj(path_maputo_province, .x)) %>% 
  reduce(rbind)


df_maputo_cidade <- map(.x = input_tabs,
                        .f = ~ extract_demproj(path_maputo_cidade, .x)) |> 
  reduce(rbind)


df <- bind_rows(df_niassa, 
                df_cabo_delgado, 
                df_nampula, 
                df_zambezia, 
                df_tete,
                df_manica,
                df_sofala,
                df_inhambane,
                df_gaza,
                df_maputo_province,
                df_maputo_cidade) %>%
  clean_demproj(output_type = "MISAU")


# MUNGE -------------------------------------------------------------------


write_csv(df, "Dataout/ine_demproj_misau.csv")

