
library(tidyverse)
library(janitor)
library(openxlsx)
library(readxl)
library(fs)
library(googlesheets4)
library(glamr)
load_secrets()

# FUNCTION ----------------------------------------------------------------

extract_demproj <- function(filename, tab) {
  
  df <- read_excel(path = filename,
                   sheet = tab,
                   skip = 87,
                   col_names = FALSE) %>% 
    rename(
      age = `...1`,
      total_total = `...2`,
      total_male = `...3`,
      total_female = `...4`,
      urban_total = `...5`,
      urban_male = `...6`,
      urban_female = `...7`,
      rural_total = `...8`,
      rural_male = `...9`,
      rural_female = `...10`
    ) %>%
    mutate(district = case_when(str_detect(age, pattern = "idade. ") ~ age,
                                TRUE ~ NA_character_)) %>% 
    fill(district, .direction = "down") %>% 
    filter(!str_detect(age, pattern = "Idade|Total|Quadro")) %>% 
    mutate(district = str_extract(district, "(?<=idade\\. ).*")) %>% 
    mutate(year = tab) %>% 
    mutate(district = str_extract(district, "^.*(?=,)")) %>% 
    relocate(any_of(c("district", "year")), .before = everything())
  
  return(df)
  
}

# PATHS & LISTS -------------------------------------------------------------------

path_nampula <- "Data/nampula.xlsx"
path_sofala <- "Data/sofala.xlsx"
path_manica <- "Data/manica.xlsx"
path_tete <- "Data/tete.xlsx"
path_niassa <- "Data/niassa.xlsx"
path_gaza <- "Data/gaza.xlsx"
path_zambezia <- "Data/zambezia.xlsx"
path_cabo_delgado <- "Data/cabo_delgado.xlsx"
path_inhambane <- "Data/inhambane.xlsx"
path_maputo_cidade <- "Data/maputo_cidade.xlsx"
path_maputo_province <- "Data/maputo_province.xlsx"

excel_tabs <- as.character(c(2017:2050))

map_psnu <- read_sheet(as_sheets_id("13mufLaIKsTdgBx5FKPuoHeqt4UMrfx4eZgaEjDW9qJQ"), sheet = "ine_demproj_psnu")

# PROCESSING --------------------------------------------------------------

df_nampula <- map_dfr(.x = c(excel_tabs),
                      .f = ~ extract_demproj(path_nampula, .x))

df_sofala <- map_dfr(.x = c(excel_tabs),
                     .f = ~ extract_demproj(path_sofala, .x))

df_manica <- map_dfr(.x = c(excel_tabs),
                     .f = ~ extract_demproj(path_manica, .x))

df_tete <- map_dfr(.x = c(excel_tabs),
                   .f = ~ extract_demproj(path_tete, .x))

df_niassa <- map_dfr(.x = c(excel_tabs),
                     .f = ~ extract_demproj(path_niassa, .x))

df_cabo_delgado <- map_dfr(.x = c(excel_tabs),
                           .f = ~ extract_demproj(path_cabo_delgado, .x))

df_zambezia <- map_dfr(.x = c(excel_tabs),
                       .f = ~ extract_demproj(path_zambezia, .x))

df_inhambane <- map_dfr(.x = c(excel_tabs),
                        .f = ~ extract_demproj(path_inhambane, .x))

df_gaza <- map_dfr(.x = c(excel_tabs),
                   .f = ~ extract_demproj(path_gaza, .x))

df_maputo_province <- map_dfr(.x = c(excel_tabs),
                              .f = ~ extract_demproj(path_maputo_province, .x))

df_maputo_cidade <- map_dfr(.x = c(excel_tabs),
                            .f = ~ extract_demproj(path_maputo_cidade, .x))

# COMPILE -----------------------------------------------------------------

df_compile <- bind_rows(df_cabo_delgado, 
                        df_gaza, 
                        df_inhambane, 
                        df_manica, 
                        df_maputo_cidade,
                        df_maputo_province,
                        df_nampula,
                        df_niassa,
                        df_sofala,
                        df_tete,
                        df_zambezia) %>% 
  pivot_longer(cols = total_total:rural_female, names_to = c("urban_rural", "sex"), names_sep = "_", values_to = "value") %>% 
  filter(!urban_rural == "total",
         !sex == "total") %>% 
  left_join(map_psnu, by = "district") %>% 
  relocate(any_of(c("snu", "psnu", "snuuid", "psnuuid")), .before = everything()) %>% 
  select(!district)

# WRITE TO DISK -----------------------------------------------------------

write_csv(df_compile,
          "Dataout/ine_demproj.csv",
          na = "")