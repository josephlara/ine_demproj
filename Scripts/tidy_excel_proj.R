library(tidyverse)
library(janitor)
library(openxlsx)
library(readxl)
library(fs)

# FUNCTION ----------------------------------------------------------------

# 
# extract_demproj <- function(filename, tab) {
#   
#   df <- read_excel(path = filename,
#                    sheet = tab,
#                    skip = 87,
#                    col_names = FALSE) %>% 
#     rename(
#       age = `...1`,
#       total_total = `...2`,
#       total_male = `...3`,
#       total_female = `...4`,
#       urban_total = `...5`,
#       urban_male = `...6`,
#       urban_female = `...7`,
#       rural_total = `...8`,
#       rural_male = `...9`,
#       rural_female = `...10`
#     ) %>%
#     mutate(district = case_when(str_detect(age, pattern = "idade. ") ~ age,
#                                 TRUE ~ NA_character_)) %>% 
#     fill(district, .direction = "down") %>% 
#     filter(!str_detect(age, pattern = "Idade|Total|Quadro")) %>% 
#     mutate(district = str_extract(district, "(?<=idade\\. ).*")) %>% 
#     mutate(year = str_extract(district, "(?<=,).*")) %>% 
#     mutate(district = str_extract(district, "^.*(?=,)"))
#   
#   year_var <- str_trim(df$year[1])
#   
#   df <- df %>% 
#     mutate(year = year_var) %>% 
#     relocate(any_of(c("district", "year")), .before = everything())
#   
#   return(df)
#   
# }


path_df <- "Data/nampula.xlsx"

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

df <- extract_demproj(path_df, "2022")


excel_tabs <- excel_sheets(path_df)

df_combine <- map_dfr(.x = c(excel_tabs),
                      .f = ~ extract_demproj(path_df, .x))

unique(df_combine$year)

# PATHS -------------------------------------------------------------------


# paths & values
path_data_repo <- "Data/" # path to repo where original files are stored
path_dataout <- "Dataout/" # path to repo where corrected files will be saved

path_demproj_repo <- dir({path_data_repo}, pattern = "*.xlsx")

tpt_historic <- path_demproj_repo %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 

# batch load files to be processed
batch_list <- path_data_repo %>% 
  dir_ls() %>% 
  map(
    .f = function(path){
      read_tsv(
        path
      )
    }
  )

# PROCESSING --------------------------------------------------------------

path_df_nam <- "Data/nampula.xlsx"

df <- extract_demproj(filename = path_df_nam, tab = "2021")
unique(df$year)

excel_tabs <- excel_sheets(path_df_nam)

df_combine <- map_dfr(.x = c(excel_tabs),
                      .f = ~ extract_demproj(path_df_nam, .x))


unique(df_combine$year)

