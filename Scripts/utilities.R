#' This functions reads in all population datasets from a specified folder
#'
#' @param filename province files 
#' @param input_tabs tabs in the file to be read
#'
#' @return a dataset of all files and tabs
#' @export
#'
#' @examples

extract_demproj <- function(filename, input_tabs) {
  
  df <- readxl::read_excel(path = filename,
                           sheet = input_tabs,
                           skip = 87,
                           col_names = FALSE) %>% 
    
    dplyr::rename(
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
    
    dplyr::mutate(
      district = dplyr::case_when(stringr::str_detect(age, pattern = "idade.") ~ age,
                                  TRUE ~ NA_character_)) %>% 
    
    tidyr::fill(district, .direction = "down") %>% 
    dplyr::filter(!stringr::str_detect(age, pattern = "Idade|Total|Quadro")) %>% 
    
    dplyr::mutate(district = stringr::str_extract(district, "(?<=idade\\.).*"),
                  year = input_tabs,
                  district = stringr::str_extract(district, "^.*(?=,)")) %>% 
    
    dplyr::relocate(tidyselect::any_of(c("district", "year")), .before = everything())
  
  return(df)
  
}


#' This function cleans population data
#'
#' @param df a population dataset
#' @param output_type 
#'
#' @return a cleaned dataset
#' @export
#'
#' @examples

clean_demproj <- function(df, output_type = "MISAU") {
  
  df <- df %>%
    
    pivot_longer(cols = total_total:rural_female, 
                 names_to = c("urban_rural", "sex"), 
                 names_sep = "_", 
                 values_to = "value") %>% 
    
    filter(!urban_rural == "total",
           !sex == "total") %>% 
    
    mutate(district = str_trim(district),
           age = if_else(age == "80+", "80", age),
           age = as.numeric(age),
           value = as.numeric(value),
           value = replace_na(value, 0)) %>%   
    
    left_join(map_psnu, by = "district") %>% 
    select(!district)
  
  if (output_type == "MISAU") {
    
    df <- df %>% 
      
      relocate(any_of(c("provincia", "distrito", "snuuid", "psnuuid")), .before = everything()) %>% 
      select(!c("snu", "psnu")) %>% 
      rename(periodo = year,
             idade = age,
             disaggregacao = urban_rural,
             sexo = sex,
             valor = value)
    
  } else if (output_type == "PEPFAR") {
    
    df <- df %>% 
      
      relocate(any_of(c("snu", "psnu", "snuuid", "psnuuid")), .before = everything()) %>% 
      select(!c("provincia", "distrito"))
    
  }
  
  return(df)
  
}
