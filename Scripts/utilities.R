# All functions used to munge and compile INE demographic projection files

#' Create all year-quarters between the start and end date.
#'
#' @param filename the path to the .xlsx file containing INE provincial demographic projections
#' @param tab the sheetname for an annual demographic projection
#'
#' @return a dataframe with compiled district population estimates
#' @export
#'
#' @examples

extract_demproj <- function(filename, tab) {
  
  df <- readxl::read_excel(path = filename,
                           sheet = tab,
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
                  year = tab,
                  district = stringr::str_extract(district, "^.*(?=,)")) %>% 
    
    dplyr::relocate(tidyselect::any_of(c("district", "year")), .before = everything())
  
  return(df)
  
}



# All functions used to tidy INE demographic projection files

#' Create all year-quarters between the start and end date.
#'
#' @param df dataframe of INE population projections after munging via `extract_demproj`
#' @param output_type defines the language of variables
#'
#' @return a tidy dataframe with cleaned population estimates
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
           value = replace_na(value, 0)) %>%   #NEW - trim all districts
    
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