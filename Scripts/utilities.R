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
                           col_names = FALSE) |> 
    
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
    ) |>
    
    dplyr::mutate(
      district = dplyr::case_when(stringr::str_detect(age, pattern = "idade.") ~ age,
                                  TRUE ~ NA_character_)) |> 
    
    tidyr::fill(district, .direction = "down") |> 
    dplyr::filter(!stringr::str_detect(age, pattern = "Idade|Total|Quadro")) |> 
    
    dplyr::mutate(district = stringr::str_extract(district, "(?<=idade\\.).*"),
                  year = input_tabs,
                  district = stringr::str_extract(district, "^.*(?=,)")) |> 
    
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
  
  df <- df |>
    
    pivot_longer(cols = total_total:rural_female, 
                 names_to = c("urban_rural", "sex"), 
                 names_sep = "_", 
                 values_to = "value") |> 
    
    filter(!urban_rural == "total",
           !sex == "total") |> 
    
    mutate(district = str_trim(district),
           age = if_else(age == "80+", "80", age),
           age = as.numeric(age),
           value = as.numeric(value),
           value = replace_na(value, 0)) |>   
    
    left_join(map_psnu, by = "district") |> 
    select(!district)
  
  if (output_type == "MISAU") { 
    
    df <- df |> 
      
      relocate(any_of(c("provincia", "distrito", "snuuid", "psnuuid")), .before = everything()) |> 
      select(!c("snu", "psnu")) |> 
      mutate(
        sex = if_else(sex == "male", "Masculino", "Feminino"),
        urban_rural = if_else(sex == "urban", "Urbano", "Rural"),
      ) |> 
      rename(periodo = year,
             idade = age,
             disaggregacao = urban_rural,
             sexo = sex,
             valor = value)
    
  } else if (output_type == "PEPFAR") {
    
    df <- df |> 
      
      relocate(any_of(c("snu", "psnu", "snuuid", "psnuuid")), .before = everything()) |> 
      select(!c("provincia", "distrito")) |> 
      mutate(
        sex = if_else(sex == "male", "Male", "Female"),
        urban_rural = if_else(sex == "urban", "Urban", "Rural"),
      )
    
  }
  
  return(df)
  
}




#' Title
#'
#' @param data 
#' @param group_level 
#' @param provincia_filter 
#' @param distrito_filter 
#' @param periodo_filter 
#'
#' @return
#' @export
#'
#' @examples

prep_pyramid_df <- function(data, group_level = "pais", provincia_filter = NULL, distrito_filter = NULL, periodo_filter = 2024) {
  library(dplyr)
  
  # Add pais variable with value "Mocambique"
  data <- data |>
    mutate(pais = "Mocambique")
  
  # Apply optional filters
  filtered_data <- data %>%
    {
      if (!is.null(provincia_filter)) filter(., provincia == provincia_filter) else .
    } %>%
    {
      if (!is.null(distrito_filter)) filter(., distrito == distrito_filter) else .
    } %>%
    {
      if (!is.null(periodo_filter)) filter(., periodo == periodo_filter) else .
    }
  
  # Define grouping variables based on user selection
  group_vars <- c("periodo", "idade", "sexo")
  
  if (group_level == "pais") {
    group_vars <- c("pais", group_vars)
  } else if (group_level == "provincia") {
    group_vars <- c("provincia", group_vars)
  } else if (group_level == "distrito") {
    group_vars <- c("distrito", group_vars)
  } else {
    stop("Invalid group_level. Choose one of 'pais', 'provincia', or 'distrito'.")
  }
  
  # Group and summarize
  summary <- filtered_data |>
    group_by(across(all_of(group_vars))) |>
    summarize(valor = sum(valor, na.rm = TRUE), .groups = 'drop')
  
  return(summary)
}


categorize_age_brackets <- function(data) {

  # Categorize idade into 5-year age brackets
  data <- data |>
    mutate(age_bracket = cut(idade,
                             breaks = seq(0, 80, by = 5),
                             labels = sprintf("%02d-%02d", seq(0, 75, by = 5), seq(4, 79, by = 5)),
                             include.lowest = TRUE,
                             right = FALSE)) |>
    mutate(age_bracket = ifelse(idade >= 80, "80+", as.character(age_bracket)))
  
  # Group by the new age_bracket, along with other grouping variables
  grouped_data <- data |>
    group_by(periodo, age_bracket, sexo, across(starts_with("pais")), across(starts_with("provincia")), across(starts_with("distrito"))) |>
    summarize(valor = sum(valor, na.rm = TRUE), .groups = 'drop')
  
  return(grouped_data)
  
}

viz_pyramid_df <- function(df) {
  plot <- ggplot(
    df, 
    aes(
      x = age_bracket, 
      fill = sexo, 
      y = ifelse(
        test = grepl("M", sexo), 
        yes = -valor, 
        no = valor
      )
    )
  )
  
  plot <- plot +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      labels = function(x) paste0(abs(x) / 1000, "k"),  # Format labels as "100k"
      limits = max(df$valor) * c(-1, 1),
      # breaks = seq(-max(df1$valor), max(df1$valor), by = 100000)  # Set breaks at 100,000 intervals
    ) + 
    coord_flip() + 
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)) +
    labs(
      x = NULL, 
      y = NULL, 
      fill = "Age", 
      title = glue::glue("Pirâmide Populacional ({ano})")) +
    # Expanding the x-axis limits to provide space for labels
    expand_limits(x = length(unique(df$age_bracket)) + 2) +
    # Adding "Masculino" and "Feminino" labels below the x-axis
    annotate("text", x = length(unique(df$age_bracket)) + 1, y = -max(df$valor) * 0.5, 
             label = "Masculino", size = 3.5, hjust = 1) +
    annotate("text", x = length(unique(df$age_bracket)) + 1, y = max(df$valor) * 0.5, 
             label = "Feminino", size = 3.5, hjust = 0)
}
