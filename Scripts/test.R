
group_and_summarize_valor <- function(data, periodo_filter = 2024, group_var = c("idade", "sexo")) {
  
  # Check if the group_var provided is valid
  if(!group_var %in% c("idade", "sexo")) {
    stop("group_var must be either 'idade', or 'sexo'")
  }

  filtered_data <- data |> 
    filter(periodo == periodo_filter)
  
  if (group_var == "idade") {
    filtered_data <- filtered_data |> 
      mutate(
        grupo = case_when(
          idade == 0 ~ "<12 meses",
          idade >= 1 & idade <= 4 ~ "1-4 anos",
          idade >= 5 & idade <= 14 ~ "5-14 anos", 
          idade >= 15 & idade <= 49 ~ "15-49 anos", 
          idade >= 50 & idade <= 64 ~ "50-64 anos", 
          idade >= 65 ~ ">65 anos",
          TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
        )
      ) |> 
      filter(!is.na(grupo)) |> 
      mutate(
        grupo = factor(grupo, levels = c("<12 meses", "1-4 anos", "5-14 anos", "15-49 anos", "50-64 anos", ">65 anos"))
      ) |> 
      group_by(grupo) |> 
      summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
  }
  
  if (group_var == "idade") {
    filtered_data <- filtered_data |> 
      mutate(
        grupo = case_when(
          idade == 0 ~ "<12 meses",
          idade >= 1 & idade <= 4 ~ "1-4 anos",
          idade >= 5 & idade <= 14 ~ "5-14 anos", 
          idade >= 15 & idade <= 49 ~ "15-49 anos", 
          idade >= 50 & idade <= 64 ~ "50-64 anos", 
          idade >= 65 ~ ">65 anos",
          TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
        )
      ) |> 
      filter(!is.na(grupo)) |> 
      mutate(
        grupo = factor(grupo, levels = c("<12 meses", "1-4 anos", "5-14 anos", "15-49 anos", "50-64 anos", ">65 anos"))
      ) |> 
      group_by(grupo) |> 
      summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
  }
    
    return(filtered_data)
    
  }
  

  test <- group_and_summarize_valor(df, group_var = "idade")
    
  
  
df |> 
  filter(periodo == 2024) |> 
  mutate(
    grupo = case_when(
      idade == 0 ~ "<12 meses",
      idade >= 1 & idade <= 4 ~ "1-4 anos",
      idade >= 5 & idade <= 14 ~ "5-14 anos", 
      idade >= 15 & idade <= 49 ~ "15-49 anos", 
      idade >= 50 & idade <= 64 ~ "50-64 anos", 
      idade >= 65 ~ ">65 anos",
      TRUE ~ NA_character_  # Assign NA for any idades outside the defined ranges
    )
  ) |> 
  filter(!is.na(grupo)) |> 
  mutate(
    grupo = factor(grupo, levels = c("<12 meses", "1-4 anos", "5-14 anos", "15-49 anos", "50-64 anos", ">65 anos"))
  ) |> 
  group_by(grupo) |> 
  summarize(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')
