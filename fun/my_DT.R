my_DT <- function(file_name,sheet_name){
  df <- readxl::read_excel(file_name,sheet = sheet_name)
  
  df <- df |> 
    dplyr::mutate(
      `From Class`=dplyr::case_when(
        `From Class` =='FOREST AND SEMI[1]NATURAL AREAS' ~ 'FOREST AND SEMINATURAL AREAS',
        .default = `From Class`
      ) ,
      `To Class`=dplyr::case_when(
        `To Class` =='FOREST AND SEMI[1]NATURAL AREAS' ~ 'FOREST AND SEMINATURAL AREAS',
        .default = `To Class`
      )
    ) |> 
    janitor::clean_names() |>
    dplyr::select('from_class','to_class','area_sq_m') |> 
    dplyr::mutate(area_sq_m = area_sq_m*0.87)
  return(df)
}