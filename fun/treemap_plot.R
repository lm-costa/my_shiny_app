
my_treemap_fun <- function(file_name,sheet_name,year_select){
  df <- readxl::read_excel(file_name,sheet_name)
  
  my_plot <- df |>
    dplyr::filter(year==year_select) |> 
    arrange(Class) |>
    ungroup()  |>
    mutate(area_p = Area_m2/sum(Area_m2)*100)  |>
    ggplot(aes(area = area_p, fill = Class))+
    geom_treemap() +
    geom_treemap_text(
      aes(label = paste(Class,
                        paste0(round(area_p, 2), "%"), sep = "\n")),
      colour = "white") +
    theme(legend.position = "none")
  
  return(my_plot)
  
}
