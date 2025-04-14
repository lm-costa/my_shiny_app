sankey_plot <- function(file_name,sheet_name){
  df <- readxl::read_excel(file_name,sheet = sheet_name)
  
  df_long <- df |> 
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
    dplyr::mutate(
      'source'=from_class,
      'target'=to_class,
      'value'=area_sq_m
    ) |> 
    dplyr::select(source,target,value)
  
  df_long <- df_long |> 
    dplyr::group_by(source,target) |> 
    dplyr::summarise(
      value=sum(value)*0.87
    ) |>
    dplyr::ungroup() |> 
    as.data.frame()
  
  df_long$target <- paste(df_long$target, " ", sep="")
  
  
  nodes <- data.frame(name=c(as.character(df_long$source), 
                             as.character(df_long$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  df_long$IDsource=match(df_long$source, nodes$name)-1 
  df_long$IDtarget=match(df_long$target, nodes$name)-1
  
  
  
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF"])'
  
  # Make the Network
  p <- sankeyNetwork(Links = df_long, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=T, colourScale=ColourScal,
                     nodeWidth=100, fontSize=20, nodePadding=60,
                     width = 1000, 
                     height = 600
  )
  
  return(p)
  
}
