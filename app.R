library(shinylive)
library(httpuv)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(treemapify)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
purrr::map(list.files('fun/',full.names = T),source)

###

file_name <- list.files('data-raw/',full.names = T)


###
# header 

header <- dashboardHeader(
  title = 'Land Use Explorer'
)

# my sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    # first page
    menuItem('Home', tabName = 'home',icon = icon('dashboard')),
    
    # Treemap page
    
    menuItem("Treemap", tabName = 'treemap',icon = icon('chart-bar')),
    
    ## given options to make the plot
    selectInput(
      inputId = "select_local",
      label = "Select Local",
      choices = c(
        "Local 1",
        "Local 2",
        "Local 3"
      ),
      selected = "Local 1",
      multiple = FALSE
    ),
    
    selectInput(
      inputId = "select_year",
      label = "Select Year",
      choices = c(
        2012,
        2017,
        2022
      ),
      selected = 2012,
      multiple = FALSE
    ),
    
    # Sankey Item
    menuItem("Sankey", tabName = 'sankey',icon = icon('chart-bar')),
    
    ## Given options
    
    selectInput(
      inputId = "select_local_sankey",
      label = "Select Local",
      choices = c(
        "Local 4",
        "Local 5",
        "Local 6"
      ),
      selected = "Local 4",
      multiple = FALSE
    )
  )
)

# my body 

body <- dashboardBody(
  # My main page
  tabItems(
    tabItem(tabName = 'home', 
            HTML("
              <h3>Welcome to the Land Use Explorer</h3>
              <p>This is my <strong>first Shiny app</strong>, designed to explore and visualize land use patterns in an interactive way.</p>
              <p>The application provides insights through two powerful tools:</p>
              <ul>
                <li><strong>Land Use Composition</strong>: Explore the distribution of different land use categories using an intuitive <strong>treemap</strong>. This section highlights the percentage share of each land use type, offering a clear snapshot of land cover patterns.</li>
                <li><strong>Land Use Transitions</strong>: Dive into dynamic changes over time with a <strong>Sankey diagram</strong>, illustrating how land use categories have transformed. Trace the flow of transitions to understand key trends and shifts in land management.</li>
              </ul>
              <p>Use the sidebar to navigate between views and gain insights into how landscapes evolve and interact over time.</p>
            ")
            ),
    
    # Treemap Page
    tabItem(
      tabName = 'treemap',
      h2(
        plotOutput(
          'treemap_plot'
        )
      )
      
    ),
    
    # Sankey page
    tabItem(tabName = 'sankey', 
            mainPanel(
              # sankey plot panel
              tabsetPanel(
                type='tabs',
                tabPanel(
                  "Sankey plot",
                  sankeyNetworkOutput(
                    'sankey_plot'
                  ),
                  
                ),
                # data panel
                tabPanel(
                  "Data",
                  DT::dataTableOutput('transition_data')
                )
              )
            )

    )
  )
)



###


ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) { 
  # Treemap
  ## treemap output
  output$treemap_plot <- renderPlot(
    my_treemap_fun(file_name |> as_tibble() |> 
                     filter(str_detect(value,'LUC')) |> 
                     pull(value),
                   sheet_name = input$select_local,
                   year_select = input$select_year)
  )
  # Sankey
  ## plot output
  output$sankey_plot <- renderSankeyNetwork(
    sankey_plot(file_name |> as_tibble() |> 
                     filter(str_detect(value,'sankey')) |> 
                     pull(value),
                   sheet_name = input$select_local_sankey
  ))
  
  ## data output
  output$transition_data <- 
    DT::renderDT(
      my_DT(
        file_name |> as_tibble() |> 
          filter(str_detect(value,'sankey')) |> 
          pull(value),
        sheet= input$select_local_sankey
        
      ) |> 
        mutate(Area_ha = area_sq_m/10000)
    )
  
  }

shinyApp(ui, server)
