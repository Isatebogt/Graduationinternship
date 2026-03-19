
library(shiny)
library(ggiraph)
library(bslib)
library(plotly)

source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/loaddata.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/GroupedAbundancePlot.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/explorationPCA.R")

ui <- page_navbar(
  title = "Microbiota analysis",
  bg = "#34B233",
  inverse = TRUE,
  
  nav_panel(
    title = "Species composition",
    
    actionButton("average","Average"),
    
    fileInput("file1", "Choose a CSV file"),
    
    girafeOutput("species_plot", width = "100%", height = "500px"),
    
  ),
  
  nav_panel(
    title = "PCA",
    plotlyOutput(outputId = "PCA_plot")
  ),
  
  nav_panel(
    title = "test",
    p("test")
  ),
  
  nav_spacer()
)


server <- function(input, output, session){

  
  average <- reactiveVal(FALSE)
  
  metadata_df <- eventReactive(input$file1, {
    df <- load_species_data(input$file1$datapath)
  })
  
  observeEvent(input$average, { 
    average(!average())
  })
  
  species_plot_obj <- reactive({
    req(metadata_df())
    make_species_plot(metadata_df(), average())
  })
  
  
  
  output$species_plot <- renderGirafe({
    species_plot_obj()
  })
  
  output$PCA_plot <- renderPlotly({
    req(metadata_df())
    makepca(metadata_df())
  })
}

shinyApp(ui, server)