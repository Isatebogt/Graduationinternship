library(shiny)
library(ggiraph)
library(bslib)
library(plotly)


source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/loaddata.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/GroupedAbundancePlot.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/explorationPCA.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/makeheatmap.R")


ui <- page_navbar(
  title = "Microbiota analysis",
  bg = "#34B233",
  inverse = TRUE,

  nav_panel(
    title = "Species composition",
    actionButton("average","Average"),
    fileInput("file1", "Choose a CSV file"),
    girafeOutput("species_plot", width = "100%", height = "500px")
  ),
  

  nav_panel(
    title = "PCA",
    plotlyOutput("PCA_plot")
  ),
  
  nav_panel(
    title = "Heatmap Beta diversity",
    fileInput("file2", "Choose Beta diversity file"),
    p("Please make sure you already loaded the abundance file before doing this step"),
    plotlyOutput("heatmap")
  ),
  
  nav_spacer()
)


server <- function(input, output, session) {
  

  average <- reactiveVal(FALSE)
  observeEvent(input$average, { 
    average(!average())
  })
  
  metadata_df <- eventReactive(input$file1, {
    load_species_data(input$file1$datapath)
  })
  
  species_plot_obj <- reactive({
    req(metadata_df())
    make_species_plot(metadata_df(), average())
  })

  df_metadata_heatmap <- eventReactive(input$file2, {
    load_sep_file(input$file2$datapath)
  })
  
  heatmap_output <- reactive({
    req(df_metadata_heatmap())
    make_heatmap(df_metadata_heatmap(), metadata_df())
  })
  
  output$species_plot <- renderGirafe({
    species_plot_obj()
  })
  
  output$PCA_plot <- renderPlotly({
    req(metadata_df())
    makepca(metadata_df())
  })
  
  output$heatmap <- renderPlotly({
    heatmap_output()
  })
}

shinyApp(ui, server)