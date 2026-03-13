library(shiny)
library(ggiraph)
library(bslib)

# load helper files
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/loaddata.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/GroupedAbundancePlot.R")

ui <- page_navbar(
  title = "Microbiota analysis",
  bg = "#34B233",
  inverse = TRUE,
  
  nav_panel(
    title = "Species composition",
    
    fileInput("file1", "Choose a CSV file"),
    
    girafeOutput("species_plot", width = "100%", height = "500px"),
    
    verbatimTextOutput("file1_contents")
  ),
  
  nav_panel(
    title = "Two",
    p("Second page content.")
  ),
  
  nav_panel(
    title = "Three",
    p("Third page content.")
  ),
  
  nav_spacer()
)


server <- function(input, output, session){
  
  # reactive data loader
  metadata_df <- reactive({
    req(input$file1)   # wait until file uploaded
    load_species_data(input$file1$datapath)
  })
  
  # show file info (optional)
  output$file1_contents <- renderPrint({
    req(input$file1)
    input$file1
  })
  
  # render plot
  output$species_plot <- renderGirafe({
    req(metadata_df())
    make_species_plot(metadata_df())
  })
  
}

shinyApp(ui, server)