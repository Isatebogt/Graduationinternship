library(shiny)
library(ggiraph)
library(bslib)
library(plotly)
library(shinyBS)
options(shiny.autoreload = TRUE)


# Call other files to use them in APP script
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/loaddata.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/GroupedAbundancePlot.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/explorationPCA.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/RDAplot.R")
source("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/makeheatmap.R")

# User interface of shiny app
ui <- page_navbar(
  title = "Microbiota analysis",
  bg = "#34B233",
  inverse = TRUE,
  
  nav_panel(
    title = "Species composition",
    
    fluidRow(
      column(
        width = 8, offset = 2,
        
        accordion(
          open = "Panel 1",
          accordion_panel("Panel 1",
                          fluidRow(
                            column(9, fileInput("file1",
                                                "Choose CSV file - Species 1")),
                            column(3, tags$div(style = "margin-top: 25px;",
                                               actionButton("average1", "Average",
                                                            style = "height: 38px; 
                                                            width: 100%;
                                                            line-height: 38px;
                                                            padding: 0 16px;")))
                          ),
                          girafeOutput("species_plot1", width = "100%",
                                       height = "370px")),
          accordion_panel("Panel 2",
                          fluidRow(
                            column(9, fileInput("file3",
                                                "Choose CSV file - Species 2")),
                            column(3, tags$div(style = "margin-top: 25px;",
                                               actionButton("average2", "Average",
                                                            style = "height: 38px;
                                                            width: 100%;
                                                            line-height: 38px;
                                                            padding: 0 16px;")))
                          ),
                          p("Species dataset 2"),
                          girafeOutput("species_plot2", width = "100%",
                                       height = "350px"))
        )   
      )     
    )        
  ),        
  
  nav_panel(
    title = "PCA",
    plotlyOutput("PCA_plot")
  ),
  
 nav_panel(
   title = "RDA",
   plotlyOutput("RDA_plot")
   ),
  
  nav_panel(
    title = "Beta diversity",
    fluidRow(
      column(width = 8, offset = 2,
             accordion(open = "panel 3",
                       accordion_panel("panel 3", title = "Beta diversity",
                                       fileInput("file2", "Choose Beta diversity file"),
                                       p("Please load abundance file first"),
                                       plotlyOutput("heatmap", height = "600px")),
                       accordion_panel("panel 4", title = "boxplot",
                                       selectInput("dropdown", "Select day", choices = c("")),
                                       girafeOutput("boxplot",height = "600px", width = "80%"))
             )    
      )          
    )         
  ),             
  
  nav_spacer()
)     

# computatoinal logic of app
server <- function(input, output, session) {
  
  #----------------
  # abundance plots
  #----------------
  
  # Average button is first set to false. 
  average1 <- reactiveVal(FALSE)
  average2 <- reactiveVal(FALSE)
  
  # when clicked, 1st average button is true. 
  observeEvent(input$average1, {
    average1(!average1())
  })
  
  observeEvent(input$average2, {
    average2(!average2())
  })
  
  # contents of file is loaded. 
  metadata_df1 <- eventReactive(input$file1, {
    load_species_data(input$file1$datapath)
  })
  
  metadata_df2 <- eventReactive(input$file3, {
    load_species_data(input$file3$datapath)
  })
  
  # Make abundance plot
  species_plot_obj1 <- reactive({
    # req checks if there is content in the metadata_df1 dataframe. 
    req(metadata_df1())
    # make_species_plot makes the abundance plot. 
    make_species_plot(metadata_df1(), average1())
  })
  
  species_plot_obj2 <- reactive({
    req(metadata_df2())
    make_species_plot(metadata_df2(), average2())
  })
  
  # The plot is shown in the app. 
  output$species_plot1 <- renderGirafe({
    species_plot_obj1()
  })
  
  output$species_plot2 <- renderGirafe({
    species_plot_obj2()
  })

  #----------
  # PCA plot
  #----------
  
  # The metadata_df1 value is also directly used to make a PCA plot. 
  output$PCA_plot <- renderPlotly({
    req(metadata_df1())
    makepca(metadata_df1())
  })
  
  output$RDA_plot <- renderPlotly({
    req(metadata_df1())
    makeRDA(metadata_df1())
    })
  
  #------------------------
  # Heatmap beta diversity
  # -----------------------
  
  # If the Bray curtis file is put in, a heatmap will be made. 
  df_metadata_heatmap <- eventReactive(input$file2, {
    load_sep_file(input$file2$datapath)
    
  })
  
  prepped <- reactive({
    req(df_metadata_heatmap(), metadata_df1())
    prepare_data(df_metadata_heatmap(), metadata_df1())
  })
  
  output$heatmap <- renderPlotly({
    req(prepped())
    make_heatmap(prepped())
  })
  
  boxplot_data <- reactive({
    req(prepped())
    make_boxplot(prepped())
  })
  
  output$boxplot <- renderGirafe({
    boxplot_data()$plot
  })
  
  observeEvent(boxplot_data(), {
    days <- as.character(boxplot_data()$days)
    choices <- c("All days" = "all", setNames(days, days))
    updateSelectInput(session, "dropdown", choices = choices, selected = "all")
  })
  
  selected_boxplot_data <- reactive({
    req(prepped())
    day_filter <- if (input$dropdown == "all") NULL else input$dropdown
    make_boxplot(prepped(),day_filter)
  })
  
  output$boxplot <- renderGirafe({
    selected_boxplot_data()$plot
  })
  
  
}

shinyApp(ui, server)