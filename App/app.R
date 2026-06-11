library(shiny)
library(ggiraph)
library(bslib)
library(plotly)
library(shinyjs)
library(shinyBS)
options(shiny.autoreload = TRUE)
# This makes bigger loading bigger datasets possible. 
options(shiny.maxRequestSize = 30 * 1024^2) 

# function: App script to run the Shiny app. 


# Call other files to use them in APP script
source("loaddata.R")
source("CompositionPlot.R")
source("PCAplot.R")
source("RDAplot.R")
source("HeatmapBoxplotBeta.R")


# User interface of shiny app
ui <- page_navbar(
  useShinyjs(),
  title = "Microbiota analysis",
  bg = "#34B233",
  inverse = TRUE,
  
  nav_panel(title = "composition",
    fluidRow(
      column(
        width = 8, offset = 2,
        
        # Using accordion cards for visibility. 
        accordion(
          open = "Panel 1",
          accordion_panel("Panel 1",
                          # Fluid row makes the layout nicer. 
                          fluidRow(
                            column(9, fileInput("file1",
                                                "Choose CSV file")),
                            column(3, tags$div(style = "margin-top: 25px;",
                                               # Action button to calculate the 
                                               # average
                                               actionButton("average1",
                                                            "Average",
                                                            style =
                                                            "height: 38px; 
                                                            width: 100%;
                                                            line-height: 38px;
                                                            padding: 0 16px;")))
                          ),
                          # Output of composition plot.
                          girafeOutput("species_plot1", width = "100%",
                                       height = "370px")),
          # Second panel potentially for comparison
          accordion_panel("Panel 2",
                          fluidRow(
                            column(9, fileInput("file3",
                                                "Choose CSV file")),
                            column(3, tags$div(style = "margin-top: 25px;",
                                               actionButton("average2", 
                                                            "Average",
                                                            style = "height:
                                                            38px;
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
  # PCA plot 
  nav_panel(
    title = "PCA",
    fluidRow(
      column(
        width = 8,
        offset = 2,
        plotlyOutput("PCA_plot", height = "500px", width = "80%"),
        selectInput(
          "dropdown_PCA",
          "Select Genotype",
          choices = c("All", "WT", "KO")
        ),
        fluidRow(
          # filter for days
          column(9, textInput("filter_pca", "Filter days",
                              placeholder = "'Day 56 and Day 90' or 'Day 7'")),
          column(3, tags$div(style = "margin-top: 25px;",
                             actionButton("filter_pca_submit", "Filter",
                                          style = "height: 38px; width: 100%;
                                        line-height: 38px; padding: 0 16px;")))
        )
      )
    )
  ),
  # RDA plot 
  nav_panel(
    title = "RDA",
    fluidRow(
      column(
        width = 8, offset = 2,
        
        accordion(
          open = "RDA",
          accordion_panel(
            title = "RDA",
            plotlyOutput("RDA_plot"),
            selectInput("dropdown_RDA", "Select explanatory variable",
                        choices = c("All", "WT", "Day", "GT", "KO", "Day * GT",
                                    "Day 7 and Day 90")),
            fluidRow(
              column(9, textInput("filter", "Filter days",
                                  placeholder = "'Day 56 and Day 90' or 
                                  'Day 7'")),
              column(3, tags$div(style = "margin-top: 25px;",
                                 actionButton("filter_submit", "Filter",
                                              style = "height: 38px; 
                                              width: 100%;
                                            line-height: 38px; padding:
                                              0 16px;")))
            ),
            selectInput("dropdown_genotype", "Select genotype",
                        choices = c("Both", "WT", "KO"))
          )
        )
      )
    )
  ),
  
  # Heatmap and boxplot
  nav_panel(
    title = "Beta diversity heatmap and boxplot",
    fluidRow(
      column(width = 8, offset = 2,
             accordion(open = "panel 3",
                       # heatmap. 
                       accordion_panel("panel 3", title = "Beta diversity",
                                       fileInput("file2",
                                                 "Choose Beta diversity file"),
                                       p("Please load abundance file first"),
                                       plotlyOutput("heatmap", 
                                                    height = "600px")),
                       # boxplot
                       accordion_panel("panel 4", title = "boxplot",
                                       selectInput("dropdown", "Select day", 
                                                   choices = c("")),
                                       girafeOutput("boxplot",height = "600px",
                                                    width = "80%"))
        )    
      )          
    )         
  ),

  nav_spacer()
)     

# server function
server <- function(input, output, session) {
  
  # composition plots
  
  # Average button is first set to false. 
  average1 <- reactiveVal(FALSE)
  average2 <- reactiveVal(FALSE)
  
  # when clicked button is clicked, average button is true. 
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
    req(metadata_df1())
    make_species_plot(metadata_df1(), average1())
  })
  
  species_plot_obj2 <- reactive({
    req(metadata_df2())
    make_species_plot(metadata_df2(), average2())
  })
  
  # The composition plot is shown in the app. 
  output$species_plot1 <- renderGirafe({
    species_plot_obj1()
  })
  
  output$species_plot2 <- renderGirafe({
    species_plot_obj2()
  })
  

  # PCA plot 

  filtered_pca_df <- reactive({
    df <- metadata_df1()
    
    # Apply genotype filter
    if (input$dropdown_PCA != "All") {
      df <- df[df$GT == input$dropdown_PCA, ]
    }
    
    # Apply day filter
    filter_text <- trimws(input$filter_pca)
    if (filter_text != "") {
      days <- trimws(strsplit(filter_text, "\\s+and\\s+", perl = TRUE)[[1]])
      day_nums <- as.numeric(gsub("[^0-9]", "", days))
      df <- df[df$Day %in% day_nums, ]
    }
    
    return(df)
    # only runs when the button is clicked and at first load of the app
  }) |> bindEvent(input$filter_pca_submit, ignoreNULL = FALSE)
  
  
  # show PCA plot in app
  output$PCA_plot <- renderPlotly({
    req(metadata_df1())
    makepca(filtered_pca_df(), input$dropdown_PCA)
  })
  

  # RDA plot 
  filtered_df <- reactive({
    df <- metadata_df1()
    
    filter_text <- trimws(input$filter)
    filter_genotype <- trimws(input$dropdown_genotype)
    
    # Apply genotype filter
    if (filter_genotype != "Both") {
      df <- df[df$GT %in% filter_genotype, ]
    }
    
    # Apply day filter
    if (filter_text != "") {
      days <- trimws(strsplit(filter_text, "\\s+and\\s+", perl = TRUE)[[1]])
      day_nums <- as.numeric(gsub("[^0-9]", "", days))
      df <- df[df$Day %in% day_nums, ]
    }
    
    return(df)
  }) |> bindEvent(input$filter_submit, ignoreNULL = FALSE)
  
  
  # show RDA plot in the app
  output$RDA_plot <- renderPlotly({
    req(metadata_df1())
    makeRDA(filtered_df(), input$dropdown_RDA)
  })
  
  #------------------------
  # Heatmap beta diversity
  #------------------------
  
  # load Bray curtis file
  df_metadata_heatmap <- eventReactive(input$file2, {
    load_sep_file(input$file2$datapath)
  })
  
  # data preparing for heatmap and boxplot
  prepped <- reactive({
    req(df_metadata_heatmap(), metadata_df1())
    prepare_data(df_metadata_heatmap(), metadata_df1())
  })
  
  # heatmap output
  output$heatmap <- renderPlotly({
    req(prepped())
    make_heatmap(prepped())
  })
  
  # boxplot data
  boxplot_data <- reactive({
    req(prepped())
    make_boxplot(prepped())
  })
  
  # days from the data is used for the input of the dropdown menu. 
  observeEvent(boxplot_data(), {
    days <- as.character(boxplot_data()$days)
    choices <- c("All days" = "all", setNames(days, days))
    updateSelectInput(session, "dropdown", choices = choices, selected = "all")
  })
  
  # default is all, otherwise the selected input from the dropdown menu. 
  selected_boxplot_data <- reactive({
    req(prepped())
    day_filter <- if (input$dropdown == "all") NULL else input$dropdown
    make_boxplot(prepped(), day_filter)
  })
  
  # show boxplot. 
  output$boxplot <- renderGirafe({
    selected_boxplot_data()$plot
  })
}   

shinyApp(ui, server)
