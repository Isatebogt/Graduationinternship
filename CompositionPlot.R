library(ggplot2)
library(dplyr)
library(ggiraph)
library(ggh4x)

# function: make abundance plots

make_species_plot <- function(metadata_df, average){
  
  
  # creates unique sample and bar label to use for hovering.
  metadata_df <- metadata_df %>%
    mutate(
      sample_label = paste(Day, sample, sep = "_"),
      bar_id = paste(sample_label, classid, sep = "_"),
    )
  
  # makes sure that the days are in specific order and GT becomes factor for
  # formatting. 
  metadata_df$Day <- factor(metadata_df$Day, levels = 
                              sort(unique(metadata_df$Day)))
  metadata_df$GT  <- factor(metadata_df$GT)
  

  
  # creates mean of the sample values per genotype. 
  if (average) {
    metadata_df <- metadata_df %>%
      group_by(GT, Day, classid) %>%
      summarise(avg_percentage = mean(percentage), .groups = "drop")
    
    x_var    <- "GT"
    y_var    <- "avg_percentage"
    tooltip  <- paste0("<br>Class: ", metadata_df$classid,
                       "<br>Percentage: ", round(metadata_df$avg_percentage, 2))
    data_id  <- paste(metadata_df$GT, metadata_df$Day, metadata_df$classid, 
                      sep = "_")
    
  # does not create mean of the sample values
  } else {
    x_var    <- "sample"
    y_var    <- "percentage"
    tooltip  <- paste0("Sample: ", metadata_df$sample_label,
                       "<br>Class: ", metadata_df$classid,
                       "<br>Percentage: ", round(metadata_df$percentage, 2))
    data_id  <- metadata_df$bar_id
  }
  
  # Only show legend if its file has 10 taxa to show, otherwise hovering can 
  # give more information
  n_taxa <- length(unique(metadata_df$classid))
  # make the abundance plot
  p <- ggplot(metadata_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar_interactive(
      aes(fill = classid, tooltip = tooltip, data_id = data_id),
      stat = "identity",
      hover_fill = "white"
    ) +
    facet_nested(
      ~ Day + GT,
      scales = "free_x",
      space = "free_x",
      strip = strip_nested(
        background_x = list(element_rect(fill = "lightgray", color = NA)),
        text_x = list(element_text(size = 10))
      )
    ) +
    scale_fill_viridis_d(option = "turbo") +
    labs(title = "Composition per sample", x = NULL,
         y = "Relative abundance (%)", fill = "Class ID") +
    theme(
      panel.spacing      = unit(0, "lines"),
      strip.placement    = "outside",
      strip.background   = element_blank(),
      strip.clip         = "off",
      axis.line          = element_line(),
      panel.grid.major.y = element_line(),
      panel.background   = element_rect(fill = "white", color = NA),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      legend.position = if (n_taxa > 10) "none" else "right"
    )
  
  
  # ggplot is girafe object for interactivity. 
  g <- girafe(
    ggobj = p,
    width_svg = 12,
    height_svg = 4,
    options = list(
      opts_hover(css = "fill-opacity:0.6;stroke:grey;stroke-width:0.5px;
                 cursor:pointer;")
    )
  )
  
  return(g)
  
}

