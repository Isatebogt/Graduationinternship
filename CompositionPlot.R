library(ggplot2)
library(dplyr)
library(ggiraph)
library(ggh4x)

# function: make composition plots. 

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
    
    
    p <- ggplot(metadata_df, aes(x = GT, y = avg_percentage)) +
      geom_bar_interactive(
        aes(
          fill = classid,
          tooltip = paste0(
            "<br>Class: ", classid,
            "<br>Percentage: ", round(avg_percentage, 2)
          ),
          # unique id for hovering. 
          data_id = paste(GT, Day, classid, sep = "_")
        ),
        stat = "identity",
        hover_fill = "white"
      ) +
      # outer strip = day, inner strip = GT (for layout of the plot)
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
      labs(
        title = "Composition per sample",
        x = NULL,
        y = "Relative abundance (%)",
        fill = "Class ID"
      ) +
      theme(
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.clip = "off",
        axis.line = element_line(),
        panel.grid.major.y = element_line(),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(), # hiding sample labels
        axis.ticks.x = element_blank(),
        legend.position = "right"
      )
    
    
    
  } else {
    
    p <- ggplot(metadata_df, aes(x = sample, y = percentage)) +
      geom_bar_interactive(
        aes(
          fill = classid,
          tooltip = paste0(
            "Sample: ", sample_label,
            "<br>Class: ", classid,
            "<br>Percentage: ", round(percentage, 2)
          ),
          data_id = bar_id
        ),
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
      labs(
        title = "Composition per sample",
        x = NULL,
        y = "Relative abundance (%)",
        fill = "Class ID"
      ) +
      theme(
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.clip = "off",
        axis.line = element_line(),
        panel.grid.major.y = element_line(),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "right"
      )
  }
  
  # ggplot is girafe object for interactivity. 
  
  g <- girafe(
    ggobj = p,
    width_svg = 12,
    height_svg = 4,
    options = list(
      opts_hover(css = "fill-opacity:0.6;stroke:grey;stroke-width:0.5px;cursor:pointer;")
    )
  )
  
  return(g)
  
}

