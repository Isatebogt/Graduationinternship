library(ggplot2)
library(dplyr)
library(ggiraph)

make_species_plot <- function(metadata_df){
  
  metadata_df <- metadata_df %>%
    mutate(
      sample_label = paste(Day, sample, sep = "_"),
      bar_id = paste(sample_label, classid, sep = "_")
    )
  
  metadata_df$Day <- factor(
    metadata_df$Day,
    levels = c("d7","d21","d56","d90")
  )
  
  p <- ggplot(metadata_df, aes(x = sample_label, y = percentage)) +
    geom_bar_interactive(
      aes(
        fill = classid,
        tooltip = paste0(
          "Sample: ", sample_label,
          "<br>Class: ", classid,
          "<br>Percentage: ", round(percentage,2)
        ),
        data_id = bar_id
      ),
      stat = "identity",
      hover_fill = "white"
    ) +
    facet_wrap(~ Day, scales = "free_x", nrow = 1) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
      title = "composition per sample",
      x = NULL,
      y = "Relative abundance (%)",
      fill = "Class ID"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(size = 12),
      legend.position = "right"
    )
  
  girafe(
    ggobj = p,
    width_svg = 12,
    height_svg = 4,
    options = list(
      opts_hover(css = "fill-opacity:0.6;stroke:grey;stroke-width:0.5px;cursor:pointer;")
    )
  )
  
}