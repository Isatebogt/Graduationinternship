library(ggplot2)
library(dplyr)
library(ggiraph)
library(ggh4x)

make_species_plot <- function(metadata_df){
  
  metadata_df <- metadata_df %>%
    rename(any_of(c(Day = "Age", Genotype = "GenotypeIL22")))
  
  metadata_df <- metadata_df %>%
    mutate(
      sample_label = paste(Day, sample, sep = "_"),
      bar_id = paste(sample_label, classid, sep = "_"),
    )
  
  metadata_df$Day <- factor(metadata_df$Day)
  metadata_df$GT <- factor(metadata_df$GT)
  
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
        background_x = list(
          element_rect(fill = "lightgray", color = NA)
        ),
        text_x = list(
          element_text(size = 10)                  
        )
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
      strip.clip = "off",                          # ← add this
      axis.line = element_line(),
      panel.grid.major.y = element_line(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.ticks.x = element_blank(),
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