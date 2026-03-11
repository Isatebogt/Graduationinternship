# ===========================
# Libraries
# ===========================
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(viridis)
library(readxl)
library(ggiraph)
library(tidyverse)
library(htmltools)

# ===========================
# Load data
# ===========================
abundance_df <- read.table(
  "C:/Users/isate/OneDrive/Documenten/STAGEwur/HMI/Rproject/App-1/Bioinformaticsresults/bioinformatics_results/bioinformatics_results/biotaviz_clean_absolute.txt",
  header = TRUE,
  sep = "\t",
  comment.char = ""
)

dataframe_splitup <- read_excel(
  "C:/Users/isate/OneDrive/Documenten/STAGEwur/HMI/Rproject/App-1/Bioinformaticsresults/bioinformatics_results/bioinformatics_results/for_canoco.xlsx",
  sheet = 4
)

# ===========================
# Output folder
# ===========================
outputdir <- "C:/Users/isate/OneDrive/Documenten/STAGEwur/HMI/Rproject/App-1/outputdir/"
dir.create(outputdir, showWarnings = FALSE)

# ===========================
# Make numeric useful values
# ===========================
abundance_df <- abundance_df %>%
  mutate(across(
    starts_with("RemovePrimerFinal"),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  ))

# ===========================
# Detect taxonomic levels
# ===========================
taxonomy_column <- abundance_df[[2]]

taxa_levels <- unique(
  str_extract(taxonomy_column, "^(domain|kingdom|phylum|class|order|family|genus|species)")
)
taxa_levels <- taxa_levels[!is.na(taxa_levels)]

print(taxa_levels)

# ===========================
# Function to process each taxonomic level
# ===========================
process_taxa <- function(df, taxon) {
  message("Processing: ", taxon)
  
  # Filter df to keep rows where the second column starts with the respective taxonomic rank
  df_taxon <- df %>% filter(str_detect(.[[2]], paste0("^", taxon)))
  view(df_taxon)
  
  # Pattern to extract the name of the bacteria for the specific taxonomic level
  pattern <- paste0("(?<=", taxon, "\\s-\\s)[\\w-]+")
  
  # Extract the name of the bacteria in the specific taxonomic level
  df_taxon <- df_taxon %>% mutate(Taxon = str_extract(.[[2]], pattern))
  view(df_taxon)
  
  # Reshape dataframe from wide to long format
  dt <- df_taxon %>%
    pivot_longer(
      cols = where(is.numeric),
      names_to = "Samples",
      values_to = "Expression"
    )
  
  # Calculate percentages per sample
  dt <- dt %>%
    group_by(Samples) %>%
    mutate(Percent = Expression / sum(Expression, na.rm = TRUE) * 100) %>%
    ungroup()
  
  # Find top 10 most abundant taxa
  top10 <- dt %>%
    group_by(Taxon) %>%
    summarise(Total = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Total)) %>%
    slice_head(n = 10) %>%
    pull(Taxon)
  
  # Rename all others to "Other"
  dt <- dt %>%
    mutate(Taxon = ifelse(Taxon %in% top10, Taxon, "Other")) %>%
    group_by(Samples, Taxon) %>%
    summarise(Percent = sum(Percent), .groups = "drop")
  
  # Save table to CSV
  csv_file <- file.path(outputdir, paste0(taxon, "_table.csv"))
  write.csv(dt, csv_file, row.names = FALSE)
  
  # Plot
  p <- ggplot(dt, aes(
    x = Samples,
    y = Percent,
    fill = Taxon,
    tooltip = paste0(Taxon, ": ", round(Percent, 2), "%"),
    data_id = paste(Samples, Taxon, sep = "_")
  )) +
    geom_bar_interactive(stat = "identity", hover_fill = "white") +
    labs(
      y = "Relative abundance (%)",
      x = "Sample",
      fill = taxon
    ) +
    scale_fill_viridis_d(option = "turbo") +
    theme(
      axis.text.x = element_text(angle = 70, hjust = 0.5, size = 4),
      legend.position = "bottom"
    )
  
  # Save interactive HTML plot
  interactive_plot <- girafe(
    ggobj = p,
    width_svg = 8,
    height_svg = 4,
    options = list(
      opts_hover(css = "fill:white;stroke:grey;stroke-width:0.5px;cursor:pointer;")
    )
  )
  
  htmltools::save_html(
    interactive_plot,
    paste0(outputdir, "/plots_", taxon, ".html")
  )
  
  # Save static JPG plot
  jpg_file <- file.path(outputdir, paste0(taxon, "_plot.jpg"))
  ggsave(
    jpg_file,
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# ===========================
# Loop over taxonomic levels
# ===========================
for(taxon in taxa_levels) {
  process_taxa(abundance_df, taxon)
}