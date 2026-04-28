library(ggpicrust2)
library(GGally)
library(KEGGREST)
library(tidyverse)
library(readxl)


metadata <- read_excel("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/inputdir/CXCL8a/for_canoco.xlsx", 
                       sheet = "metadata")

metadata <- metadata %>% rename(sample = "Sample-id")
metadata <- metadata %>%
  filter(GT != "MOCK") 


results_data_input <- ggpicrust2(
  file               = "C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/outputdir/cxcl8a/CXCL8aGalaxy146-[KO_pred_metagenome_unstrat].tsv",
  metadata           = metadata,
  group              = "GT",
  pathway            = "KO",
  daa_method         = "LinDA",
  ko_to_kegg         = TRUE,
  order              = "pathway_class",
  p_values_bar       = TRUE,
  x_lab              = "pathway_name",
  p.adjust    = "BH",
  p_values_threshold = 0.05
)


kegg_abundance <- ko2kegg_abundance("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/outputdir/cxcl8a/CXCL8aGalaxy146-[KO_pred_metagenome_unstrat].tsv")

example_plot    <- results_data_input[[1]]$plot
example_results <- results_data_input[[1]]$results
print(example_plot)


sig_features <- example_results %>%
  filter(p_adjust < 0.05) %>%
  pull(feature)

print(sig_features)


pca_plot <- pathway_pca(
  abundance = kegg_abundance,
  metadata  = metadata,
  group     = "GT"
)
print(pca_plot)

if (length(sig_features) > 0) {
  sig_abundance <- kegg_abundance %>%
    rownames_to_column("pathway") %>%
    filter(pathway %in% sig_features) %>%
    column_to_rownames("pathway")
  
  heatmap_plot <- pathway_heatmap(
    abundance = sig_abundance,
    metadata  = metadata,
    group     = "GT"
  )
  print(heatmap_plot)
} else {
  message("No significant features to plot in heatmap")
}

ggsave("errorbar_plot.png", plot = example_plot, width = 10, height = 6, dpi = 300)
ggsave("pca_plot.png",      plot = pca_plot,     width = 8,  height = 6, dpi = 300)
if (exists("heatmap_plot")) {
  ggsave("heatmap_plot.png", plot = heatmap_plot, width = 10, height = 6, dpi = 300)
}
