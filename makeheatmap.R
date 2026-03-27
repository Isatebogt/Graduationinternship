library("heatmaply")
library(heatmaply)
library(RColorBrewer)
library(dplyr)
library(tibble)

#data_for_betadiversity <- read.delim("C:/Users/isate/Downloads/bray_curtis_distance_matrix.qza.txt", sep = '\t')
#metadata <- read.csv("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/outputdir/cxcl8a/family_table.csv")


make_heatmap <- function(df_metadata_heatmap, metadata_df){

  data_for_betadiversity <- df_metadata_heatmap |>
    dplyr::rename(sample_id = 1) |>
    tibble::column_to_rownames(var = "sample_id")
  

  
  mat <- as.matrix(data_for_betadiversity)
  

  colnames(mat) <- sub("\\..*", "", colnames(mat))
  rownames(mat) <- sub("\\..*", "", rownames(mat))


  sample_meta <- metadata_df %>%
    select(sample, Day, GT) %>%
    distinct(sample, .keep_all = TRUE) %>%
    tibble::column_to_rownames("sample")
  
  View(sample_meta)
  View(mat)
  
  sample_meta <- sample_meta[rownames(mat), , drop = FALSE]
  
  sample_meta$Day <- as.factor(sample_meta$Day)
  
  # Create combined labels for ticks: Day + GT
  tick_labels <- paste0("DAY: ",sample_meta$Day, " GT: ",sample_meta$GT)
  
  View(sample_meta)
  view(mat)
  
  # Heatmap
  h <- heatmaply(
    mat,
    colors        = rev(brewer.pal(9, "YlOrRd")),
    limits        = c(0, 1),
    hclust_method = "average",
 
    col_side_colors = sample_meta,
    row_side_colors = sample_meta,
    
  
    
    showticklabels = c(F,F),
    
    main = "Bray-Curtis dissimilarity",
    
    custom_hovertext = outer(
      tick_labels, tick_labels, 
      FUN = function(x, y) paste0("Row: ", x, "<br>Col: ", y)
    ),
    
   
  )
  
  return(h)

}