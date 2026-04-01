library("heatmaply")
library(heatmaply)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)


# Prepare the data for the plotting. 
prepare_data <- function(df_metadata_heatmap, metadata_df){

  # make sample id the rownames of the dataframe
  data_for_betadiversity <- df_metadata_heatmap |>
    dplyr::rename(sample_id = 1) |>
    tibble::column_to_rownames(var = "sample_id")
  
  mat <- as.matrix(data_for_betadiversity)
  
  # Clean data from unwanted spaces and enters
  colnames(mat) <- sub("\\..*", "", colnames(mat))
  rownames(mat) <- sub("\\..*", "", rownames(mat))

  # From metadata only keep sample, day and GT
  sample_meta <- metadata_df %>%
    select(sample, Day, GT) %>%
    distinct(sample, .keep_all = TRUE) %>%
    tibble::column_to_rownames("sample")
  
  # Reorders sample_meta so its rows are in the exact same order as the matrix.
  sample_meta <- sample_meta[rownames(mat), , drop = FALSE]
  
  # Makes day a factor
  sample_meta$Day <- as.factor(sample_meta$Day)
 
  # To use matrix and sample_meta for both functions. 
  list(mat = mat, sample_meta = sample_meta)
  
}

# Makes bray curtis heatmap

make_heatmap <- function(prepped) {
  
  # prepared data
  mat         <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  # Creates a label for each sample combining both Day and GT
  tick_labels <- paste0("DAY: ",sample_meta$Day, " GT: ",sample_meta$GT)
  
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

# Makes boxplot of bray curtis dissimilarity grouped on GT. 
make_boxplot <- function(prepped) {
  
  mat <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  sample_meta <- tibble::rownames_to_column(sample_meta, var="sample")
  
  # Make long data format of matrix 
  test <- melt(mat)
  
  
  # Match Var 1 from matrix against sample metadata
  newframe1 <- left_join(test, sample_meta %>% select(sample, GT_var1 = GT), 
                         by = c("Var1" = "sample"))
  
  # Match Var2 from matrix to sample, bring in GT as GT_var2
  newframe2 <- left_join(newframe1, sample_meta %>% select(sample, GT_var2 = GT), 
                         by = c("Var2" = "sample"))
  
  # combine the GT values 
  newframe2$combined <- paste(newframe2$GT_var1, "-", newframe2$GT_var2)
  
  # leave mock out. 
  newframe2 <- filter(newframe2, GT_var1 != "MOCK", GT_var2 != "MOCK")
  
  
  p <- ggplot(newframe2, aes(x=combined, y=value)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)
  
  
  girafe(ggobj = p)
  
  
}