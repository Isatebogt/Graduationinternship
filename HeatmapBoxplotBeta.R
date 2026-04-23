library("heatmaply")
library(heatmaply)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(ggiraph)

# function: make heatmap and corresponding boxplot

# Prepare the data for the plotting. 
prepare_data <- function(df_metadata_heatmap, metadata_df){

  # make sample id the rownames of the dataframe
  data_for_betadiversity <- df_metadata_heatmap |>
    dplyr::rename(sample_id = 1) |>
    tibble::column_to_rownames(var = "sample_id")
  
  
  mat <- as.matrix(data_for_betadiversity)
  
  # clean data from unwanted spaces and enters
  colnames(mat) <- sub("\\..*", "", colnames(mat))
  rownames(mat) <- sub("\\..*", "", rownames(mat))
  

  # from metadata only keep sample, day and GT
  sample_meta <- metadata_df %>%
    select(sample, Day, GT) %>%
    distinct(sample, .keep_all = TRUE) %>%
    tibble::column_to_rownames("sample")
  
  
  # Reorder sample_meta so its rows are in same order as the matrix.
  sample_meta <- sample_meta[rownames(mat), , drop = FALSE]
  
  # makes day a factor
  sample_meta$Day <- as.factor(sample_meta$Day)

  
  row.names.remove <- rownames(subset(sample_meta, is.na(Day)))
  
  sample_meta <- sample_meta[!(rownames(sample_meta) %in% row.names.remove), ]
  mat <- mat[!(rownames(mat) %in% row.names.remove), ]
  mat <- mat[, !(colnames(mat) %in% row.names.remove)]
 
  # To use matrix and sample_meta for both functions. 
  list(mat = mat, sample_meta = sample_meta)
  
}

# Makes bray curtis heatmap

make_heatmap <- function(prepped) {
  
  # prepared data
  mat         <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  View(mat)
  View(sample_meta)
  
  # Creates a label for each sample combining both Day and GT
  tick_labels <- paste0("DAY: ",sample_meta$Day, " GT: ",sample_meta$GT)
  
  # Heatmap
  h <- heatmaply(
    mat,
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "yellow", high = "red", midpoint = 0.5
    ),
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
make_boxplot <- function(prepped, day_filter = NULL) {  # NULL default, not None
  
  mat <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  sample_meta <- tibble::rownames_to_column(sample_meta, var = "sample")
  
  # Make long data format of matrix 
  test <- melt(mat)
  
  # Match Var1 from matrix against sample metadata
  newframe1 <- left_join(test, sample_meta %>% select(sample, GT_var1 = GT, Day_var1 = Day), 
                         by = c("Var1" = "sample"))
  
  # Match Var2 from matrix to sample, bring in GT as GT_var2
  newframe2 <- left_join(newframe1, sample_meta %>% select(sample, GT_var2 = GT, Day_var2 = Day), 
                         by = c("Var2" = "sample"))
  

  newframe2 <- filter(newframe2, as.character(Var1) != as.character(Var2))
  
  
  newframe2 <- filter(newframe2, as.character(Var1) < as.character(Var2))
  
  View(newframe2)
  
  Days_for_dropdown <- sort(unique(newframe2$Day_var2))

  
  # Combine the GT values 
  newframe2$combined <- paste(
    pmin(newframe2$GT_var1, newframe2$GT_var2),
    "-",
    pmax(newframe2$GT_var1, newframe2$GT_var2)
  )
  
  
  # Filter out MOCK; apply day filter only if day_data is provided
  if (!is.null(day_filter)) {
    newframe2 <- filter(newframe2, GT_var1 != "MOCK", GT_var2 != "MOCK", Day_var2 == day_filter)
  } else {
    newframe2 <- filter(newframe2, GT_var1 != "MOCK", GT_var2 != "MOCK")
  }
  
  p <- ggplot(newframe2) + 
    geom_boxplot_interactive(aes(
      x = combined,
      y = value,
      fill = combined,
      data_id = combined,
      tooltip = after_stat(
        paste0(
          "class: ", .data$fill,
          "\nQ1: ",    prettyNum(.data$lower),
          "\nQ3: ",    prettyNum(.data$upper),
          "\nmedian: ", prettyNum(.data$middle)
        )
      )
    ))
  
  return(list(
    plot = girafe(ggobj = p),
    days = Days_for_dropdown
  ))
}
