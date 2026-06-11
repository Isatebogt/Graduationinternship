library("heatmaply")
library(heatmaply)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(svglite)

# function: make Heat and corresponding boxplot for Bray-curtis values. 

# Prepare the data for the plotting. 
prepare_data <- function(df_metadata_heatmap, metadata_df){
  
  # Make sample id the rownames of the dataframe
  data_for_betadiversity <- df_metadata_heatmap |>
    dplyr::rename(sample_id = 1) |>
    tibble::column_to_rownames(var = "sample_id")
  
  # Convert to matrix
  mat <- as.matrix(data_for_betadiversity)
  
  # clean data from unwanted spaces and enters
  colnames(mat) <- sub("\\..*", "", colnames(mat))
  rownames(mat) <- sub("\\..*", "", rownames(mat))
  
  
  # From metadata only keep sample, day and GT
  sample_meta <- metadata_df %>%
    select(sample, Day, GT) %>%
    distinct(sample, .keep_all = TRUE) %>%
    tibble::column_to_rownames("sample")
  
  
  # Reorder sample_meta so its rows are in same order as the matrix
  sample_meta <- sample_meta[rownames(mat), , drop = FALSE]
  
  # makes day a factor for plotting
  sample_meta$Day <- as.factor(sample_meta$Day)
  
  # Remove samples with missing day values from both metadat and matrix
  row.names.remove <- rownames(subset(sample_meta, is.na(Day)))
  sample_meta <- sample_meta[!(rownames(sample_meta) %in% row.names.remove), ]
  mat <- mat[!(rownames(mat) %in% row.names.remove), ]
  mat <- mat[, !(colnames(mat) %in% row.names.remove)]
  
  # To use matrix and sample_meta for both functions. 
  list(mat = mat, sample_meta = sample_meta)
  
}

make_heatmap <- function(prepped) {
  
  # Extract prepared matrix and metadata
  mat         <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  # Creates a label for each sample combining both Day and GT
  tick_labels <- paste0("DAY: ",sample_meta$Day, " GT: ",sample_meta$GT)
  
  #  build interactive heatmap
  h <- heatmaply(
    mat,
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "yellow", high = "red", midpoint = 0.5
    ),
    limits        = c(0, 1),
    hclust_method = "average",
    
    # Annotate rows and columns with Day and GT color bars
    col_side_colors = sample_meta,
    row_side_colors = sample_meta,
    
    
    # hide tick labels
    showticklabels = c(F,F),
    
    main = "Bray-Curtis dissimilarity",
    
    # Custom hover text showing Day and GT for both samples in each square
    custom_hovertext = outer(
      tick_labels, tick_labels, 
      FUN = function(x, y) paste0("Row: ", x, "<br>Col: ", y)
    ),
    
    
  )
  
  return(h)
  
}

# function: make Bray-Curtis boxplot

make_boxplot <- function(prepped, day_filter = NULL) {
  
  mat <- prepped$mat
  sample_meta <- prepped$sample_meta
  
  # Add sample column to metadata 
  sample_meta <- tibble::rownames_to_column(sample_meta, var = "sample")
  
  # Melt the dissimilarity matrix from wide to long format 
  # Each row represents a pairwise dissimilarity between two samples.
  matrix_data <- melt(mat)
  
  # Join metadata for the first sample in each pair (Var1)
  newframe1 <- left_join(matrix_data, sample_meta %>% select(sample,
                                                             GT_var1 = GT, 
                                                             Day_var1 = Day),
                         by = c("Var1" = "sample"))
  
  # Join metadata for the second sample in each pair (Var2)
  newframe2 <- left_join(newframe1, sample_meta %>% select(sample,
                                                           GT_var2 = GT, 
                                                           Day_var2 = Day),
                         by = c("Var2" = "sample"))
  
  # Remove self comparisons 
  newframe2 <- filter(newframe2, as.character(Var1) != as.character(Var2))
  
  # Remove duplicate pairs
  newframe2 <- filter(newframe2, as.character(Var1) < as.character(Var2))
  
  # Get unique days for dropdown menu in the app
  Days_for_dropdown <- sort(unique(newframe2$Day_var2))
  
  # Create a genotype combination label
  newframe2$combined <- paste(
    pmin(newframe2$GT_var1, newframe2$GT_var2),
    "-",
    pmax(newframe2$GT_var1, newframe2$GT_var2)
  )
  
  #  Filter to a specific day and always exclude MOCK samples
  if (!is.null(day_filter)) {
    newframe2 <- filter(newframe2, GT_var1 != "MOCK", GT_var2 != "MOCK",
                        Day_var2 == day_filter)
  } else {
    newframe2 <- filter(newframe2, GT_var1 != "MOCK", GT_var2 != "MOCK")
  }
  
  # Remove between-group comparisons (KO vs WT) keeping only group pairs
  # Remove to include KO - WT
  newframe2 <- filter(newframe2, combined != "KO - WT")
  
  # Run pairwise Wilcoxon test with Benjamini-Hochberg correction for microbiota
  # to assess whetther dissimilarity differs between genotype groups. 
  groups <- split(newframe2$value, newframe2$combined)
  
  if (length(groups) >= 2) {
    pw_result <- pairwise.wilcox.test(
      x = newframe2$value,
      g = newframe2$combined,
      p.adjust.method = "BH"
    )
    print(pw_result)
  } else {
    message("Only one group present — skipping statistical test")
  }
  
  # Build interactive boxplot using ggplot, with tooltips showing stats
  p <- ggplot(newframe2) +
    geom_boxplot_interactive(aes(
      x       = combined,
      y       = value,
      fill    = combined,
      data_id = combined,
      tooltip = after_stat(
        paste0(
          "class: ",   .data$fill,
          "\nQ1: ",    prettyNum(.data$lower),
          "\nQ3: ",    prettyNum(.data$upper),
          "\nmedian: ", prettyNum(.data$middle)
        )
      )
    )) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25))
  
  # Return interactive plot, days for dropdown and stats in list
  return(list(
    plot  = girafe(ggobj = p),
    days  = Days_for_dropdown,
    stats = if (length(groups) >= 2) pw_result else NULL
  ))
}
