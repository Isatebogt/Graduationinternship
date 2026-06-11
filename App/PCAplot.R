
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(plotly)
library(svglite)

# function: Make an interactive PCA plot of microbiota data.

makepca <- function(metadata_df, formula_choice = "All"){
  
  
  
  # Log transform for more representative data and reshape to wide format
  df <- metadata_df %>%
    mutate(percentage = log(percentage * 100 + 1, base = 10)) %>%
    pivot_wider(names_from = classid, values_from = percentage, values_fill
                = 0) %>%
    tibble::column_to_rownames(var = "sample")
  
  
  # No NA values
  df <- na.omit(df)

  
  # Optionally subset to genotype
  if (formula_choice != "All") {
    if (formula_choice %in% df$GT) {
      df <- df[df$GT == formula_choice, ]
    } else {
      formula_choice <- as.numeric(gsub("[^0-9]", "", formula_choice))
      df <- df[df$Day == formula_choice, ]
    }
  }
  
  
  # Extract metadata [first 3 columns: sample, GT and Day]
  metadata = df[0:3]
  
  # run PCA on taxa columns only
  pca_result <- vegan::rda(df[, -c(1,2)]) 
  pca_summary <- summary(pca_result)
  
  # Calculate variance explained by PCA1 and PCA2 
  PCA1_perc=pca_summary$cont$importance[3,1]*100
  PCA2_perc=(pca_summary$cont$importance[3,2]-
               pca_summary$cont$importance[3,1])*100
  
  # Extract sample positions in PCA 
  scores <- scores(pca_result, display = "all")  
  
  
  df1 <- data.frame(scores(pca_result, display = "sites", choices = 1:2))
  df1 <- tibble::rownames_to_column(df1, var = "sample")  
  colnames(df1)<-c("sample","PCA1","PCA2")
  
  # PCA scores and Metadata are joined
  metadata <- tibble::rownames_to_column(metadata, var = "sample")
  df2 <- metadata%>%left_join(df1, by="sample") %>% filter(PCA1!="NA")
  df2.1 <- df2%>%tibble::column_to_rownames(var="sample")
  
  # Extract the vector scores 
  df1 <- data.frame(scores(pca_result, display = "species", choices = 1:2))
  df1 <- rownames_to_column(df1)
  colnames(df1) <- c("Microbes","PCA1","PCA2")
  
  
  # Keep the top 10 vectors with the highest loading, most contribution to the
  # PCA plot. 
  df3 <- df1 %>% arrange(desc(sqrt(PCA1^2+PCA2^2))) %>% slice(1:10)
  df3.1 <- df3%>%tibble::column_to_rownames(var="Microbes")
  
  
  # Day to factor so ggplot can attach colors per day. 
  df2$Day <- as.factor(df2$Day)
  
  # Make the PCA biplot
  p <- ggplot(data = df2, aes(x = PCA1, y = PCA2)) + 
    geom_point(
      aes(
        fill = Day,
        shape = GT,
        text = paste(
          "Sample:", sample,
          "<br>Day:", Day,
          "<br>PCA1:", round(PCA1, 2),
          "<br>PCA2:", round(PCA2, 2),
          "<br>Genotype:", GT
        )
      ),
      size = 3,
      colour = "black",
      stroke = 0.4
    ) +
    scale_shape_manual(values = c(21, 24, 23)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    expand_limits(x = c(-3.5, 3.5)) + 
    expand_limits(y = c(-3.5, 3.5)) +   
    labs(
      x = paste("PCA1 (", sprintf("%.2f", PCA1_perc), "%)", sep = ""),
      y = paste("PCA2 (", sprintf("%.2f", PCA2_perc), "%)", sep = ""),
      title = "PCA biplot"
    ) +
    geom_segment(
      data = df3.1,
      aes(x = 0, xend = PCA1, y = 0, yend = PCA2),
      colour = "black",
      arrow = arrow(length = unit(0.01, "npc"))
    ) +
    geom_text(
      data = df3.1,
      aes(
        x = PCA1,
        y = PCA2,
        label = rownames(df3.1),
        hjust = 0.5 * (1 - sign(PCA1)),
        vjust = 0.5 * (1 - sign(PCA2))
      ),
      colour = "black",
      size = 4
    )
  
  # Interactive plotly object
  return(ggplotly(p, tooltip = "text"))
  
}
