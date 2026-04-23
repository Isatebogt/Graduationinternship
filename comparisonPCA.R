
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(plotly)

# function: Make PCA plot. 

makepca <- function(metadata_df, formula_choice = "All"){
  
  
  
  # Log-transform relative abundance, then pivot to wide format (samples as rows)
  df <- metadata_df %>%
    mutate(percentage = log(percentage * 100 + 1, base = 10)) %>%
    pivot_wider(names_from = classid, values_from = percentage, values_fill = 0) %>%
    tibble::column_to_rownames(var = "sample")
  
  df <- na.omit(df)
  
  # optionally subset to genotype
  if (formula_choice != "All") {
    if (grepl("AND", formula_choice)) {
      parts <- trimws(strsplit(formula_choice, "AND")[[1]])
      print(parts)
      for (part in parts) {
        if (part %in% df$GT) {
          df <- df[df$GT == part, ]
        } else {
          day_num <- as.numeric(gsub("[^0-9]", "", part))
          df <- df[df$Day == day_num, ]
        }
      }
    } else if (formula_choice %in% df$GT) {
      df <- df[df$GT == formula_choice, ]
    } else {
      formula_choice <- as.numeric(gsub("[^0-9]", "", formula_choice))
      df <- df[df$Day == formula_choice, ]
    }
  }
  
  
  # make sure to get the metadata
  meta_cols <- intersect(c("GT", "Day", "source"), names(df))
  metadata <- df[, meta_cols, drop = FALSE]
  
  # run PCA
  numeric_cols <- sapply(df, is.numeric)
  pca_result <- vegan::rda(df[, numeric_cols])
  pca_summary <- summary(pca_result)
  PCA1_perc=pca_summary$cont$importance[3,1]*100
  PCA2_perc=(pca_summary$cont$importance[3,2]-pca_summary$cont$importance[3,1])*100
  scores <- scores(pca_result, display = "all")  
  print(pca_result)
  print(scores)
  
  
  df1 <- data.frame(scores(pca_result, display = "sites", choices = 1:2))
  df1 <- tibble::rownames_to_column(df1, var = "sample")  
  colnames(df1)<-c("sample","PCA1","PCA2")
  metadata <- tibble::rownames_to_column(metadata, var = "sample")
  df2 <- metadata%>%left_join(df1, by="sample") %>% filter(PCA1!="NA")
  df2.1 <- df2%>%tibble::column_to_rownames(var="sample")
  
  
  df1 <- data.frame(scores(pca_result, display = "species", choices = 1:2))
  df1 <- rownames_to_column(df1)
  colnames(df1) <- c("Microbes","PCA1","PCA2")
  
  
  df3 <- df1 %>% arrange(desc(sqrt(PCA1^2+PCA2^2))) %>% slice(1:10)
  df3.1 <- df3%>%tibble::column_to_rownames(var="Microbes")
  
  
  df2$Day <- as.factor(df2$Day)
  
  # make the PCA biplot
  p <- ggplot(data=df2, aes(x = PCA1, y = PCA2)) + 
    geom_point(aes(fill = Day, shape = GT, color = source, 
                   text = paste("Sample:", sample,
                                "<br>Day:", Day,
                                "<br>PCA1:", round(PCA1, 2),
                                "<br>PCA2:", round(PCA2, 2),
                                "<br>Genotype:", GT)),
               size = 3) +
    geom_hline(yintercept=0, linetype="dotted") +
    geom_vline(xintercept=0, linetype="dotted") +
    expand_limits(x = -3.5) + 
    expand_limits(x = 3.5) + 
    expand_limits(y = -3.5) + 
    expand_limits(y = 3.5) +   
    labs(x=paste("PCA1 (",sprintf("%.2f",PCA1_perc),"%)",sep=""),
         y=paste("PCA2 (",sprintf("%.2f",PCA2_perc),"%)", sep=""),
         title="PCA biplot") +
    # vector
    geom_segment(data=df3.1, aes(x=0, xend=PCA1, y=0, yend=PCA2), 
                 color="black", arrow=arrow(length=unit(0.01,"npc"))) +
    geom_text(data=df3.1, 
              aes(x=PCA1,y=PCA2,label=rownames(df3.1),
                  hjust=0.5*(1-sign(PCA1)),vjust=0.5*(1-sign(PCA2))), 
              color="black", size=4) 
  
  
  return(ggplotly(p, tooltip = "text"))
  
}
