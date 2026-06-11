
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(plotly)
library(svglite)

# function: Make  an interactive RDA plot of microbiota data. 

makeRDA <- function(df, formula_choice = "Day + GT", statistic_value = "") {
  
  # Remove all NA values
  df <- na.omit(df)
  
  # Determine RDA formula and optionally subset data based on formula_choice
  if (formula_choice == "All") {
    formula_choice <- "Day + GT"
  } else if (formula_choice == "KO") {
    df <- df[df$GT == "KO", ]
    formula_choice <- "Day"
  } else if (formula_choice == "WT") {
    df <- df[df$GT == "WT", ]
    formula_choice <- "Day"
  } else if (formula_choice == "Day * GT") {
    formula_choice <- "Day * GT"
  } else if (formula_choice == "Day") {
    formula_choice <- "Day"
  } else if (formula_choice == "GT") {
    formula_choice <- "GT"
  } 
  
  # Log-transform data and reshape to wide format
  df <- df %>%
    mutate(
      percentage = log(percentage * 100 + 1, base = 10),
      GT = factor(GT)
    ) %>%
    pivot_wider(names_from = classid, values_from = percentage, 
                values_fill = 0) %>%
    tibble::column_to_rownames(var = "sample")
  
  # Seperate metadata from the taxa abundance columns
  metadata <- df[, 1:2]
  response_cols <- df %>% select(-Day, -GT)
  
  # Run RDA with chosen explanatory variables
  rda_formula <- as.formula(paste("response_cols ~", formula_choice))
  RDA_result <- vegan::rda(rda_formula, data = df)
  
  # Run permutation-based ANOVA tests to assess model significance: 
  # - Overall model significance
  anova <- vegan::anova.cca(RDA_result)
  # print (anova)

  # Extract the proportion variance explained by RDA1 and RDA2
  RDA_summary <- summary(RDA_result)
  RDA1_perc <- RDA_summary$cont$importance[2, 1] * 100
  RDA2_perc <- RDA_summary$cont$importance[2, 2] * 100
  
  # Extract sample position scores for RDA1 and RDA2
  df1 <- data.frame(scores(RDA_result, display = "sites", choices = 1:2))
  df1 <- tibble::rownames_to_column(df1, var = "sample")
  colnames(df1) <- c("sample", "RDA1", "RDA2")
  
  # Join RDA scores with metadta 
  metadata <- tibble::rownames_to_column(metadata, var = "sample")
  df2 <- metadata %>% left_join(df1, by = "sample") %>% filter(!is.na(RDA1))
  
  # Extract vector loading for RDA1 and RDA2
  df1 <- data.frame(scores(RDA_result, display = "species", choices = 1:2))
  df1 <- rownames_to_column(df1)
  colnames(df1) <- c("Microbe", "RDA1", "RDA2")

  
  # Flip RDA1 if KO samples have a negative mean score, so KO is consistently
  # on the right side of the plot for easier visual comparison across plots.
  ko_mean_rda1 <- mean(df2$RDA1[df2$GT == "KO"], na.rm = TRUE)
  if (!is.na(ko_mean_rda1) && ko_mean_rda1 < 0) {
    df2$RDA1 <- -df2$RDA1   
    df1$RDA1 <- -df1$RDA1   
  }
  
  # Additionally flip RDA1 if the latest time point has a negative mean score,
  # so later time points are consistently on the right
  reference_day <- max(df2$Day, na.rm = TRUE)
  mean_ref_rda1 <- mean(df2$RDA1[df2$Day == reference_day], na.rm = TRUE)
  if (!is.na(mean_ref_rda1) && mean_ref_rda1 < 0) {
    df2$RDA1 <- -df2$RDA1
    df1$RDA1 <- -df1$RDA1
  }
  
  # Rank all vector loadings and keep the top 10 highest vector loadings. 
  respvariable <- df1 %>% arrange(desc(sqrt(RDA1^2 + RDA2^2)))
  df3 <- df1 %>% arrange(desc(sqrt(RDA1^2 + RDA2^2))) %>% slice(1:10)
  df3.1 <- df3 %>% tibble::column_to_rownames(var = "Microbe")

  # Convert Day to factor so ggplot can use it for colors. 
  df2$Day <- as.factor(df2$Day)

  # Build RDA plot
  p <- ggplot(data = df2, aes(x = RDA1, y = RDA2)) +
    geom_point(aes(shape = GT, fill = Day,
                   text = paste("Sample:", sample,
                                "<br>Day:", Day,
                                "<br>RDA1:", round(RDA1, 2),
                                "<br>RDA2:", round(RDA2, 2),
                                "<br>Genotype:", GT)),
               size = 3,
               colour = "black",
               stroke = 0.4) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    expand_limits(x = c(-3.5, 3.5), y = c(-3.5, 3.5)) +
    labs(x = paste0("RDA1 (", sprintf("%.2f", RDA1_perc), "%)"),
         y = paste0("RDA2 (", sprintf("%.2f", RDA2_perc), "%)"),
         title = "RDA") +
    geom_segment(data = df3.1, aes(x = 0, xend = RDA1, y = 0, yend = RDA2),
                 colour = "black", arrow = arrow(length = unit(0.02, "npc"))) +
    geom_text(data = df3,
              aes(x = RDA1, y = RDA2, label = Microbe,
                  hjust = 0.5 * (1 - sign(RDA1)), vjust = 0.5 * 
                    (1 - sign(RDA2))),
              colour = "black", size = 4)

  # return interactive plotly RDA plot
  return(ggplotly(p, tooltip = "text"))
}
