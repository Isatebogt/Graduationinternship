
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(plotly)

makeRDA <- function(metadata_df){
  
  
  df = metadata_df
  df <- na.omit(df)
  
  
  df[,3] <- log(df[,3]*100+1, base=10)
  
  wide_data <- df %>%
    pivot_wider(
      names_from = classid,
      values_from = percentage,
      values_fill = 0) %>%
    tibble::column_to_rownames(var = "sample")  
  
  
  metadata = wide_data[, 1:2]
  
  # if you know the column names
  response_cols <- wide_data %>% select(-Day, -GT)
  
  
  
  RDA_result <- vegan::rda(response_cols ~ Day + GT, data = wide_data)
  anova <- vegan::anova.cca(RDA_result)
  RDA_summary <- summary(RDA_result)
  print(RDA_summary$cont$importance)
  RDA1_perc <- RDA_summary$cont$importance[2, 1] * 100
  RDA2_perc <- RDA_summary$cont$importance[2, 2] * 100
  
  temp<-data.frame(Feature="RDA_1", anova, RDA1_perc=RDA1_perc, RDA2_perc=RDA2_perc)
  temp<-temp%>%tibble::rownames_to_column("Variable")
  anova_df <- bind_rows(as.data.frame(anova), temp)
  
  # print explained variation 
  cat("Explained variation RDA1 axis: ",RDA1_perc,"\n")
  cat("Explained variation RDA2 axis: ",RDA2_perc)
  
  
  df1 <- data.frame(scores(RDA_result, display = "sites", choices = 1:2))
  df1 <- tibble::rownames_to_column(df1, var = "sample")  
  colnames(df1)<-c("sample","RDA1","RDA2")
  metadata <- tibble::rownames_to_column(metadata, var = "sample")
  df2 <- metadata %>% left_join(df1, by = "sample") %>% filter(!is.na(RDA1))
  df2.1 <- df2%>%tibble::column_to_rownames(var="sample")
  
  
  df1 <- data.frame(scores(RDA_result, display = "species", choices = 1:2))
  df1 <- rownames_to_column(df1)
  colnames(df1) <- c("Genus","RDA1","RDA2")
  
  df3 <- df1 %>% arrange(desc(sqrt(RDA1^2+RDA2^2))) %>% slice(1:10)
  df3.1 <- df3%>%tibble::column_to_rownames(var="Genus")
  
  head(df2)
  head(df3.1)
  
  df2$Day <- as.factor(df2$Day)
  
  p <- ggplot(data=df2, aes(x = RDA1, y = RDA2)) + 
    geom_point(aes(shape=GT, color=Day, 
                   text = paste("Sample:", sample,
                                "<br>Day:", Day,
                                "<br>RDA1:", round(RDA1, 2),
                                "<br>RDA2:", round(RDA2, 2),
                                "<br>Genotype:", GT)),
               size = 3) +
    geom_hline(yintercept=0, linetype="dotted") +
    geom_vline(xintercept=0, linetype="dotted") +
    expand_limits(x = -3.5) + 
    expand_limits(x = 3.5) + 
    expand_limits(y = -3.5) + 
    expand_limits(y = 3.5) +   
    labs(x=paste("RDA1 (",sprintf("%.2f",RDA1_perc),"%)",sep=""),
         y=paste("RDA2 (",sprintf("%.2f",RDA2_perc),"%)", sep=""),
         title="RDA") +
    # vector
    geom_segment(data=df3.1, aes(x=0, xend=RDA1, y=0, yend=RDA2), 
                 color="black", arrow=arrow(length=unit(0.01,"npc"))) +
    geom_text(data=df3, 
              aes(x=RDA1,y=RDA2,label= Genus,
                  hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(RDA2))), 
              color="black", size=4) 
  
  
  return(ggplotly(p, tooltip = "text"))
  

}
  