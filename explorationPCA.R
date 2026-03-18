
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)


dataframe = "C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/outputdir/cxcl8a/phylum_table.csv"

df = read.csv(dataframe)



df[,3] <- log(df[,3]*100+1, base=10)

wide_data <- df %>%
  pivot_wider(
    names_from = classid,
    values_from = percentage,
    values_fill = 0) %>%
      tibble::column_to_rownames(var = "sample")  # ← actual sample names become rownames
  

metadata = wide_data[0:3]

pca_result <- vegan::rda(wide_data[, -c(1,2)]) 
pca_summary <- summary(pca_result)
PCA1_perc=pca_summary$cont$importance[3,1]*100
PCA2_perc=(pca_summary$cont$importance[3,2]-pca_summary$cont$importance[3,1])*100

cat("Explained variation PCA1 axis: ",PCA1_perc,"\n")
cat("Explained variation PCA2 axis: ",PCA2_perc)

# collect data: cases (sample symbols)
df1 <- data.frame(scores(pca_result, display = "sites", choices = 1:2))
df1 <- tibble::rownames_to_column(df1, var = "sample")  
colnames(df1)<-c("sample","PCA1","PCA2")
metadata <- tibble::rownames_to_column(metadata, var = "sample")
df2 <- metadata%>%left_join(df1, by="sample") %>% filter(PCA1!="NA")
df2.1 <- df2%>%tibble::column_to_rownames(var="sample")


# collect data: response variable responsible for the separation (genus arrows)
df1 <- data.frame(scores(pca_result, display = "species", choices = 1:2))
df1 <- rownames_to_column(df1)
colnames(df1) <- c("Genus","PCA1","PCA2")

## the ten longest arrows
df3 <- df1 %>% arrange(desc(sqrt(PCA1^2+PCA2^2))) %>% slice(1:10)
df3.1 <- df3%>%tibble::column_to_rownames(var="Genus")

# print a summary
head(df2)
head(df3.1)


ggplot(data=df2, aes(x = PCA1, y = PCA2)) + 
  geom_point(aes(fill=Day), shape=21, size = 3) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  expand_limits(x = -1.5) + 
  expand_limits(x = 1.5) + 
  expand_limits(y = -1.5) + 
  expand_limits(y = 1) +   
  labs(x=paste("PCA1 (",sprintf("%.2f",PCA1_perc),"%)",sep=""),
       y=paste("PCA2 (",sprintf("%.2f",PCA2_perc),"%)", sep=""),
       title="PCA biplot") +
  geom_segment(data=df3.1, aes(x=0, xend=PCA1, y=0, yend=PCA2), 
               color="black", arrow=arrow(length=unit(0.01,"npc"))) +
  # arrow labels
  geom_text(data=df3.1, 
            aes(x=PCA1,y=PCA2,label=rownames(df3.1),
                hjust=0.5*(1-sign(PCA1)),vjust=0.5*(1-sign(PCA2))), 
            color="black", size=4) +
  # symbol labels
  #geom_text(data=df1.1, 
  #          aes(x=PCA1,y=PCA2,label=rownames(df1.1),
  #              hjust=0.5*(1-sign(PCA1)),vjust=0.5*(1-sign(PCA2))), 
  #          color="black", size=4) +  
  theme(plot.title = element_text(hjust = 0.5)) 

