# Microbiota Analysis Shiny App

## Overview

This Shiny application provides an interactive environment for exploring microbiota datasets. The app allows users to visualize microbial composition, perform ordination analyses and assess beta diversity.

The application was developed to facilitate microbiota data exploration.

## Features

### Composition Plots

* Upload one or two abundance datasets.
* Visualize microbial composition interactively.
* Compare two datasets side by side.
* Option to display average abundances.

### Principal Component Analysis (PCA)

* Interactive PCA visualization.
* Filter samples by genotype.
* Filter samples by selected time points.

### Redundancy Analysis (RDA)

* Explore relationships between microbiota composition and explanatory variables.
* Select different explanatory variables (Day, Genotype, Day × Genotype, etc.).
* Filter by genotype and time point.

### Beta Diversity Analysis

* Interactive Bray-Curtis distance heatmap.
* Beta diversity boxplots.
* Compare within-group variation over time.

## Project Structure

```text
├── App
  ├── app.R
  ├── loaddata.R
  ├── CompositionPlot.R
  ├── PCAplot.R
  ├── RDAplot.R
  ├── HeatmapBoxplotBeta.R
  └── README.md
```

### File Description

| File                 | Description                                        |
| -------------------- | -------------------------------------------------- |
| app.R                | Main Shiny application                             |
| loaddata.R           | Functions for loading and preprocessing input data |
| CompositionPlot.R    | Functions for microbiota composition plots         |
| PCAplot.R            | Functions for PCA visualization                    |
| RDAplot.R            | Functions for RDA analysis and plotting            |
| HeatmapBoxplotBeta.R | Functions for beta diversity heatmaps and boxplots |

## Input file 

### Input file composition and ordination plots 

The input file used to generate the composition and ordination plots should contain the following columns:

Sample: sample identifier
classid: taxonomic classification (e.g., family, genus, or species)
percentage: relative abundance (%) of the taxon in the sample
Day: sampling time point
GT: genotype


| Sample | classid        | percentage | Day | GT |
| -------| ---------------| -----------|-----|----|
| sample1| Vibrio         | 0.54       | 21  | KO | 
| sample2| Streptococcus  | 0.60       | 7   | WT | 


This file can also be generated using the processfile.py script, which converts a relative abundance table exported from QIIME into the required format for each taxonomic level (e.g., phylum, class, order, family, genus, and species).

### Input file beta diversity heatmap and boxplot

The beta diversity heatmap requires a Bray-Curtis matrix file. This is a table containing pairwise Bray-Curtis values between all samples.

* The first column contains the sample names.
* The remaining columns contain Bray-Curtis values for each sample comparison.
* Samples should be listed as both rows and columns.

Example:

| Sample        | BC74.fastq.gz | BC75.fastq.gz | BC76.fastq.gz |
| ------------- | ------------- | ------------- | ------------- |
| BC74.fastq.gz | 0.000         | 0.139         | 0.245         |
| BC75.fastq.gz | 0.139         | 0.000         | 0.182         |
| BC76.fastq.gz | 0.245         | 0.182         | 0.000         |

This file can be generated using QIIME 2, R (`vegan`), or other microbiome analysis tools.

## Required Packages

The following R packages are required:

| Package      | Version | Purpose                   |
| ------------ | ------- | ------------------------- |
| shiny        | 1.13.0  | Shiny app                 |
| bslib        | 0.10.0  | Style Shiny app           |
| shinyBS      | 0.65.0  | Style Shiny app           |
| ggiraph      | 0.9.6   | Interactive visualisation |
| plotly       | 4.12.0  | Interactive visualisation |
| shinyjs      | 2.1.0   | Shiny app functionality   |
| dplyr        | 1.2.1   | Data processing           |
| tidyr        | 1.3.2   | Data processing           |
| tidyverse    | 2.0.0   | Data toolkit              |
| tibble       | 3.3.1   | Data processing           |
| stringr      | 1.6.0   | Data processing           |
| forcats      | 1.0.1   | Data processing           |
| lubridate    | 1.9.5   | Date handling             |
| vegan        | 2.7-3   | RDA/PCA                   |
| permute      | 0.9-10  | Statistical methods       |
| ggplot2      | 4.0.3   | Visualisation             |
| ggh4x        | 0.3.1   | Plotting                  |
| heatmaply    | 1.6.0   | Heatmaps                  |
| RColorBrewer | 1.1-3   | Color                     |

## Running the App

Open R or RStudio and run:

```R
shiny::runApp()
```

or

```R
source("app.R")
```

