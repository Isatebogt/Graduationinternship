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

```R
library(shiny)
library(ggiraph)
library(bslib)
library(plotly)
library(shinyjs)
library(shinyBS)
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)
library(svglite)
library(dplyr)
library(ggh4x)
library(heatmaply)
library(RColorBrewer)
library(tibble)
library(reshape2)

## Running the App

Clone the repository:

```bash
git clone https://github.com/yourusername/microbiota-analysis.git
```

Open R or RStudio and run:

```R
shiny::runApp()
```

or

```R
source("app.R")
```

