Microbiota analysis

This Shiny application provides an interactive environment for exploring microbiota datasets. The app allows users to visualize microbial composition, perform ordination analyses and assess beta diversity through a user friendly interface.

The application was developed to facilitate microbiota data exploration and comparison across different experimental groups and time points.

Features
Composition Plots
Upload one or two abundance datasets.
Visualize microbial composition interactively.
Compare two datasets side by side.
Option to calculate and display average abundances.
Principal Component Analysis (PCA)
Interactive PCA visualization.
Filter samples by genotype.
Filter samples by selected time points.
Redundancy Analysis (RDA)
Explore relationships between microbiota composition and explanatory variables.
Select different explanatory variables (Day, Genotype, Day × Genotype, etc.).
Filter by genotype and time point.
Beta Diversity Analysis
Interactive Bray-Curtis distance heatmap.
Beta diversity boxplots.
Compare within-group and between-group variation over time.
Project Structure
├── app.R
├── loaddata.R
├── CompositionPlot.R
├── PCAplot.R
├── RDAplot.R
├── HeatmapBoxplotBeta.R
└── README.md
File Description
File	Description
app.R	Main Shiny application
loaddata.R	Functions for loading and preprocessing input data
CompositionPlot.R	Functions for microbiota composition plots
PCAplot.R	Functions for PCA visualization
RDAplot.R	Functions for RDA analysis and plotting
HeatmapBoxplotBeta.R	Functions for beta diversity heatmaps and boxplots
Required Packages

The following R packages are required:

library(shiny)
library(ggiraph)
library(bslib)
library(plotly)
library(shinyjs)
library(shinyBS)

Install them using:

install.packages(c(
  "shiny",
  "ggiraph",
  "bslib",
  "plotly",
  "shinyjs",
  "shinyBS"
))
Running the App

Clone the repository:

git clone https://github.com/yourusername/microbiota-analysis.git

Open R or RStudio and run:

shiny::runApp()

or

source("app.R")
Input Data
