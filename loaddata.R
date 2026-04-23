
# Load data from the CSV files
load_species_data <- function(path){
  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE      
  )
  
  # Make sure that Age and Genotypeil22 is renamed to GT and DAY, 
  # to correctly make the plots. 
  df <- df %>%
    dplyr::rename(any_of(c(Day = "Age", GT = "GenotypeIL22")))
  
  # Make sure day is an integer value 
  df$Day <- as.integer(df$Day)
  
  return(df)
}

# Load data from the bray curtis file. 
load_sep_file <- function(path){
  
  df <- read.delim(path, sep = '\t', stringsAsFactors = FALSE,
                   check.names = FALSE)
  
  
  return(df)
  

}

load_compare_file <- function(path1, path2) {
  
  # Load both files using the existing function so renaming/typing is applied
  df1 <- load_species_data(path1)
  df2 <- load_species_data(path2)
  
  # Add a source column so you can distinguish them in the PCA later
  df1$source <- "Dataset 1"
  df2$source <- "Dataset 2"
  
  # Bind only the columns that exist in both datasets
  shared_cols <- intersect(names(df1), names(df2))
  combined <- dplyr::bind_rows(df1[, shared_cols], df2[, shared_cols])
  
  View(combined)
  
  return(combined)
}