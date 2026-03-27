load_species_data <- function(path){
  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE      
  )
  

  df <- df %>%
    dplyr::rename(any_of(c(Day = "Age", GT = "GenotypeIL22")))
  

  df$Day <- as.integer(df$Day)
  
  return(df)
}

load_sep_file <- function(path){
  
  df <- read.delim(path, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
  
  
  return(df)
  

}