# function: load files to use for the shiny app

load_species_data <- function(path) {
  # check if file is csv
  
  if (tolower(tools::file_ext(path)) != "csv") {
    stop("Expected a CSV file, got: ", tools::file_ext(path), " please provide a .csv file")
  }
  
  
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to read CSV: ", e$message)
  )
  
  df <- df %>%
    dplyr::rename(any_of(c(Day = "Age", GT = "GenotypeIL22")))
  
  # warn if expected columns are still missing after rename
  required_cols <- c("Day", "GT", "sample", "classid", "percentage")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Loaded file is missing expected columns: ", 
            paste(missing_cols, collapse = ", "))
  }
  
  df$Day <- suppressWarnings(as.integer(df$Day))
  if (all(is.na(df$Day))) warning("Day column could not be converted
                                  to integer")
  
  return(df)
}

load_sep_file <- function(path) {
  if (tolower(tools::file_ext(path)) != "txt") {
    stop("Expected a CSV file, got:", tools::file_ext(path), " please provide a .txt file")
  }
  
  
  df <- tryCatch(
    read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to read file: ", e$message)
  )
  
  if (nrow(df) == 0) warning("File loaded but contains no rows: ", path)
  
  return(df)
}