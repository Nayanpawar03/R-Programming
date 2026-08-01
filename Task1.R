file_path <- file.choose()

tryCatch({
  
  air_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  cat("Dataset Loaded Successfully!\n\n")
  
  cat("First 6 Records:\n")
  print(head(air_data))
  
  cat("\nStructure of Dataset:\n")
  str(air_data)
  
  cat("\nRows:", nrow(air_data), "\n")
  cat("Columns:", ncol(air_data), "\n")
  
  if(any(is.na(air_data))){
    cat("\nMissing values are present.\n")
  } else {
    cat("\nNo missing values.\n")
  }
  
  cat("Total Missing Values:", sum(is.na(air_data)), "\n")
  
}, error = function(e){
  
  cat("Error:", e$message)
  
})
