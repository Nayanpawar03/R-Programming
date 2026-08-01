cat("\n---------- Task 1 ----------\n")

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

cat("\n---------- Task 2 ----------\n")

temperature <- c(28, 30, NA, 32)
missing_object <- NULL
undefined_value <- 0/0

cat("NA Example:\n")
print(temperature)
print(is.na(temperature))

cat("\nNULL Example:\n")
print(missing_object)
print(is.null(missing_object))

cat("\nNaN Example:\n")
print(undefined_value)
print(is.nan(undefined_value))

cat("\n---------- Task 3 ----------\n")

missing_summary <- function(data){
  
  variables <- c("PM2.5","PM10","SO2","NO2","TEMP","WSPM","wd")
  
  summary_table <- data.frame(
    Variable=character(),
    Total_Records=integer(),
    Missing_Values=integer(),
    Missing_Percentage=double(),
    stringsAsFactors=FALSE
  )
  
  for(var in variables){
    
    total <- nrow(data)
    if(!(var %in% names(data))){
      next
    }
    
    missing <- sum(is.na(data[[var]]))
    percentage <- (missing/total)*100
    
    summary_table <- rbind(summary_table,
                           data.frame(
                             Variable=var,
                             Total_Records=total,
                             Missing_Values=missing,
                             Missing_Percentage=round(percentage,2)
                           ))
    
    if(percentage > 20){
      warning(paste(var,"contains more than 20% missing values"))
    }
    
  }
  
  return(summary_table)
  
}

summary_table <- missing_summary(air_data)

print(summary_table)

cat("\n---------- Task 4 ----------\n")

air_data$pollution_ratio <- air_data$PM2.5 / air_data$PM10

cat("NA Values :", sum(is.na(air_data$pollution_ratio)), "\n")
cat("NaN Values :", sum(is.nan(air_data$pollution_ratio)), "\n")
cat("Infinite Values :", sum(is.infinite(air_data$pollution_ratio)), "\n")

air_data$pollution_ratio[is.nan(air_data$pollution_ratio)] <- NA
air_data$pollution_ratio[is.infinite(air_data$pollution_ratio)] <- NA

cat("\n---------- Task 5 ----------\n")

numeric_variables <- c("PM2.5","PM10","SO2","NO2","TEMP","WSPM")

missing_before <- c()

for(var in numeric_variables){
  
  if(var %in% names(air_data)){
    
    before <- sum(is.na(air_data[[var]]))
    
    median_value <- median(air_data[[var]], na.rm = TRUE)
    
    air_data[[var]][is.na(air_data[[var]])] <- median_value
    
    after <- sum(is.na(air_data[[var]]))
    
    missing_before[var] <- before
    
    cat("\nVariable :", var,"\n")
    cat("Missing Before :", before,"\n")
    cat("Median Used :", median_value,"\n")
    cat("Missing After :", after,"\n")
    
  }
  
}

cat("\n---------- Task 6 ----------\n")

calculate_mode <- function(x){
  
  x <- x[!is.na(x)]
  
  unique_values <- unique(x)
  
  unique_values[which.max(tabulate(match(x, unique_values)))]
  
}

before_wd <- sum(is.na(air_data$wd))

mode_wd <- calculate_mode(air_data$wd)

air_data$wd[is.na(air_data$wd)] <- mode_wd

after_wd <- sum(is.na(air_data$wd))

cat("Mode :", mode_wd,"\n")
cat("Missing Before :", before_wd,"\n")
cat("Missing After :", after_wd,"\n")

cat("\n---------- Task 7 ----------\n")

clean_variable <- function(data, var_name){
  
  tryCatch({
    
    if(!(var_name %in% names(data))){
      stop("Variable does not exist.")
    }
    
    if(!is.numeric(data[[var_name]])){
      stop("Variable is not numerical.")
    }
    
    if(all(is.na(data[[var_name]]))){
      stop("Variable contains only missing values.")
    }
    
    med <- median(data[[var_name]], na.rm = TRUE)
    
    if(is.na(med)){
      stop("Median cannot be calculated.")
    }
    
    data[[var_name]][is.na(data[[var_name]])] <- med
    
    cat(var_name, "cleaned successfully.\n")
    
    return(data[[var_name]])
    
  }, error = function(e){
    
    cat("Error:", e$message, "\n")
    
    return(NULL)
    
  })
  
}

for(var in numeric_variables){
  air_data[[var]] <- clean_variable(air_data, var)
}

cat("\n---------- Task 8 ----------\n")

variables <- c("PM2.5","PM10","SO2","NO2","TEMP","WSPM","wd")

before_values <- c(
  missing_before["PM2.5"],
  missing_before["PM10"],
  missing_before["SO2"],
  missing_before["NO2"],
  missing_before["TEMP"],
  missing_before["WSPM"],
  before_wd
)

after_values <- c(
  sum(is.na(air_data$PM2.5)),
  sum(is.na(air_data$PM10)),
  sum(is.na(air_data$SO2)),
  sum(is.na(air_data$NO2)),
  sum(is.na(air_data$TEMP)),
  sum(is.na(air_data$WSPM)),
  after_wd
)

comparison_table <- data.frame(
  Variable = variables,
  Missing_Before = before_values,
  Missing_After = after_values,
  Values_Replaced = before_values - after_values
)

print(comparison_table)

if(all(comparison_table$Missing_After == 0)){
  cat("\nAll selected missing values have been handled successfully.\n")
}else{
  cat("\nSome missing values are still present.\n")
}



cat("\n---------- Task 9 ----------\n")

before_values <- comparison_table$Missing_Before
after_values <- comparison_table$Missing_After

barplot(
  rbind(before_values, after_values),
  beside = TRUE,
  names.arg = comparison_table$Variable,
  col = c("red","green"),
  main = "Missing Values Before and After Cleaning",
  xlab = "Variables",
  ylab = "Number of Missing Values"
)

legend(
  "topright",
  legend = c("Before Cleaning","After Cleaning"),
  fill = c("red","green")
)


cat("\n---------- Task 10 ----------\n")

write.csv(
  air_data,
  "cleaned_air_quality_data.csv",
  row.names = FALSE
)

cat("Cleaned dataset exported successfully.\n")