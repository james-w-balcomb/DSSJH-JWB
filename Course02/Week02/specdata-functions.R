
#### Part 1 ####
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  id_length <- length(id)
  means_vector <- numeric(length(id))
  observations_vector <- integer(length(id))
  id_minimum <- min(id)
  
  for (i in id) {
    
    i_name <- i
    i_name <- paste("000",i_name, sep = "")
    i_name <- substr(i_name,(nchar(i_name) - 2), nchar(i_name))
    
    data_file <- paste(directory,"\\",i_name,".csv", sep = "")
    
    data_set_tmp <- read.table(data_file, header = TRUE, colClasses = c(Date = "Date", sulfate = "numeric", nitrate = "numeric", ID = "integer"), sep = ",", na.strings = "NA", comment.char = "", stringsAsFactors = FALSE)
    
    means_vector[(i - id_minimum + 1)] <- mean(data_set_tmp[[pollutant]], na.rm = TRUE)
    observations_vector[(i - id_minimum + 1)] <- sum(!is.na(data_set_tmp[[pollutant]]))
    
  }
  
  pollutant_mean <- sum((means_vector * observations_vector), na.rm = TRUE) / sum(observations_vector, na.rm = TRUE)
  
  return(pollutant_mean)
  
}



#### Part 2 ####
complete <- function(directory, id = 1:332) {
  
  id_vector <- integer()
  nobs_vector <- integer()
  
  for (i in id) {
    
    id_vector <- append(
      id_vector,
      i
    )
    
    i_name <- paste("000",i, sep = "")
    i_name <- substr(i_name,(nchar(i_name) - 2), nchar(i_name))
    
    data_file <- paste(directory,"\\",i_name,".csv", sep = "")
    
    data_set <- read.table(data_file, header = TRUE, colClasses = c(Date = "Date", sulfate = "numeric", nitrate = "numeric", ID = "integer"), sep = ",", na.strings = "NA", comment.char = "", stringsAsFactors = FALSE)
    
    complete.cases(data_set)
    complete_logical <- complete.cases(data_set)
    
    nobs_vector <- append(
      nobs_vector,
      nrow(data_set[complete_logical,])
    )
    
  }
  
  pollutant_complete <- data.frame(id_vector, nobs_vector)
  colnames(pollutant_complete) <- c("id", "nobs")
  
  return(pollutant_complete)
  
}



#### Part 3 ####
corr <- function(directory, threshold = 0) {
  
  data_files_list <- list.files(path = directory, pattern = "*.csv", full.names = T, recursive = FALSE)
  data_files_list
  
  corr_vector <- vector(mode = "numeric", length = 0)
  
  for (data_file in data_files_list) {
    
    data_set <- read.table(data_file, header = TRUE, colClasses = c(Date = "Date", sulfate = "numeric", nitrate = "numeric", ID = "integer"), sep = ",", na.strings = "NA", comment.char = "", stringsAsFactors = FALSE)
    
    complete_logical <- complete.cases(data_set)
    nrow(data_set[complete_logical,])
    nobs <- nrow(data_set[complete_logical,])
    
    if (nobs > threshold) {
      corr_vector <- append(
        corr_vector,
        cor(data_set[["nitrate"]], data_set[["sulfate"]], use = "complete.obs")
      )
    }
    
  }
  
  return(corr_vector)
  
}
