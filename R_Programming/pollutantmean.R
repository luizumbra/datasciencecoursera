pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  # Create a standard data file.
  fileNames <- lapply(id, function(x) {
    fileName <- if (x < 10) {
      paste(directory, "/00", as.character(x), ".csv", sep = "")
    }
    else if (x < 100) {
      paste(directory, "/0", as.character(x), ".csv", sep = "")
    }
    else {
      paste(directory, "/", as.character(x), ".csv", sep = "")
    }
    fileName
  })
  
  # Read the csv files and concat them
  fileComplete <- do.call("rbind", lapply(fileNames, read.csv, header = TRUE))
  
  # Return the 'pollutant' mean
  mean(fileComplete[[pollutant]], na.rm = TRUE)
}