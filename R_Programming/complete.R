complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Anonymous function
  nobsResult <- lapply(id, function(x) {
    
    # Develop the standard file names
    filename <- if(x < 10) {
      paste(directory, "/00", as.character(x), ".csv", sep = "")
    }
    else if(x < 100) {
      paste(directory, "/0", as.character(x), ".csv", sep = "")
    }
    else {
      paste(directory, "/", as.character(x), ".csv", sep = "")
    }
    
    # Read csv files
    fileread <- read.csv(filename, header = TRUE)
    
    ## Calculate the nobs
    nobsSulfate <- !is.na(fileread$sulfate)
    nobsNitrate <- !is.na(fileread$nitrate)
    nobsBool <- nobsSulfate & nobsNitrate
    
    ## Create the specified array with id, nobs and the respective names
    nobsVec <- c(as.integer(x), as.integer(sum(as.integer(nobsBool))))
    names(nobsVec) <- c("id", "nobs")
    nobsVec
  })
  
  nobsResult <- as.data.frame(nobsResult)
  #names(nobsResult) <- c("id", "nobs")
  nobsResult
}