corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # Calculation of nobs of all files
  nobsVec <- complete(directory = directory)
  
  # File with nobs > 'threshold'
  fileComplete <- c()
  for (i in nobsVec$id) {
    if (nobsVec$nobs[i] > threshold) {
      fileComplete <- c(fileComplete, i)
    }
  }
  
  # Calculate the correlation
  result <- c()
  for (i in fileComplete) {
    
    # Readers file names
    fileName <- if (i < 10) {
      paste(directory, "/00", as.character(i), ".csv", sep = "")
    }
    else if (i < 100) {
      paste(directory, "/0", as.character(i), ".csv", sep = "")
    }
    else {
      paste(directory, "/", as.character(i), ".csv", sep = "")
    }
    
    # Read csv file
    fileRead <- read.csv(file = fileName, header = TRUE)
    
    # Calculate de correlation with cor function and put in 'result'
    result <- c(result, cor(fileRead$sulfate, fileRead$nitrate, use = "complete.obs"))
  }
  
  result
}