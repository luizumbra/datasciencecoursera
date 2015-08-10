rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  # Define the outcome name and verify a invalid outcome
  outcome <- if (tolower(outcome) == "heart attack") {
    "Heart.Attack"
  }
  else if (tolower(outcome) == "heart failure") {
    "Heart.Failure"
  }
  else if (tolower(outcome) == "pneumonia") {
    "Pneumonia"
  }
  else {
    stop("invalid outcome")
  }
  
  # Read csv file
  fileToOutcome <- read.csv(
    file = "./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",
    header = TRUE)
  
  # Separete in subdataset
  hospitalName <- fileToOutcome[["Hospital.Name"]]
  hospitalState <- fileToOutcome[["State"]]
  hospitalMortality <- fileToOutcome[[
    paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")]]
  
  # Eliminate the "Not Available"
  eliminateBool <- hospitalMortality == "Not Available"
  hospitalName <- hospitalName[!eliminateBool]
  hospitalState <- hospitalState[!eliminateBool]
  hospitalMortality <- hospitalMortality[!eliminateBool]
  
  
}