best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  # Define the outcome name
  outcome <- if (tolower(outcome) == "heart attack") {
    "Heart.Attack"
  }
  else if (tolower(outcome) == "heart failure") {
    "Heart.Failure"
  }
  else {
    "Pneumonia"
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
  
  # Eliminate not defined states
  eliminateBool <- hospitalState == state
  hospitalName <- hospitalName[eliminateBool]
  hospitalState <- hospitalState[eliminateBool]
  hospitalMortality <- hospitalMortality[eliminateBool]
  
  # Eliminate the "Not Available"
  eliminateBool <- hospitalMortality == "Not Available"
  hospitalName <- hospitalName[!eliminateBool]
  hospitalState <- hospitalState[!eliminateBool]
  hospitalMortality <- hospitalMortality[!eliminateBool]
  
  # Find the less hospitalMortality
  lessMortality <- 1
  lessName <- c(as.character(hospitalName[lessMortality]))
  for (i in seq_along(hospitalMortality)) {
    if (as.numeric(hospitalMortality[lessMortality]) > as.numeric(hospitalMortality[i])) {
      lessMortality <- i
      lessName <- as.character(hospitalName[lessMortality])
    }
    else if (as.numeric(hospitalMortality[lessMortality]) == as.numeric(hospitalMortality[i])) {
      lessName <- c(lessName, as.character(hospitalName[i]))
    }
  }
  
  sort(lessName)[1]
}