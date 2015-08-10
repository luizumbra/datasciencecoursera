best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
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
  
  # Eliminate not defined states and verify a valid state
  eliminateBool <- hospitalState == state
  
  # Verify invalid state
  if (sum(as.integer(eliminateBool)) == 0) {
    stop("invalid state")
  }
  
  # Continue to eliminate not defined states
  hospitalName <- hospitalName[eliminateBool]
  hospitalState <- hospitalState[eliminateBool]
  hospitalMortality <- hospitalMortality[eliminateBool]
  
  # Eliminate the "Not Available"
  eliminateBool <- hospitalMortality == "Not Available"
  hospitalName <- hospitalName[!eliminateBool]
  hospitalState <- hospitalState[!eliminateBool]
  hospitalMortality <- hospitalMortality[!eliminateBool]
  
  # Find the less hospitalMortality and put in a indice
  indiceOfLessMortality <- 1
  nameLessMortalityHospital <- as.character(hospitalName[indiceOfLessMortality])
  for (i in seq_along(hospitalMortality)) {
    if (as.numeric(as.character(hospitalMortality[i])) < as.numeric(as.character(hospitalMortality[indiceOfLessMortality]))) {
      indiceOfLessMortality <- i
      nameLessMortalityHospital <- as.character(hospitalName[indiceOfLessMortality])
    }
    else if (as.numeric(as.character(hospitalMortality[i])) == as.numeric(as.character(hospitalMortality[indiceOfLessMortality]))) {
      nameLessMortalityHospital <- sort(c(nameLessMortalityHospital, as.character(hospitalName[i])))[1]
    }
  }
  
  nameLessMortalityHospital
}