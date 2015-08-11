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
  
  # Define a list of satates
  state <- sort(unique(as.character(hospitalState)))
  
  # Eliminate the "Not Available"
  eliminateBool <- hospitalMortality == "Not Available"
  hospitalName <- hospitalName[!eliminateBool]
  hospitalState <- hospitalState[!eliminateBool]
  hospitalMortality <- hospitalMortality[!eliminateBool]
  
  # Develop a rank for each state
  hospital <- sapply(state, function(x) {
    rankhospital(hospitalName = hospitalName, hospitalState = hospitalState, hospitalMortality = hospitalMortality, state = x, num = num)
  })
  
  unname(hospital)
  hospital <- cbind(hospital, state)
}

rankhospital <- function(hospitalName, hospitalState, hospitalMortality, state, num = "best") {
  # Eliminate not defined states and verify a valid state
  eliminateBool <- hospitalState == state
  
  # Verify inexistant state
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
  
  # Order hospitalMortality rank with bouble-sort
  for (i in seq_along(hospitalMortality)) {
    for (j in i:length(hospitalMortality)) {
      if (as.numeric(as.character(hospitalMortality[i])) > as.numeric(as.character(hospitalMortality[j]))) {
        
        # Swap hospitalName
        aux <- hospitalName[i]
        hospitalName[i] <- hospitalName[j]
        hospitalName[j] <- aux
        
        # Swap hospitalMortality
        aux <- hospitalMortality[i]
        hospitalMortality[i] <- hospitalMortality[j]
        hospitalMortality[j] <- aux[1]
      }
      
      # Tie case, sort in alphabetical order
      else if (as.numeric(as.character(hospitalMortality[i])) == as.numeric(as.character(hospitalMortality[j]))) {
        if (as.character(hospitalName[i]) > as.character(hospitalName[j])) {
          # Swap hospitalName
          aux <- hospitalName[i]
          hospitalName[i] <- hospitalName[j]
          hospitalName[j] <- aux
          
          # Swap hospitalMortality
          aux <- hospitalMortality[i]
          hospitalMortality[i] <- hospitalMortality[j]
          hospitalMortality[j] <- aux[1]
        }
      }
    }
  }
  
  # Define the real value of num
  num <- if (num == "best") {
    1
  }
  else if (num == "worst") {
    length(hospitalName)
  }
  else {
    num
  }
  
  # Return rank
  as.character(hospitalName[num])
}