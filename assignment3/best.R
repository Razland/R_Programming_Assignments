## file best.R.  Dave Kenny. Programming Assignment 3 part 1. Coursera R 
##               Programming course.  initial commit 11 Sep 14

## ** lines copied from assignment spec:
best <- function(state, outcome){    
  colsList <- makeColumnsList() ## get a hard-coded list of column numbers
  outcomeNames <- makeOutcomesList() ## get a hard-coded list of outcomes
  namesColsMat <- cbind(colsList, outcomeNames) ## set a lookup matrix for 
              ## column numbers and names
  state <- toupper(state) ## fix state argument to always evaluate as uppercase
  outcome <- toupper(outcome) ## fix outcome argument to evaluate regardless of 
              ## input text case

  ## ** Read outcome data
  outcomesDF <- readDataFile(colsList)## read in, mod data to useful format
  ## ** Check that state and outcome are valid
  if(!checkState(outcomesDF, as.character(state)))## stop on bad state argument 
    stop(c(state, " invalid state"))              ## with error message 
  if(!length(which(outcomeNames == outcome))) ## stop on bad outcome argument
    stop(c(outcome, " invalid outcome"))      ## with error message
  
  ## ** Return hospital name in that state with lowest 30-day death rate
  ## Klingon concatenated command sets the column of the data frame for death 
  ## rates from the lookup table created above, based on the outcome argument.
  rateCol <- as.integer(suppressWarnings(na.omit(namesColsMat[namesColsMat[,2] 
                                                              == outcome, 1])))
  ## Klingon concatenated command gets the minimum of the range for state and 
  ## outcome matching the input arguments.
  minOutcome <-min(as.numeric(outcomesDF[outcomesDF$State == state, rateCol]),
                              na.rm = TRUE)
  ## and another concatenated command to get the hospitals from the state that
  ## match the minimum
  hospList <- (outcomesDF[(outcomesDF$State == state)&
                         (outcomesDF[rateCol]==minOutcome), 2])
  ## find the NA in the list of hospitals
  naVect <- is.na(hospList)
  bestListLen <- length(hospList[!naVect])
  
  if (bestListLen== 1) {  ## found only one hospital that meets criteria
    outPut<-(hospList[!naVect])
  } 
  else { ## sort the hospital names and send back the first
     outPut <- sort(hospList[!naVect])[1]
  }
  ## return the first from the sorted list of hospitals without the NA values
  return(outPut)
}

checkState <- function(outcomeDF, stringVar = character()) 
{ ## function accepts a data frame in the form specified by Assignment 3
  ## and a character string for the state abbreviation.  Returns TRUE
  ## if the string does occur within the $State vector.
  isThere <- FALSE
  if ( length(which(unique(outcomeDF$State) == stringVar )) >= 1 ){ 
    isThere <- TRUE 
  } 
  return(isThere)
}

makeColumnsList <- function() { ## returns a list of columns for the input
                                ## table
  colsList <- list(HEART.ATTACK = 11, ## Columnsthat contain death rates in 
                   HEART.FAILURE = 17, ## the outcome-of-care-measures.csv
                   PNEUMONIA = 23) ## file.   Hard-code column numbers!
  return(colsList)
}  

makeOutcomesList <- function(){ ## Makes a list of outcomes in standard form
                                ## to the input file
  outcomeNames <- list(HEART.ATTACK = "HEART ATTACK", ## List holds values
                     HEART.FAILURE ="HEART FAILURE",  ## for lookup
                     PNEUMONIA = "PNEUMONIA")
  return(outcomeNames)
}

readDataFile <- function(colsList){ ## reads in data file and converts
                                    ## listed columns to numerical format
  outcomesDF <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character") ## Read in file per example
  for (colI in colsList){ ## convert death rate columns to numeric values, 
    ## suppressing NA coercion warnings
    suppressWarnings(outcomesDF[, colI] <- as.numeric(outcomesDF[, colI]))
  }
  return(outcomesDF)
}  

testIsState <- function(outcomeDF, statevar = character())
{ ## This function tests function checkState
  ## outcomeDF is a data frame read from input csv file 
  if (checkState(outcomeDF, statevar)) {
    return(paste0(statevar, " is a state in the list"))
  } 
  else {
    return(paste0(statevar, " is not a state in the list"))
  }
}

testBest <-function(){ ## test everthing from command line: ###
   colsList <- makeColumnsList() ## get a hard-coded list of column numbers
   outcomeNames <- makeOutcomesList() ## get a hard-coded list of outcomes
   namesColsMat <- cbind(colsList, outcomeNames) ## matrix created from lists
   stateList <- unique((read.csv("outcome-of-care-measures.csv", ## Gets list
                       colClasses = "character"))$State) ## of recognized state
                      ## abbrevations for validation                     
  for (ailment in namesColsMat[,2]){ ## outer loop through all outcome names
    for (i in 1:54 ){  ## inner loops through all the states
      print(paste0("if you've got ", ailment, " in ", stateList[i], 
            " go to ", best(stateList[i], ailment)))}} ## silly formatting
                       ## for output exercises the best() function
}
