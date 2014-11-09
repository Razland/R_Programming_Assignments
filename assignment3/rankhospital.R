## file rankhospital.R.  Dave Kenny. Programming Assignment 3 part 2. Coursera 
##           R Programming course.  initial commit 13 Sep 14

## Programming Assignment 3.1 best.R contains useful functions for validating
## input state, loading data files, and setting up lookup.  Source best.R:
source("best.R")

## ** lines copied from assignment spec:
rankhospital <- function(state, outcome, num = "best") { ## 
  colsList <- makeColumnsList() ## get a hard-coded list of column numbers
  outcomeNames <- makeOutcomesList() ## get a hard-coded list of outcomes
  namesColsMat <- cbind(colsList, outcomeNames) ## set a lookup matrix for 
                                                ## column numbers and names
  state <- toupper(state) ## fix state argument to always evaluate as uppercase
  outcome <- toupper(outcome) ## fix outcome argument to evaluate regardless of 
                              ## input text case
  
  ## ** Read outcome data
  outcomesDF <- na.omit(readDataFile(colsList))## read in, mod data to useful format
  ## ** Check that state and outcome are valid
  if(!checkState(outcomesDF, as.character(state)))## stop on bad state argument 
    stop(c(state, " invalid state"))              ## with error message 
  if(!length(which(outcomeNames == outcome))) ## stop on bad outcome argument
    stop(c(outcome, " invalid outcome"))      ## with error message

  ## Klingon concatenated command sets the column of the data frame for death 
  ## rates from the lookup table created above, based on the outcome argument.
  rateCol <- as.integer(suppressWarnings(na.omit(namesColsMat[namesColsMat[,2] 
                                                              == outcome, 1])))
  
  ## create an ordered rank vector from the desired outcome(ailment) and the 
  ## hospital names (#2) columns for the rows that match the state value
  rankVector <- order(outcomesDF[outcomesDF$State == state, rateCol], 
                      outcomesDF[outcomesDF$State == state, 2]) ## highest == first
  ## create a new data frame using only the columns we care about
  resultsDataFrame <-data.frame(name = outcomesDF[outcomesDF$State ==  state, 2], 
                                rates = outcomesDF[outcomesDF$State == state, 17],
                                stringsAsFactors=FALSE)

  numRows <- nrow(resultsDataFrame)## save the number of rows
   
  resultsDataFrame <- resultsDataFrame[rankVector,] ## change data frame to be 
                                                    ## in order of rank
  resultsDataFrame$rank <- 1:numRows
  ## ** 30-day death rate
  if (tolower(num)=="best") { 
    outName <- as.character(resultsDataFrame[resultsDataFrame$rank==1,1L])
    return(outName)
  } 
  else if (tolower(num)=="worst"){  
    outName <- as.character(resultsDataFrame[resultsDataFrame$rank==as.integer(numRows), 1L])
    return(outName)
  }  
  else if ( as.numeric(num)>=numRows){ 
    return(NA) 
  }
  else {
    outName <- as.character(resultsDataFrame[resultsDataFrame$rank==as.integer(num), 1L ])
    return(outName)
  }  
}

##  Test functions
testRankHosp <- function(){
  print("Testing for invalid state:")
  try(rankhospital("zz", "pneumonia", 3)) ## invalid state gen error
  print("Testing for invalid outcome: ")
  try(rankhospital("iN", "hydrangia", 3)) ## invalid ailment gen error
  print("Testing for texas, heart failure, 4")
  print(rankhospital("Tx", "heart failure", 4)) ## normal operation
  print("Testing for Tennessee, Pneumonia")
  print(rankhospital("tN", "pneuMonia")) ## normal operation var best undefined
  print("Testing for Utah, Heart Attack, best")
  print(rankhospital("ut", "Heart attack", "best")) ## normal operation var num as "best"
  print("Testing for Minnesota, Heart Attack, 50000 (out-of-range)")
  print(rankhospital("MN", "heart attack", 5000)) ## normal operation var num as 100
  print("Maryland, Heart Attack, worst")
  print(rankhospital("mD", "Heart aTTACK", "worst")) ## normal operation var num as "worst"
}
