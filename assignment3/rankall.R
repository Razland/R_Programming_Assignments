## file rankall.R.  Dave Kenny. Programming Assignment 3 part 3. Coursera 
##           R Programming course.  initial commit 14 Sep 14

## Programming Assignment 3.1 best.R contains useful functions for validating
## input state, loading data files, and setting up lookup.  Source best.R:
source("best.R")

## ** lines copied from assignment spec:

rankall <- function(outcome, num = "best") {
  colsList <- makeColumnsList() ## get a hard-coded list of column numbers
  outcomeNames <- makeOutcomesList() ## get a hard-coded list of outcomes
  namesColsMat <- cbind(colsList, outcomeNames) ## set a lookup matrix for 
  ## column numbers and names
  outcome <- toupper(outcome) ## fix outcome argument to evaluate regardless of 
                              ## input text case
  ## ** Read outcome data
  ##outcomesDF <- na.omit(readDataFile(colsList))## read in, mod data to useful format
  outcomesDF <- readDataFile(colsList)## read in, mod data to useful format
  ## ** Check that state and outcome are valid
  if(!length(which(outcomeNames == outcome))) ## stop on bad outcome argument
    stop(c(outcome, " invalid outcome"))      ## with error message
  
  ## Klingon concatenated command sets the column of the data frame for death 
  ## rates from the lookup table created above, based on the outcome argument.
  rateCol <- as.integer(suppressWarnings(na.omit(namesColsMat[namesColsMat[,2] 
                                                              == outcome, 1])))
  ## ** For each state, find the hospital of the given rank
  stateVector <- sort(unique(outcomesDF$State))
  rankingsDataFrame <-data.frame(hospital = NULL, 
                                 ratings = NULL, 
                                 state = NULL, 
                                 ranking = NULL,
                                 stringsAsFactors=FALSE)
  stateDataFrame <- rankingsDataFrame
    
  if (num == "best") { ## sets rank to return for "best"
    num <- 1
  } else if (num == "worst"){ ## sets num from string "worst" to out-of-range,
      num <- "0.99999999"     ## illegal, detectable value that can be coerced
  }                           ## to numeric for fun and profit.  
  else { ## otherwise sets num to numeric value
    num <- as.numeric(num)
    if (is.na(num)){   ## if entered a non-numeric or not string "worst" or 
      stop(c(outcome, " invalid num for ranking"))} ## not string "best" stop 
  }                                                 ## with an error message  
  for(i in 1:length(stateVector)){  
    numReturns <- nrow(outcomesDF[(outcomesDF$State == stateVector[i] 
                                   & !is.na(outcomesDF[rateCol])), ])
    if (num == "worst"){ num <- numReturns}
    stateDataFrame <- getStatesData(outcomesDF, 
                                    rateCol, 
                                    stateVector[i],
                                    as.numeric(num))
    rankingsDataFrame <- rbind(rankingsDataFrame, stateDataFrame)
  }
return(rankingsDataFrame[,c(1,3)])
}

getStatesData <-function(outcomesDF, rateCol, state, num=1){ ##   
  rankVector <-  order(na.omit(outcomesDF[outcomesDF$State==state 
                               & !is.na(outcomesDF[rateCol]), rateCol]), 
                       na.omit(outcomesDF[outcomesDF$State==state
                               & !is.na(outcomesDF[rateCol]), 2]))
  newDF <- (outcomesDF[outcomesDF$State==state
                       & !is.na(outcomesDF[rateCol]), 
                       c(2, rateCol, 7)])[rankVector,]
  newDF$ranking <- 1:nrow(newDF)
  outDataFrame <- data.frame(hospital=newDF[,1], 
                             ratings=newDF[,2], 
                             state=newDF[,3],
                             ranking=newDF[,4]
                             )
  numRows <-nrow(outcomesDF[outcomesDF$State==state 
                            & !is.na(outcomesDF[rateCol]),])
  if(num == 0.99999999) 
    {num <- numRows} 
  else if(num > numRows ){ 
    outDataFrame <- data.frame(hospital = NA, 
                              ratings = NA, 
                              state = state, 
                              ranking = numRows )
    num <- numRows
  }
  return(outDataFrame[outDataFrame$ranking == num,])
}

### Test functions from instructions for assignment
testRankAll <-function() {  
  print("Testing for invalid outcome: ")
  try(rankall("hydrangia", 3)) ## invalid ailment gen error
  try(print(head(rankall("heart attack", 20), 10)))
  try(print(tail(rankall("pneumonia", "worst"), 3)))
  try(print(tail(rankall("heart failure"), 10)))
      return("done")
}