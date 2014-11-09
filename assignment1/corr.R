#Coursera R Programming
#Programming assignment 1, part 3, Week 2
#Dave Kenny 
#spec: 
#constraints: 
#notes: Hints in assignment evaluation suggest that this function
#  use the complete() function from the previous assignment. But
#  the method used for developing complete() reads in each specified
#  "*.csv" file then discards it.  This function must read in the data
#  again to correlate the two observation vectors, which is inefficient
#  in terms of IO if used together.  In this case, took the easier path
#  and read in data filtering out NA. More robust answer would be to
#  use complete() to pre-screen each file for actual !NA observations
#  before reading in to corr() ((thereby only reading bad files in once)), 
#  or rewrite complete() to accept a data frame as input, then return boolean, 
#  ((thereby reading all files in once)) rather than each function iterating 
#  through all file I/O on its own.

# options settings to output expected results per assignment specs
#options(digits = 5)
#options(warn = -1)


corr <- function(directory, threshold = 0) {
  #Cut-and-paste from instructions:
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
# initialize some variables for cat.  NA value numCorr[1] 
# will be eliminated on return() to calling function
  numCorr <- c(NA)
  df <- NULL
  fileslist <- dir(directory)
# iterate through all files
  for(i in 1:length(fileslist))
  {
    # eliminate NA values on read using na.omit. Potential issue
    # with NA values in Date or ID columns, but not an issue encountered
    # with test data provided.
    df <- na.omit(read.csv(paste0(directory,"/",fileslist[i]))) 
    if((dim(df)[1])>threshold) # Problem with >= threshold returning incorrect
    {                          # results, even thought assignment description
                               # seems to indicate that the desired behavior 
                               # to be threshold is the lowest acceptable data.
                               # Greater than threshold provides correct test 
                               # result.
      # Concatenates correlations
      numCorr<-na.omit(c(numCorr, cor(df[,"sulfate"],df[,"nitrate"])))
    }
  } 
  # eliminates NA values, and attempts to format result for output.
  ### This code didn't work.  Looks like formatting may have messed up some of the data through truncation
  ### or rounding errors.  The submit() script contains its own rounding, which would have produced the
  ### assignment examples without formatting in the return() from the function.
  ### return(na.omit(as.numeric(format(numCorr, digits = 3, nsmall = 4))))
  ### remove formatting for submission.  
  return(na.omit(as.numeric(numCorr)))
}

test_corr <- function() # exercises corr() function per assignment spec
{
  print("test 1 :")
  cr<- corr("specdata", 150)
  print("head(cr) :")
  print(head(round(cr, 5)))
  print("summary(cr) :")
  print(summary(round(cr,4)))
  print("test 2 :")
  cr<- corr("specdata", 400)
  print("head(cr) :")
  print(head(round(cr, 5)))
  print("summary(cr) :")
  print(summary(round(cr,4)))
  print("test 3 :")
  cr<- corr("specdata", 5000)
  print("summary(cr) :") ## results in NA/NAN values when called from 
                         ## this function. Results in blanks from command line
  print(summary(cr))
  print("length(cr) :")
  print(format(length(cr),na.encode=FALSE))
  print("test 4 :")
  cr<- corr("specdata")
  print("summary(cr) :")
  print(summary(round(cr ,4)))
  print("length(cr) :")
  print(length(cr))
}

