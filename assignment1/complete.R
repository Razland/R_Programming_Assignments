#Coursera R Programming
#Programming assignment 1, part 2, Week 2
#Dave Kenny 
#spec: include function to test complete function so that it runs
#     on loading  
#constraints: 
#notes:

complete <- function(directory, id = 1:332) {
  #Cut-and-paste from instructions:
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
# get list of files into a vector
  files <- dir(directory)
# init an output vector  
  nObsOut <- NULL # R seems to be pretty good at cleanup, but initialize anyway for cat
  for(i in id)
  {  
    #iterate on filenames by number i.  So much easier than padding 0 like first assignment
    filename <- paste0(directory,"/",files[i])
    # read file for eval
    compframe <- read.csv(filename)
    # this is probably more elaborate than it needs to be: if reading file above, omit.na()
    # would only leave complete observations, which would then be simple to count as a positive. 
    # However inadvertent NA values in sensor id or date columns would also negate the value, 
    # and eliminate complete observations from consideration.  This way, only pollutant NA values
    # are counted out.  Compares the number of NA for both pollutants, then returns the smaller
    # of the two vector lengths as the number of complete observations.  Indexes out the NA values.
## this code didn't work in submit, mostly for the reasons above:
##                goodsulf <- (compframe["sulfate"])[!(is.na(compframe["sulfate"]))]
##                goodnite <- (compframe["nitrate"])[!(is.na(compframe["nitrate"]))]
##                if(length(goodsulf)<=length(goodnite)) nObsOut <-c(nObsOut, length(goodsulf)) else nObsOut <- c(nObsOut, length(goodnite))
## Turns out the submit() script points to different test data than the class assignment, and there is at least
## one file where this bad algorithm is detected and marked off.  Ouch.
   ### Need to try to remove/not count NA in either pollutant column, rather than assuming that NA values do not
   ### coincide.  Using OR and different subset notation, count pollutant rows that do not have NA values seems to
   ### work okay.  At least gets the correct test result for the one case in the submit() script.
   nObsOut <- c(nObsOut, sum(as.numeric(!(is.na(compframe$sulfate)| is.na(compframe$nitrate)))))
  }
  # data frame output built from id vector and observation count
  
  output <-data.frame(id = id, nobs = nObsOut)
  return(output)
}

test_complete <-function()  ## performs tests on complete() function per assignment spec
{ 
  print("test 1 :")
  print(format(complete("specdata", 1)))
  print("test 2 :")
  print(format(complete("specdata", c(2, 4, 8, 10, 12))))
  print("test 3 :")
  print(format(complete("specdata", 30:25)))
  print("test 4 :")
  print(format(complete("specdata", 3)))
}

# test_complete() ## uncomment to execute test_complete on source()
