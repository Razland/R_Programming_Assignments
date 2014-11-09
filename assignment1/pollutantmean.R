#Coursera R Programming
#Programming assignment 1, part 1, Week 2
#Dave Kenny 
#spec: read in files from {directory} to a new numeric vector,
# such that name of file is left-padded with zeroes (e.g. 
# "{dirname}/001.csv" for first id). Mean of all values of   
# variable type {pollutant} aggregated from all files in {id} vector.  
#constraints: files must be csv with naming convention as above,
# The {id} indicated by the file name. Files must have header and
# structure consistent with labels "Date","sulfate","nitrate","ID".
#notes: basic functions are:
# mean(polldat[!(is.na(polldat$sulfate)),"sulfate"])
# polldat <- read.csv("specdata/001.csv", header = T)
# omit.na" useful for removing NA values from vector.
# paste0("0", as.numeric(x[as.numeric(x)>=10 & as.numeric(x)<100]))

pollutantmean <- function(directory, pollutant, id = 1:332) {
  #Cut-and-paste from instructions:
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  datavect <- NULL
  newdata <- NULL
  for(i in id) 
    {
## call to lpad function sets filename to short character variable.  
## function below reads in each file & appropriate column into a temporary vector.    
## reads in data using indexing rules for pollutant.
    newdata <- ((read.csv(paste0(directory,"/",lpad(i,3),".csv")))[pollutant])[,pollutant]
## concatenates the current file data into a local vector
   datavect <- c(datavect, newdata)
    }
## ouput formatted mean, removing NA values from sample    
   return(format(mean(datavect, na.rm = TRUE), digits = 4))
}

lpad <- function (name, numspaces){
  ## name is an integer. numspaces is the text length 
  ## required.  e.g. numspaces==3, then name<10 is padded
  ## with {numspaces-1} zeroes and returned as a char "001." 
  ## returns character padded with zeroes to char length 
  ## {numspaces}. 
  name <- as.character(name)
  while(nchar(name)< numspaces)
    { 
    name <- paste0("0",name) 
    }
  return(name)
}

# test function
test_pollutantmean <-function()
{
  expected1 <- "4.064"
  expected2 <- "1.706"
  expected3 <- "1.281" 
  test1 <- pollutantmean("specdata", "sulfate", 1:10)
  print(c("expected answer: ", expected1, ", returned: ", test1))
  if ( expected1 == as.character(test1)) print("successful test1") else print("failed test1")
  test2 <- pollutantmean("specdata", "nitrate", 70:72)
  print(c("expected answer: 1.706, returned: ", test2))
  if ( expected2 == as.character(test2)) print("successful test2") else print("failed test2")
  test3 <- pollutantmean("specdata", "nitrate", 23)
  print(c("expected answer: 1.281, returned: ", test3))
  if ( expected3 == as.character(test3)) print("successful test2") else print("failed test2")
}

test_lpad <-function()
{ if(lpad(99,5) == "00099") print("lpad test successful") else print ("lpad test failed")}