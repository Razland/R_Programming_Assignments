## cachematrix.R Assigment 2 of R programming 
## Dave Kenny

## Accepts a(n assumed) square matrix and creates a new data structure
## (a list) that uses an embedded function to store the original matrix 
## and the functions for that will return the input matrix. Structure 
## will also store the inverse matrix. Include in the data structure the
## functions for returning and storing the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  ## null inverse matrix
  inverseMatrix <- NULL
  ## make a copy of matric input x and a place for the inverse.
  mCache <- function(dummyMatrix){
    ## read in the input
    x <<- dummyMatrix
    ## null the inverse matrix when new matrix is cached
    inverseMatrix <- NULL
  }
  ## read out the original matrix from cache
  mReturn <- function() x
  ## solves the inverse and stores the result
  invCache <- function(solve) inverseMatrix <<- solve
  ## prints the cached invers
  invReturn <- function() inverseMatrix
  list(mCache = mCache,
       mReturn = mReturn,
       invCache = invCache,
       invReturn = invReturn)
}


## Implement embedded functions that compute the inverse of the input
## matrix.

cacheSolve <- function(x, ...) {  ## expects a list of matrices and functions
        ## Return a matrix that is the inverse of 'x'
  invM <- x$invReturn() ## attempts to find if inverse already stored
  if(!is.null(invM)){ # if inverse is already solved, message and return it
    message("getting cached inverse")  ## user message
    return(invM)  ## return cached inverse, exits function
  }
  dataM <- x$mReturn() ## load up cached input matrix in x
  invM <- solve(dataM, ...) ## solves for inverse of cached x
  x$invCache(invM) ## caches inverse in x
  message("setting inverse cache")
  return(invM) ## output inverse when initially set
}
