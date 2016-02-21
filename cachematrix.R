## First Function makes a list that sets & get mantrix with its inverse within the environment
## Second Function takes the list from First Function & calculates the inverse.
##Function belows sets the matrix
makeCacheMatrix <- function(x = matrix()) { 
  Invcache <- NULL ## initializing inverse
  
  ## Set x in parent environment with the desired value, if inverse is already set, get rid of it!
  set <- function(userValue = matrix()) {
    x <<- userValue 
    Invcache <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in parent environment to desired value and return the value as required
  setInverse <- function(invValue) {
    Invcache <<- invValue 
    return(Invcache)
  }
  
  getInverse  <- function() Invcache
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x=makeCacheMatrix(1:4,nrow=2,ncol=2),...) { ##matrix created
  ## Return a matrix that is the inverse of 'x'
  ## Check if the inverse already exist
  calcInverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(calcInverse) && is.matrix(calcInverse)) { 
    message("We found cached data and saved valuable cpus!!!")
    return(calcInverse)
  }
  
  ## Else get the matrix
  matrixToSolve <- x$get()  
  
  ## Solving the matrix and catch errors and warnings
  calcInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:")
  x$setInverse(calcInverse)
}
