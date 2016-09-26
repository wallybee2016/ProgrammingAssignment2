## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  CachedInv<- NULL   ## initializing the inverse
  ##if the inverse is set already, dispose of it and set x in 
  ##the parent environment
 
}


 set <- function(userValue = matrix()){
    x<<- userValue## set x in the parent environment
    CachedInv<<-NULL## if inverse is already set, dispose of it.
    
    x<<- userValue
    cachedInv<<-NULL
  }
  get <- function( )  x##get x
  
  get <- function( ) x
  ##set the inverse variable in the parent environment to your
  ##desired value and return the value.
  setInverse <- function(invVal){
    cachedInv<<-(invVal)## setting the inverse variable in
    ##parent environment to desired value
    return(cachedInv) ##return the value
    cachedInv<<-invVal
    return(cachedInv)
  }
  getInverse <- function() cachedInv## obtain inverse
  
  getInverse <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##given a matrix X, we will solve for the inverse and return it
##given the list variable from the first function, we will       
##check if there's a cached inverse and return
##else, we solve for the inverse and return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  ## Return a matrix that is the inverse of 'x'
  calculatedInverse <- x$getInverse() ## checking if there already is a result
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) {
    ##check if there's a cached value AND its a matrix
    calculatedInverse <- x$getInverse() 
    ##check if there's a cached value AND it's a matrix
    if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) {
      message("cached data found!!!")
      return(calculatedInverse)
    }
    matrixToSolve <- x$get()  ##alternatively, get the matrix
    calculatedInverse <- tryCatch({ ## try to solve the matrix and catch errors and warnings otherwise
      
      ## otherwise get the matrix
      matrixToSolve <- x$get()
      ##scouting for errors
      calculatedInverse <- tryCatch({
        solve(matrixToSolve)
      }, warning=function(w) {
        message("This may not be the desired result")
        ## set the value of the inverse (NULL if something went wrong)
        message(e)
        message(\n)
      })
      message("Setting provided matrix to:") ## set the value of the inverse (Null if something went wrong)
      message("Setting the value of inverse to:") 
      x$setInverse(calculatedInverse)
    }
