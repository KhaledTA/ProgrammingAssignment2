## This function creates a matrix object and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mtxinverse <- NULL
  
  ## getter and Setter functions
  ## set is used to set the matrix values
  ## get is used to retrive the matrix
  set <- function(m) { mtx <<- m; mtxinverse <<- NULL; }
  get <- function() return(mtx);
  
  ## setinv is used to store the matrix inverse
  ## etinv is used to retrive the matrix inverse
  setInverse <- function(inv) mtxinverse <<- inv;
  getInverse <- function() return(mtxinverse);
  
  ## returns the list of methods 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## This function computes the inverse of a given matrix returned by the "makeCacheMatrix" function. 
## - If the inverse has already been calculated (and the matrix has not changed), 
##   then the "cachesolve" should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## initialize the inverse of 'x' matrix
  mtxinv <- x$getInverse()
  
  ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
  if(!is.null(mtxinv)) {
    message("getting cached data")
    return(mtxinv)
  }
  ## If the inverse is not cached 
  
  ## getting the matrix
  mtx <- x$get()
  
  ## calculating the inverse by using matrix multiplication
  inv <- solve(mtx, ...)
  
  ## storing the inverse
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv 
  
}
