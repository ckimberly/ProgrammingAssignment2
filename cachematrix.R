## A pair of functions that cache the inverse a matrix.
## First function is to create a matrix that can cache its inverse.
## Second function is to retrieve the inverse matrix.

## This function creates a special "matrix" that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## create a vector for inverse matrix
  
  set <- function(y) {           
    x <<- y   
    m <<- NULL 
  }
  ## set value of the matrix
  
  get <- function() x    
  ## get the value of the matrix and return the matrix
  
  setinv <- function(solve) m <<- solve
  ## set the value of the inverse matrix
  
  getinv <- function() m
  ## return the value of the inverse matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  ##return the created vector of functions
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

## The function returns a inverse matrix

cacheSolve <- function(x, ...) {
        ##return a matrix that is the inverse of 'x'
  m <- x$getinv()        
  ## get the inverse matrix out of cache
  
  if(!is.null(m)) {                       
    message("getting cached data")
    return(m)                       
  }
  ## return to the inverse matrix if it is set
  
  data <- x$get() 
  ## get the matrix
  
  m <- solve(data, ...)
  ## stored in cache
  
  x$setinv(m)                             
  ## set the inverse matrix
  
  m                                       
  ## Return the matrix       
}
