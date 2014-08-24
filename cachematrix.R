## The two functions below, makeCacheMatrix and cacheSolve will cache 
## the inverse of a matrix if the inverse of that specific vector has not
## yet been calculated. If the inverse has been calculated and cached, it
## will return the cached result.

## MakeCacheMatrix will create a list of functions that will allow us
## to cache the inverse of a vector.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse
  setinv <- function(solve) m <<- solve
  
  #get the value of the inverse
  getinv <- function() m
  
  #return the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## CacheSolve will use the list created in he MakeCacheMatrix function
## to caluclate the inverse of a matrix, or if it has already been 
## calculated, returned the cached result.

cacheSolve <- function(x, ...) {
  
  #get the inverse of the matrix, if cached version exists; return
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #otherwise get value of matrix
  data <- x$get()
  
  #and cache value
  m <- solve(data, ...)
  x$setinv(m)

  #return inverse of matrix
  m

}
