## This pair of functions creates and caches the inverse of a provided 
## square invertable matrix for further use

makeCacheMatrix <- function(x = matrix()) {
  ## Define methods for getting and setting variables
  ## Presets the object to hold the matrix inverse to NULL
  inv <- NULL
  ## Defines the method for setting the matrix in the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Defines the method for retrieving the matrix from cache
  get <- function() x
  
  ##Defines the method for setting the inverse in the parent environment
  setinv <- function(inv) m <<- inv
  
  ##Defines the method for retrieving the inverse from cache
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##Returns a matrix that is inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Retrieves the inverse if it already exists
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Retrieves the initial matrix, solves to produce the inverse, and sets the inverse in cache
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}