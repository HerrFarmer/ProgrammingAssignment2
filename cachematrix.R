## The following two functions demonstrate how the scoping rules in R can be used to 
## preserve state within an R object, in this case within the function makeCacheMatrix().

## Provide functions to set and get a given matrix as well as to retrieve its inverse. 
## If a new matrix is set, the inverse is reset.
makeCacheMatrix <- function(x = matrix()) {
  
  # Object to cache the inverted matrix
  inverse <- NULL
  
  # Set new matrix and reset cached value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get matrix
  get <- function() x
  
  # Set new inverse
  setinverse <- function(inv) inverse <<- inv
  
  # Get inverse
  getinverse <- function() inverse
  
  # List of functions provided
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Computes the inverse of a matrix. The inverse is looked-up from cache first. If it 
# cannot be found, a new inverse is calculated using the solve() function.
cacheSolve <- function(x, ...) {
  
  # Get inverse from cache
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    ## Retrieve from cache
    message("Getting cached data")
    return(inverse)
  }
  
  # Get matrix and compute inverse
  data <- x$get
  inverse <- solve(data)
  x$setinverse(inverse)
  
  # Return inverse
  inverse
}