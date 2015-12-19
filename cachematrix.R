## These functions allow for caching the inverse of the matrix 
## to prevent repeated calculation of the value

## makeCacheMatrix creates the special matrix variable 
## using matrix 'x' as the argument with ability to keep track
## whether its Inverse value has been already calculated
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function returns inverse of the special matrix x
## - from the cache if it has been previously calculated, or
## - freshly calculated, and stores this value in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
  
}
