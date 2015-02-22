## These functions calculate an input matrix inverse and
## save the inverse to cache.  If the same inverse is 
## required on subsequent calls of the function, the solution
## is pulled from cache rather than recalculated.

## makeCacheMatrix builds the options for cacheSolve to choose 
## whether an inverse is calculated or pulled from cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve uses the results of makeCacheMatrix to
## either pull from cache or calculate the inverse of
## the input matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
